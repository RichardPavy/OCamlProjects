open Lwt.Infix
open SyncTypes

module AsyncUtils = Sync_Utils_AsyncUtils

module DummyProtocol : Protocol.Sig = struct
  exception DummyProtocol of string
  include
    Protocol.Make
      (struct
	type 'a request = unit
	type 'a response = unit
	let prefix = '\000'
	let read_request _ = raise (DummyProtocol "read_request")
	let write_request _ = raise (DummyProtocol "write_request")
	let read_response _ = raise (DummyProtocol "read_response")
	let write_response _ = raise (DummyProtocol "write_response")
	let get_operation _ = raise (DummyProtocol "get_operation")
	let default_settings = Protocol.default_settings
      end)
      (struct
	type 'a value = unit
	type 'a serialized = unit
	let wrap_value _ = raise (DummyProtocol "wrap_value")
	let wrap_exn _ = raise (DummyProtocol "wrap_exn")
	let unwrap_value _ = raise (DummyProtocol "unwrap_value")
      end)
end

let protocols = Array.make 256 (module DummyProtocol : Protocol.Sig)
let register_protocol (module P : Protocol.Sig) =
  protocols.(int_of_char P.prefix) <- (module P)

let respond connection =
  Lwt_io.read_char connection.input
  >>= begin fun prefix ->
      let (module P : Protocol.Sig) = protocols.(int_of_char prefix) in
      P.respond connection
      end

let shutdown_fd wakener fd =
  let x =
    lazy begin
	Lwt.wakeup wakener ();
	Lwt.catch
	  begin fun () ->
	  Lwt.finalize
	    (fun () -> Lwt_unix.close fd)
	    (fun () -> Lwt.catch
			 (fun () -> Lwt.wrap2 Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL)
			 (fun exn -> Lwt.return_unit))
	  end
	  begin fun exn ->
	  Lwt_io.eprintlf "Exception when closing connection %s"
			  (Printexc.to_string exn)
	  end
      end
  and once = ref false in
  fun () -> if !once
	    then Lwt.return_unit
	    else (once := true; Lazy.force x)

let create_connection ?buffer_size root fd sockaddr =
  (try Lwt_unix.set_close_on_exec fd with Invalid_argument _ -> ());
  let onclose_waiter, onclose_wakener = Lwt.wait () in
  let close = shutdown_fd onclose_wakener fd in
  let create_buffer () =
    Lwt_bytes.create
      begin
	match buffer_size with
	| None -> 1024
	| Some x -> x
      end
  in
  { root;
    peer_name = "";
    sockaddr = sockaddr;
    lock = Lwt_mutex.create ();
    input = Lwt_io.of_fd ~buffer:(create_buffer()) ~mode:Lwt_io.input ~close fd;
    output = Lwt_io.of_fd ~buffer:(create_buffer()) ~mode:Lwt_io.output ~close fd;
    onclose = onclose_waiter;
    close = close;
  }

type accept_or_shutdown =
  | Accept of (Lwt_unix.file_descr * Lwt_unix.sockaddr)
  | Shutdown

let establish_server ?buffer_size ?(backlog = 5) ~ip ~port root callback =
  let sockaddr = Unix.ADDR_INET(Unix.inet_addr_of_string ip, port) in
  let sock = Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
  Lwt_unix.bind sock sockaddr;
  Lwt_unix.listen sock backlog;
  let abort_waiter, abort_wakener = Lwt.wait () in
  let onshutdown_waiter, onshutdown_wakener = Lwt.wait () in
  let root =
    let shutdown = lazy (Lwt.wakeup abort_wakener Shutdown) in
    { root with server = { ip;
			   port;
			   shutdown = (fun () -> Lazy.force shutdown);
			   onshutdown = onshutdown_waiter } }
  in
  let rec loop () =
    Lwt.pick [Lwt_unix.accept sock >|= (fun x -> Accept x); abort_waiter] >>= function
    | Accept(fd, sockaddr) ->
       callback (create_connection ?buffer_size root fd sockaddr);
       loop ()
    | Shutdown ->
       let close_server_socket () =
	 Lwt.apply Lwt_unix.close sock
	 >>= begin fun () ->
	     match sockaddr with
	     | Unix.ADDR_UNIX path when path <> "" && path.[0] <> '\x00' ->
		Lwt.catch (fun () -> Lwt.apply Lwt_unix.unlink path)
			  (fun _ -> Lwt.return_unit)
	     | _ -> Lwt.return_unit
	     end
       in
       AsyncUtils.async "Server %s:%i shutdown." ip port
		        close_server_socket;
       Lwt_io.eprintlf "Server %s:%i is down." ip port
       >|= fun () -> Lwt.wakeup onshutdown_wakener ()
  in
  AsyncUtils.async "server loop" loop;
  root

let open_connection ?buffer_size ~ip ~port root =
  let sockaddr = Unix.ADDR_INET(Unix.inet_addr_of_string ip, port) in
  let fd = Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  Lwt_unix.connect fd sockaddr
  >|= fun () -> create_connection ?buffer_size root fd sockaddr

let () =
  register_protocol (module JsonProtocol.Sync);
  register_protocol (module JsonProtocol.Async);
  register_protocol (module NativeProtocol.Sync);
  register_protocol (module NativeProtocol.Async);
  List.iter (fun m -> register_protocol m)
	    HttpProtocol.modules
