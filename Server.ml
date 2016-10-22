open Lwt.Infix
open SyncTypes

let name = Utils.gen_uuid ()

let ip, port =
  let ip = ref "127.0.0.1" in
  let port = ref 8080 in
  let specs = [ "ip", Arg.Set_string ip, "<ip> IP address" ;
		"port", Arg.Set_int port, "<port> Port number" ]
  in
  CommandLine.parse specs;
  !ip, !port

let create_root ~ip ~port =
  { root_name = "fake name";
    server = { ip;
	       port;
	       shutdown = (fun () -> failwith "fake_server.shutdown");
	       onshutdown = Lwt.fail_with "fake_server.onshutdown" };
    connections = Hashtbl.create 16 ;
    storage = (module Storage) }

let serve connection =
  let worker () =
    let rec f () = (Rpc.respond connection) >>= f in
    f ()
  in
  let run () =
    Lwt.catch
      (fun () -> Lwt.finalize worker connection.close)
      begin function
	| End_of_file (* ignore EOF exceptions *)
	| Unix.Unix_error(Unix.EBADF, _, _) -> Lwt.return_unit
	| exn -> Lwt.fail exn
      end
  in
  Utils.async "Serve connection" run

let start ?(name = name) ?(ip = ip) ?(port = port) () =
  Rpc.establish_server
    ip port
    { (create_root ~ip ~port) with root_name = name }
    serve
