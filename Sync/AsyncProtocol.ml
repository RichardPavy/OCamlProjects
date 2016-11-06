open SyncTypes
open Lwt.Infix

type 'response resource = 'response Lwt.t * 'response Lwt.u
type 'response key = 'response resource Pool.key
type ('request, 'response, 'serialized_response) message =
  | Request of 'response key * 'request
  | Response of 'response key * 'serialized_response

module type AsyncWrapper =
  sig
    type 'a request
    type 'a response
    type 'a serialized
    type 'a msg = ('a request, 'a response, 'a serialized response) message
    val prefix : char
    val wrap_request : 'a response key -> 'a request -> 'a msg request
    val wrap_response : 'a response key -> 'a serialized response -> 'a msg request
    val unwrap_response : 'a msg response -> 'a msg
  end

module Make =
  functor (IO : Protocol.IO) ->
  functor (S : Protocol.Serializer with type 'a value = 'a IO.response) ->
  functor (W : AsyncWrapper with
	     type 'a request = 'a IO.request and
	     type 'a response = 'a IO.response and
	     type 'a serialized = 'a S.serialized) ->
  struct
    include Protocol.Make(IO)(S)
    module AsyncPool =
      Pool.Make(struct
		 type 'a t = 'a response Lwt.t * 'a response Lwt.u
		 let create = Lwt.task
	       end)

    let () = assert (W.prefix = IO.prefix)

    let call ?(settings=IO.default_settings) connection =
      Lwt.apply begin
	  fun (request : 'a request) ->
	  let open Pool in
	  let { key ; resource = waiter, _ } = AsyncPool.alloc () in
	  Lwt.on_cancel waiter (fun () -> AsyncPool.free key);
	  let async_request = W.wrap_request key request in
	  write_request connection.output async_request
	  >>= fun () -> Lwt.catch
			  begin fun () ->
			  Lwt_unix.with_timeout
			    settings.Protocol.timeout
			    (fun () -> (waiter : 'a response Lwt.t))
			  end
			  begin function
			    | Lwt_unix.Timeout ->
			       Lwt_io.eprintlf "Asynchronous request from %s to %s timed out."
					       connection.root.root_name connection.peer_name
			       >>= connection.close
			       >>= fun () -> Lwt.fail Protocol.Timeout
			    | exn -> Lwt.fail exn
			  end
	end

    let respond connection =
      Utils.async
	"AsyncProtocol.respond" begin
	  fun () ->
	  read_request connection.input

          >>= begin fun async_request ->
	      get_operation async_request connection
              end

          >>= begin fun async_response ->
	      match W.unwrap_response async_response with

	      | Request (key, request) ->
	         Lwt.catch
		   (fun () -> get_operation request connection >|= S.wrap_value)
		   (fun exn -> Lwt.return (S.wrap_exn exn))
		 >>= begin fun response ->
		     let async_response = W.wrap_response key response in
		     write_request connection.output async_response
                     end

              | Response (key, response) ->
		 try
		   let _, wakener = AsyncPool.get key in
		   AsyncPool.free key;
		   (try Lwt.wakeup wakener (S.unwrap_value response)
		    with exn -> Lwt.wakeup_exn wakener exn);
		   Lwt.return_unit
		 with _ -> connection.close ()
              end
        end;
      Lwt.return_unit
  end
