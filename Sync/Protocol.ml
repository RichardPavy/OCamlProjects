open Lwt.Infix
open SyncTypes

type settings = {
    timeout : float
  }

let default_settings = {
    timeout = 10.
  }

exception Timeout

module type Serializer =
  sig
    type 'a value
    type 'a serialized
    val wrap_value : 'a value -> 'a serialized value
    val wrap_exn : exn -> 'a serialized value
    val unwrap_value : 'a serialized value -> 'a value
  end

module type IO = sig
    type 'a request
    type 'a response

    val prefix : char
    val default_settings : settings
    val get_operation : 'a request -> connection -> 'a response Lwt.t

    val read_request : Lwt_io.input_channel -> 'a request Lwt.t
    val write_request : Lwt_io.output_channel -> 'a request -> unit Lwt.t

    val read_response : Lwt_io.input_channel -> 'a response Lwt.t
    val write_response : Lwt_io.output_channel -> 'a response -> unit Lwt.t
  end

module type Sig = sig
    include IO
    val respond : connection -> unit Lwt.t
    val call : ?settings : settings -> connection -> 'a request -> 'a response Lwt.t
  end

module Make =
  functor (IO : IO) ->
  functor (S : Serializer with type 'a value = 'a IO.response) ->
  struct
    include IO
    let read_request channel =
      Lwt_io.atomic IO.read_request channel
    let write_request channel request =
      Lwt_io.atomic
	(fun channel -> IO.write_request channel request)
	channel
    let read_response channel =
      Lwt_io.atomic IO.read_response channel
    let write_response channel response =
      Lwt_io.atomic
	(fun channel -> IO.write_response channel response)
	channel
    let call ?(settings = IO.default_settings) connection (request : 'a request)  =
      write_request connection.output request
      >>= (fun () -> Lwt.catch
		       begin fun () ->
			     Lwt_unix.with_timeout
			       settings.timeout
			       (fun () -> read_response connection.input)
		       end
		       begin function
			 | Lwt_unix.Timeout ->
			    Lwt_io.eprintlf "Synchronous request from %s to %s timed out."
					    connection.root.root_name connection.peer_name
			    >>= connection.close
			    >>= fun () -> Lwt.fail Timeout
			 | exn -> Lwt.fail exn
		       end)
      >|= S.unwrap_value
    let respond connection =
      read_request connection.input
      >>= (fun request ->
	   Lwt.catch
	     (fun () -> get_operation request connection >|= S.wrap_value)
	     (fun exn -> Lwt.return (S.wrap_exn exn)))
      >>= (write_response connection.output)
  end
