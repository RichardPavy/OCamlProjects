open SyncTypes

type settings = {
    timeout : float;
  }

val default_settings : settings
exception Timeout

module type Serializer =
  sig
    type 'a value
    type 'a serialized
    val wrap_value : 'a value -> 'a serialized value
    val wrap_exn : exn -> 'a serialized value
    val unwrap_value : 'a serialized value -> 'a value
  end

module type IO =
  sig
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

module type Sig =
  sig
    include IO
    val respond : connection -> unit Lwt.t
    val call : ?settings:settings -> connection -> 'a request -> 'a response Lwt.t
  end

module Make :
functor (IO : IO) ->
functor (S : Serializer with type 'a value = 'a IO.response) ->
(Sig with type 'a request = 'a IO.request and
	  type 'a response = 'a IO.response)
