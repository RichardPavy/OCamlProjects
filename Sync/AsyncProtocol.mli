(** Module for Async protocols.

    With sync protocols, we must wait for the peer to respond to a request
    before making a new request.

    With async protocols, a machine can send several requests to a peer
    without waiting for the response. *)
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

(** Module that creates async protocols. *)
module Make :
functor (IO : Protocol.IO) ->
functor (S : Protocol.Serializer with type 'a value = 'a IO.response) ->
functor (W : AsyncWrapper with
	   type 'a request = 'a IO.request and
	   type 'a response = 'a IO.response and
	   type 'a serialized = 'a S.serialized) ->
(Protocol.Sig with type 'a request = 'a IO.request and
		   type 'a response = 'a IO.response)
