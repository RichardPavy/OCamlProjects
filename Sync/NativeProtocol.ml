open Lwt.Infix
open SyncTypes

module type Prefix = sig val prefix : char end

module ExnWrapper = struct
  type 'a value = 'a
  type 'a serialized = Success of 'a | Failure of exn
  let wrap_value response = Success response
  let wrap_exn exn = Failure exn
  let unwrap_value = function
    | Success response -> response
    | Failure exn -> raise exn
end

module NativeIO =
  functor (Prefix : Prefix) ->
  struct
    include Prefix
    type 'a request = connection -> 'a response Lwt.t
     and 'a response = 'a

    let read channel = Lwt_io.read_value channel
    let read_request = read
    let read_response = read

    let write_request channel request =
      Lwt_io.write_char channel prefix
      >>= fun () -> Lwt_io.write_value ~flags:[Marshal.Closures] channel request
    let write_response channel response =
      Lwt_io.write_value ~flags:[Marshal.Closures] channel response

    let get_operation request = request

    let default_settings = Protocol.default_settings
  end

module SyncIO = NativeIO(struct let prefix = 'N' end)
module Sync = Protocol.Make(SyncIO)(ExnWrapper)

module AsyncIO = NativeIO(struct let prefix = 'A' end)
module Async =
  AsyncProtocol.Make
    (AsyncIO)
    (ExnWrapper)
    (struct
      include AsyncIO
      type 'a serialized = 'a ExnWrapper.serialized
      type 'a msg = ('a request, 'a response, 'a serialized response) AsyncProtocol.message
      let wrap_request key request =
	fun connection ->
	Lwt.return (AsyncProtocol.Request (key, request))
      let wrap_response key response =
	fun connection ->
	Lwt.return (AsyncProtocol.Response (key, response))
      let unwrap_response response = response
    end)
