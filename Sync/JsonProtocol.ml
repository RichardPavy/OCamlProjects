open Lwt.Infix
open SyncTypes

module Y = Yojson.Basic

type operation = Y.json -> connection -> Y.json Lwt.t

let operations : (string, operation) Hashtbl.t = Hashtbl.create 16
let register_operation name operation =
  Hashtbl.replace operations name operation

module ExnWrapper = struct
    type 'a value = Y.json
    type 'a serialized = Y.json
    let success_str = "success"
    and response_str = "response"
    and exception_str = "exception"
    let wrap_value response =
      `Assoc [ success_str, `Bool true ;
	       response_str, response ]
    let wrap_exn exn =
      `Assoc [ success_str, `Bool false ;
	       exception_str, ExnTranslator.to_json exn ]
    let unwrap_value response =
      let open Y.Util in
      if response |> (member success_str) |> to_bool
      then response |> (member response_str)
      else raise (response |> (member exception_str) |> ExnTranslator.to_exn)
  end

module SyncIO =
  struct
    type 'a request = Y.json
    type 'a response = Y.json

    let prefix = '{'

    let read channel b =
      let rec concat l = match l with
	| t :: q -> Buffer.add_string b t; Buffer.add_char b '\n'; concat q
	| [] -> Buffer.contents b
      in
      let stream =
	Lwt_stream.get_while
	  (fun line -> line <> "")
	  (Lwt_io.read_lines channel)
      in
      stream >|= concat
      (* DEBUG: * >>= (fun message -> Lwt_io.eprintl message >>= fun () -> Lwt.return message) **)
      >>= Lwt.wrap1 Y.from_string
    let read_request channel =
      let b = Buffer.create 32 in
      Buffer.add_char b prefix;
      read channel b
    let read_response channel = read channel (Buffer.create 32)

    let write channel v =
      Lwt_io.write_line channel (Y.to_string v)
      >>= (fun () -> Lwt_io.write_line channel "")
    let write_request = write
    let write_response = write

    let get_operation request =
      let open Y.Util in
      let name = request |> (member "operation") |> to_string in
      try Hashtbl.find operations name request
      with Not_found -> fun connection -> failwith ("Operation not found: <" ^ name ^ ">")

    let default_settings = Protocol.default_settings
  end

module Sync = Protocol.Make(SyncIO)(ExnWrapper)

module AsyncIO =
  struct
    type 'a request = Y.json
    type 'a response = Y.json

    exception BadAsyncJsonType of Y.json

    let async_request = "async_request"
    let async_response = "async_response"

    type message_type = Request | Response

    let get_type async_message =
      let open Y.Util in
      match async_message |> (member "operation") |> to_string with
      | "async_request" -> Request
      | "async_response" -> Response
      | _ -> raise (BadAsyncJsonType async_message)

    let prefix = 'J'

    let read_request channel =
      let b = Buffer.create 32 in
      SyncIO.read channel b

    let write_request channel v =
      Lwt_io.write_char channel prefix
      >>= (fun () -> SyncIO.write channel v)

    let read_response channel = Utils.fail "Unsupported operation JsonProtocol.AsyncIO.read_response"
    let write_response channel v = Utils.fail "Unsupported operation JsonProtocol.AsyncIO.write_response"

    let get_operation = SyncIO.get_operation
    let () =
      let open Y.Util in
      register_operation
	async_request
	begin fun request connection ->
	      Lwt.return request
	end
    let () =
      register_operation
	async_response
	begin fun response connection ->
	      Lwt.return response
	end

    let default_settings = SyncIO.default_settings
  end

module Async =
  AsyncProtocol.Make
    (AsyncIO)
    (ExnWrapper)
    (struct
	include AsyncIO
	type 'a serialized = 'a ExnWrapper.serialized
	type 'a msg = ('a request, 'a response, 'a serialized response) AsyncProtocol.message
	module Key = Pool.Key

	let wrap_request key request =
	  `Assoc [ "operation", `String async_request ;
		   "key", `Int (Key.index key) ;
		   "request", request ]
	let wrap_response key response =
	  `Assoc [ "operation", `String async_response ;
		   "key", `Int (Key.index key) ;
		   "response", response ]
	let unwrap_response response =
	  let open Y.Util in
	  let key = Key.key (response |> (member "key") |> to_int) in
	  match get_type response with
	  | Request ->
	     AsyncProtocol.Request(key, response |> (member "request"))
	  | Response ->
	     AsyncProtocol.Response(key, response |> (member "response"))
      end)
