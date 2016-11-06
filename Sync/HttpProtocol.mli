(** This error is raised whenever stuff can't be parsed. *)
exception ParseError of string

module Method :
sig
  type t = GET | POST
  val parse : string -> t
  val print : t -> string
  val all : t array
end

module Header :
sig
  type t =
    | Host
    | Date
    | Server
    | ContentType
    | ContentEncoding
    | ContentLength
    | TransferEncoding
    | Connection
    | KeepAlive
    | Expect
    | Other of string
  type headers = (t, string) Hashtbl.t
  val parse : string -> t
  val print : t -> string
  val all : t array
  val split_value : string -> string list
  val has_content_length : headers -> bool
  val has_chunked_transfer_encoding : headers -> bool
  val has_expect_100_continue : headers -> bool
  val chunked : t * string
  val date : ?timestamp:float -> unit -> t * string
end

module HttpVersion :
sig
  type t = HTTP_1_1 | HTTP_1_0
  val parse : string -> t
  val print : t -> string
  val all : t array
end

module ResponseCode :
sig
  type t = Continue | OK | BadRequest | NotFound | Other of int
  val parse : string -> t
  val print : t -> string
  val all : t array
  val decode : t -> int * string
end

type start_line = {
    http_method : Method.t;
    request_target : string;
    request_http_version : HttpVersion.t;
  }
 and status_line = {
     response_http_version : HttpVersion.t;
     response_code : ResponseCode.t;
   }
 and headers = Header.headers
 and body = string Lwt_stream.t
 and http_request = {
     start_line : start_line;
     request_headers : headers;
     request_body : body;
   }
 and http_response = {
     status_line : status_line;
     response_headers : headers;
     response_body : body;
   }

(** Start line of HTTP requests.
    start_line: <GET|POST> <target> <HTTP/1.1> *)
module StartLine :
sig
  type t = start_line
  val parse : ?prefix:char -> Lwt_io.input_channel -> start_line Lwt.t
  val print : Buffer.t -> start_line -> unit
end

(** Status line of HTTP responses.
    status_line: <HTTP/1.1> <3 digit code> <Explanation> *)
module StatusLine :
sig
  type t = status_line
  val parse : ?prefix:char -> Lwt_io.input_channel -> status_line Lwt.t
  val print : Buffer.t -> status_line -> unit
end

(** Headers of HTTP requests or responses. *)
module Headers :
sig
  type t = headers
  val add : headers -> (Header.t * string) list -> unit
  val from : (Header.t * string) list -> headers
  val parse : Lwt_io.input_channel -> headers Lwt.t
  val print : Buffer.t -> headers -> unit
  val find : headers -> Header.t -> string
end

module Body :
sig
  type t = body
  val empty : body

  val parse : Lwt_io.input_channel -> headers -> body

  (** Turns a stream of text into a stream of chunks (length in hex + data *)
  val to_chunked : body -> body
  val print : Buffer.t -> headers -> body -> unit Lwt.t
  val write : Lwt_io.output_channel -> headers -> body -> unit Lwt.t

  (** Adds header "Transfer-Encoding: chunked" if missing *)
  val fix_body_size : headers -> body -> unit Lwt.t

  (** Helper method used to write requests and responses. *)
  val write_message_aux :
    channel: Lwt_io.output_channel ->
    print_first_line: (Buffer.t -> unit) ->
    headers: headers ->
    body: body ->
    unit Lwt.t
end

module HttpRequest :
sig
  type t = http_request
  val parse : ?prefix:char -> Lwt_io.input_channel -> http_request Lwt.t
  val print : Buffer.t -> http_request -> unit Lwt.t
  val write : Lwt_io.output_channel -> http_request -> unit Lwt.t
end

module HttpResponse :
sig
  type t = http_response
  val parse : ?prefix:char -> Lwt_io.input_channel -> http_response Lwt.t
  val print : Buffer.t -> http_response -> unit Lwt.t
  val write : Lwt_io.output_channel -> http_response -> unit Lwt.t
end

exception HttpException of http_response

module ExnWrapper :
sig
  type 'a value = http_response
  type 'a serialized = http_response
  val wrap_value : http_response -> http_response
  val wrap_exn : exn -> http_response
  val unwrap_value : http_response -> http_response
end

val set_handler : (http_request -> SyncTypes.connection -> http_response Lwt.t) -> unit

module type Prefix = sig val prefix : char end

module IO :
functor (Prefix : Prefix) ->
sig
  type 'a request = http_request
  type 'a response = http_response
  val prefix : char
  val default_settings : Protocol.settings
  val get_operation : http_request -> SyncTypes.connection -> http_response Lwt.t
  val read_request : Lwt_io.input_channel -> http_request Lwt.t
  val read_response : Lwt_io.input_channel -> http_response Lwt.t
  val write_request : Lwt_io.output_channel -> http_request -> unit Lwt.t
  val write_response : Lwt_io.output_channel -> http_response -> unit Lwt.t
end

val modules : (module Protocol.Sig) list
