open Lwt.Infix
open SyncTypes

module AsyncUtils = Sync_Utils_AsyncUtils

let crlf = "\r\n"

(** This error is raised whenever stuff can't be parsed. *)
exception ParseError of string
let parse_error x = raise (ParseError x)

module Mapping = struct
  type ('token, 'value) mapping =
    { parse : 'value -> 'token ;
      print : 'token -> 'value ;
      tokens : 'token array }

  let make values unknown_value =
    let to_token = Hashtbl.create 16
    and to_value = Hashtbl.create 16
    and tokens = ref [] in
    List.iter
      (fun (token, value) ->
	Hashtbl.add to_token (String.uppercase_ascii value) token;
	Hashtbl.add to_value token value;
	tokens := token :: !tokens)
      values;
    let parse value =
      try Hashtbl.find to_token (String.uppercase_ascii value)
      with Not_found -> unknown_value value
    and print token = Hashtbl.find to_value token
    and tokens = !tokens |> List.rev |> Array.of_list
    in { parse ; print ; tokens }
end

module Method = struct
  type t = GET | POST

  let { Mapping.tokens = all ;
	parse ;
	print } =
    Mapping.make [ GET, "GET" ;
		   POST, "POST" ]
		 (fun m -> failwith ("Unknown method " ^ m))
end

module Header = struct
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
    | Expect (* Expect: 100-continue *)
    | Other of string
  type headers = (t, string) Hashtbl.t
  let { Mapping.tokens = all ;
	parse ;
	print } =
    Mapping.make [ Host, "Host" ;
		   Date, "Date" ;
		   Server, "Server" ;
		   ContentType, "Content-Type" ;
		   ContentEncoding, "Content-Encoding" ;
		   ContentLength, "Content-Length" ;
		   TransferEncoding, "Transfer-Encoding" ;
		   Connection, "Connection" ;
		   KeepAlive, "Keep-Alive" ;
		   Expect, "Expect" ]
		 (fun other -> Other other)

  let split_value =
    let spaces_regex = "\\([ \r\n\t]+\\)" in
    let regex = Str.regexp (spaces_regex ^ "," ^ spaces_regex) in
    Str.split regex

  let has_content_length headers = Hashtbl.mem headers ContentLength

  let has_header_with_value headers header expected =
    Hashtbl.mem headers header && begin
	(Hashtbl.find headers header)
	|> split_value
	|> (List.rev_map String.lowercase_ascii)
	|> (List.mem expected)
      end

  let has_chunked_transfer_encoding headers =
    has_header_with_value headers TransferEncoding "chunked"

  let has_expect_100_continue headers =
    has_header_with_value headers Expect "100-continue"

  let chunked = TransferEncoding, "chunked"

  let date, parse_date =
    let days = [| "Sun" ; "Mon" ; "Tue" ; "Wed" ; "Thu" ; "Fri" ; "Sat" |]
    and months = [| "Jan" ; "Feb" ; "Mar" ; "Apr" ;
                    "May" ; "Jun" ; "Jul" ; "Aug" ;
                    "Sep" ; "Oct" ; "Nov" ; "Dec" |] in
    let parse_month =
      let h = Hashtbl.create 12 in
      Array.iteri (fun i m -> Hashtbl.add h (String.lowercase_ascii m) i) months;
      fun month -> Hashtbl.find h (String.lowercase_ascii month)
    in

    let local_diff =
      let { Unix.tm_year = year } = Unix.time () |> Unix.localtime in
      let jan1 = { Unix.tm_mday = 1 ;
		   Unix.tm_mon = 0 ;
		   Unix.tm_year = year ;
		   Unix.tm_hour = 0 ;
		   Unix.tm_min = 0 ;
		   Unix.tm_sec = 0 ;
		   Unix.tm_wday = -1 ;
		   Unix.tm_yday = -1 ;
		   Unix.tm_isdst = false } in
      let jan1 = jan1 |> Unix.mktime |> fst in
      let jan1_local = Unix.localtime jan1 |> Unix.mktime |> fst
      and jan1_gmt = Unix.gmtime jan1 |> Unix.mktime |> fst
      in jan1_gmt -. jan1_local
    in

    let to_gmt_timestamp tm =
      let jan1_local_timestamp, _ =
	Unix.mktime { tm with
		      Unix.tm_mday = 1 ;
		      Unix.tm_mon = 0 }
      and _, { Unix.tm_yday = yday } = Unix.mktime tm
      in
      jan1_local_timestamp -. local_diff +. float_of_int (60*60*24*yday)
    in

    let date ?timestamp () =
      Date,
      let moment = begin match timestamp with
		   | Some t -> t
		   | None -> Unix.time ()
		   end |>  Unix.gmtime
      in
      let open Unix in
      Printf.sprintf "%s, %02d %s %d %02d:%02d:%02d GMT"
		     days.(moment.tm_wday)
		     moment.tm_mday
		     months.(moment.tm_mon)
		     (1900 + moment.tm_year)
		     moment.tm_hour
		     moment.tm_min
		     moment.tm_sec
    in
    let parse_date date =
      try
	Scanf.sscanf date "%_s@, %02d %s %d %02d:%02d:%02d"
		     (fun day month year h m s ->
		       { Unix.tm_mday = day ;
			 Unix.tm_mon = parse_month month ;
			 Unix.tm_year = year - 1900;
			 Unix.tm_hour = h ;
			 Unix.tm_min = m ;
			 Unix.tm_sec = s ;
			 Unix.tm_wday = -1 ;
			 Unix.tm_yday = -1 ;
			 Unix.tm_isdst = false })
	|> to_gmt_timestamp
      with
      | Scanf.Scan_failure _
      | Failure _
      | End_of_file
      | Invalid_argument _ -> parse_error date

    in date, parse_date

  let () = assert (date ~timestamp:1453872218. ()
                   = (Date, "Wed, 27 Jan 2016 05:23:38 GMT"))
  let () =
    assert begin
	try let aux timestamp = (date ~timestamp ()) |> snd |> parse_date in
	    for i = 0 to 365 * 24 * 10 do
	      let t = 1453872218. +. float_of_int (i*60*60) in
	      if t <> aux t
	      then failwith (((date ~timestamp:t ()) |> snd)
                             ^ " vs. "
                             ^ ((date ~timestamp:(aux t) ()) |> snd))
	    done;
	    true
	with e -> Printf.printf "%s\n" (Printexc.to_string e);
		  false
      end
end

module HttpVersion = struct
  type t = HTTP_1_1 | HTTP_1_0
  let { Mapping.tokens = all ;
	parse ;
	print } =
    Mapping.make [ HTTP_1_1, "HTTP/1.1" ;
		   HTTP_1_0, "HTTP/1.0" ]
		 (fun other -> failwith ("Unknown http version " ^ other))
end

module ResponseCode = struct
  (** See http://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml *)
  type t =
    | Continue
    | OK
    | BadRequest
    | NotFound
    | Other of int

  let { Mapping.tokens = all ;
	parse ;
	print } =
    Mapping.make [ Continue, "100" ;
		   OK, "200" ;
		   BadRequest, "400" ;
		   NotFound, "404" ]
		 (fun code -> Other (int_of_string code))

  let decode code =
    match code with
    | Continue -> (100, "Continue")
    | OK -> (200, "OK")
    | BadRequest -> (400, "Bad Request")
    | NotFound -> (404, "Not Found")
    | Other code -> (code, "Unknown Response Code")
end

type start_line = {
    http_method: Method.t ;
    request_target: string ;
    request_http_version: HttpVersion.t ;
  }

 and status_line = {
     response_http_version: HttpVersion.t ;
     response_code: ResponseCode.t
   }

 and headers = Header.headers
 and body = string Lwt_stream.t

 and http_request = {
     start_line: start_line ;
     request_headers: headers ;
     request_body: body ;
   }

 and http_response = {
     status_line: status_line ;
     response_headers: headers ;
     response_body: body ;
   }

module StartLine = struct
  type t = start_line

  let parse ?prefix channel =
    Lwt_io.read_line channel
    >|= fun start_line ->
    let start_line = match prefix
      with None -> start_line
	 | Some c -> (String.make 1 c) ^ start_line
    in
    let protocol_start = String.rindex start_line ' ' + 1 in
    let target_start = String.index start_line ' ' + 1 in
    let request_http_version =
      let v = String.sub start_line
                         protocol_start
                         (String.length start_line - protocol_start) in
      HttpVersion.parse v in
    let http_method = Method.parse (String.sub start_line 0 (target_start-1)) in
    let request_target = String.sub start_line
                                    target_start
                                    (protocol_start - target_start - 1)
    in { http_method ; request_target ; request_http_version }

  let print b start_line =
    Printf.bprintf b "%s %s %s%s"
		   (Method.print start_line.http_method)
		   start_line.request_target
		   (HttpVersion.print start_line.request_http_version)
		   crlf
end

module StatusLine = struct
  type t = status_line

  let parse =
    let spaces_regex = Str.regexp "\\([ \r\n\t]+\\)" in
    fun ?prefix channel ->
    Lwt_io.read_line channel
    >|= fun status_line ->
    let status_line = match prefix
      with None -> status_line
	 | Some c -> (String.make 1 c) ^ status_line
    in
    match Str.bounded_split spaces_regex status_line 3 with
    | response_http_version :: response_code :: _ ->
       let response_http_version = HttpVersion.parse response_http_version
       and response_code = ResponseCode.parse response_code
       in
       { response_http_version ; response_code }
    | _ -> failwith "parse_status_line"

  let print b status_line =
    let (code, reason) = ResponseCode.decode status_line.response_code in
    Printf.bprintf b "%s %s %s%s"
		   (HttpVersion.print status_line.response_http_version)
		   (string_of_int code)
		   reason
		   crlf
end

module Headers = struct
  type t = headers

  let add headers values =
    List.iter (fun (header, value) -> Hashtbl.add headers header value) values

  let from key_values =
    let headers = Hashtbl.create 16 in
    add headers key_values;
    headers

  let parse channel =
    let headers = Hashtbl.create 16 in
    let headers_stream = Lwt_io.read_lines channel in
    let rec read_headers () =
      Lwt_stream.next headers_stream
      >>= fun header_line ->
      if header_line <> ""
      then begin
	  let colon = String.index header_line ':' in
	  let header_name =
            Header.parse (String.trim (String.sub header_line 0 colon))
	  and header_value =
            String.trim (String.sub header_line
				    (colon + 1)
				    (String.length header_line - colon - 1))
	  in Hashtbl.add headers header_name header_value;
	     read_headers ()
	end
      else Lwt.return_unit
    in
    read_headers () >|= fun () -> headers

  let print b (headers : headers) =
    let module S = Set.Make (struct type t = Header.t let compare = compare end) in
    let sorted_keys = Hashtbl.fold (fun k v s -> S.add k s) headers S.empty in
    S.iter begin fun k ->
	   let v = Hashtbl.find headers k in
	   Array.iter (Buffer.add_string b)
		      [| Header.print k; ": "; v; crlf |]
	   end
	   sorted_keys;
    Buffer.add_string b crlf

  (** Hashtbl functions useful for headers. *)
  let find = Hashtbl.find
end

module Body = struct
  type t = body

  let empty = Lwt_stream.of_list []

  let read_data ?(chunked = false) channel length =
    let remaining = ref length in
    let stream_fn () =
      if !remaining = 0
      then if chunked
	   then Lwt_io.read ~count:(String.length crlf) channel
                >>= fun _ -> Lwt.return_none
	   else Lwt.return_none
      else Lwt_io.read ~count: !remaining channel
	   >|= fun data -> remaining := !remaining - (String.length data);
			   Some data
    in Lwt_stream.from stream_fn

  let parse channel headers =
    if Header.has_content_length headers then
      let length = int_of_string (Headers.find headers Header.ContentLength) in
      read_data channel length
    else if Header.has_chunked_transfer_encoding headers then
      let stream_fn () =
	Lwt_io.read_line channel
	>>= fun chunk_header ->
	let chunk_length = Scanf.sscanf chunk_header "%X" (fun x -> x) in
	if chunk_length = 0
	then (* skip the trailer *)
	  channel
	  |> Lwt_io.read_lines
	  |> Lwt_stream.junk_while (fun line -> line <> "")
	  >>= fun () -> Lwt.return_none
	else Lwt.return_some (read_data ~chunked:true channel chunk_length) in
      let chunks_stream = Lwt_stream.from stream_fn in
      Lwt_stream.concat chunks_stream
    else empty

  let to_chunked =
    let end_of_stream = "0" ^ crlf ^ crlf
    and to_chunk chunk =
      let length = String.length chunk in
      let length_hex = Printf.sprintf "%X\r\n" length in
      [ length_hex ; chunk ; crlf ]
    in
    fun stream ->
    Lwt_stream.append
      (Lwt_stream.map_list to_chunk stream)
      (Lwt_stream.of_list [ end_of_stream ])

  let print b headers body =
    if Header.has_content_length headers then
      let length = int_of_string (Headers.find headers Header.ContentLength)
      and concat stream =
	let b = Buffer.create 0 in
	Lwt_stream.iter (fun chunk -> Buffer.add_string b chunk) stream
	>|= fun () -> Buffer.contents b
      in
      concat body >>= fun body ->
      if length <> (String.length body)
      then Lwt.fail_with "Unexpected body size"
      else (Buffer.add_string b body; Lwt.return_unit)
    else if Header.has_chunked_transfer_encoding headers then
      Lwt_stream.iter (fun chunk -> Buffer.add_string b chunk) (to_chunked body)
    else Lwt.return_unit

  let write channel headers body =
    if Header.has_content_length headers then
      let length = int_of_string (Headers.find headers Header.ContentLength)
      and concat stream =
	let b = Buffer.create 0 in
	Lwt_stream.iter (fun chunk -> Buffer.add_string b chunk) stream
	>|= fun () -> Buffer.contents b
      in
      concat body >>= fun body ->
      if length <> (String.length body)
      then AsyncUtils.fail "Unexpected body size"
      else Lwt_io.write channel body
    else if Header.has_chunked_transfer_encoding headers then
      Lwt_stream.iter_s (Lwt_io.write channel) (to_chunked body)
    else
      AsyncUtils.fail "Unknown body encoding"

  let fix_body_size headers body =
    if Header.has_content_length headers then
      Lwt.return_unit
    else if Header.has_chunked_transfer_encoding headers then
      Lwt.return_unit
    else
      Lwt_stream.peek body
      >|= function
      | None -> () (* empty body, not headers for body *)
      | Some _ -> Headers.add headers [ Header.chunked ]

  let write_message_aux
	~channel
	~print_first_line
	~headers
	~body =
    fix_body_size headers body
    >>= fun () ->
    let http_headers =
      let b = Buffer.create 80 in
      print_first_line b;
      Headers.print b headers ;
      Buffer.contents b
    in
    (* Write headers in a single Lwt_io.write call. *)
    Lwt_io.write channel http_headers
    >>= fun () -> write channel headers body
end

module HttpRequest =
  struct
    type t = http_request

    let parse ?prefix channel =
      StartLine.parse ?prefix channel
      >>= fun start_line -> Headers.parse channel
      >>= begin fun request_headers ->
          let request_body = Body.parse channel request_headers in
	  Lwt.return { start_line ; request_headers ; request_body }
          end

    let print b { start_line = start_line ;
		  request_headers = headers ;
		  request_body = body } =
      StartLine.print b start_line;
      Headers.print b headers;
      Body.print b headers body

    let write channel { start_line ;
			request_headers = headers ;
			request_body = body } =
      Body.write_message_aux
	~channel
	~print_first_line:(fun buffer -> StartLine.print buffer start_line)
	~headers
	~body
  end

module HttpResponse =
  struct
    type t = http_response

    let parse ?prefix channel =
      StatusLine.parse ?prefix channel
      >>= fun status_line -> Headers.parse channel
      >>= begin fun response_headers ->
          let response_body = Body.parse channel response_headers in
	  Lwt.return { status_line ; response_headers ; response_body }
          end

    let print b { status_line ;
		  response_headers = headers ;
		  response_body = body } =
      StatusLine.print b status_line;
      Headers.print b headers;
      Body.print b headers body

    let write channel { status_line ;
			response_headers = headers ;
			response_body = body } =
      Body.write_message_aux
	~channel
	~print_first_line:(fun buffer -> StatusLine.print buffer status_line)
	~headers
	~body
  end

exception HttpException of http_response

module ExnWrapper =
  struct
    type 'a value = http_response
    type 'a serialized = http_response
    let wrap_value response = response
    let wrap_exn exn =
      { status_line =
	  { response_http_version = HttpVersion.HTTP_1_1 ;
	    response_code = ResponseCode.BadRequest } ;
	response_headers =
	  Headers.from [ Header.ContentType, "text/plain" ;
			 Header.chunked ;
			 Header.date () ] ;
	response_body =
	  let module Y = Yojson.Basic in
	  let module E = ExnTranslator in
	  let open Y.Util in
	  [ exn |> E.to_json |> E.repr |> Y.to_string ]
	  |> Lwt_stream.of_list }
    let unwrap_value response =
      match response.status_line.response_code with
      | ResponseCode.Continue
      | ResponseCode.OK -> response
      | other_code -> raise (HttpException response)
  end

let handler = ref (fun request -> failwith "Undefined http handler")
let set_handler h = handler := h

module type Prefix = sig val prefix : char end
module IO =
  functor (Prefix : Prefix) ->
  struct
    type 'a request = http_request
    type 'a response = http_response

    let prefix = Prefix.prefix
    let default_settings = Protocol.default_settings

    let get_operation : http_request -> SyncTypes.connection -> http_response Lwt.t =
      fun request -> !handler request

    let read_request = HttpRequest.parse ~prefix:Prefix.prefix
    let read_response = HttpResponse.parse ~prefix:Prefix.prefix

    let write_request = HttpRequest.write
    let write_response = HttpResponse.write
  end

let modules =
  let letters =
    Array.fold_left
      begin fun letters m ->
      let letter = (Method.print m).[0] in
      if List.mem letter letters
      then letters
      else letter :: letters
      end
      []
      Method.all
  in List.rev_map
       begin fun prefix ->
       let module P = Protocol.Make(IO(struct let prefix = 'G' end))(ExnWrapper)
       in (module P : Protocol.Sig)
       end
       letters

(*****************)
(***** Tests *****)
(*****************)

let () =
  assert begin
      let hashtbl_eq a b =
        let open Hashtbl in
        length a = length b && begin
	      try Hashtbl.iter (fun k v -> if v <> find b k then raise Exit) a; true
	      with Not_found | Exit -> false;
	    end
      in

      let test_parse_request () =
        let text =
          "GET /path/file.html HTTP/1.1" ^ crlf
          ^ "Host: www.host1.com:80" ^ crlf
          ^ "Date: Fri, 31 Dec 1999 23:59:59 GMT" ^ crlf
          ^ "Content-Type: text/plain" ^ crlf
          ^ crlf
          ^ "body..." ^ crlf
        in
        text |> Lwt_bytes.of_string
        |> (Lwt_io.of_bytes ~mode:Lwt_io.input)
        |> HttpRequest.parse
        >>= begin fun actual ->
            let expected =
              let expected_headers =
                Headers.from [ Header.Host, "www.host1.com:80" ;
			       Header.Date, "Fri, 31 Dec 1999 23:59:59 GMT" ;
			       Header.ContentType, "text/plain" ]
              in
              { start_line = { http_method = Method.GET ;
		               request_target = "/path/file.html" ;
		               request_http_version = HttpVersion.HTTP_1_1 } ;
	        request_headers = expected_headers ;
	        request_body = Body.empty }
            in
            if actual.start_line <> expected.start_line then
              Lwt_io.eprintl "Start lines don't match"
              >>= fun () -> Lwt.fail_with "test_parse_request"
            else if not (hashtbl_eq actual.request_headers expected.request_headers) then
              Lwt_io.eprintl "Headers don't match"
              >>= fun () -> Lwt.fail_with "test_parse_request"
            else
              Lwt.return_unit
            end

      and test_print_request () =
        let print_request r =
          let b = Buffer.create 10 in
          HttpRequest.print b r
          >|= fun () -> Buffer.contents b
        in
        let test_aux ?request expected =
          (match request
           with None -> expected ^ "body..." ^ crlf
	      | Some request -> request)
          |> Lwt_bytes.of_string
          |> (Lwt_io.of_bytes ~mode:Lwt_io.input)
          |> HttpRequest.parse
          >>= print_request
          >>= begin fun actual ->
              if expected <> actual
	      then Lwt_io.eprintlf "Request \n%s\nis not equal to\n%s" actual expected
	           >>= fun () -> Lwt.fail_with "test_print_request"
	      else Lwt.return_unit
              end
        in
        begin
          let request =
	    "get /path/file.html http/1.1" ^ crlf
	    ^ "host:\twww.host1.com:80" ^ crlf
	    ^ "date:   Fri, 31 Dec 1999 23:59:59 GMT" ^ crlf
	    ^ "content-TYPE:text/plain" ^ crlf
	    ^ "content-length:9" ^ crlf
	    ^ crlf
	    ^ "body..." ^ crlf
          and expected =
	    "GET /path/file.html HTTP/1.1" ^ crlf
	    ^ "Host: www.host1.com:80" ^ crlf
	    ^ "Date: Fri, 31 Dec 1999 23:59:59 GMT" ^ crlf
	    ^ "Content-Type: text/plain" ^ crlf
	    ^ "Content-Length: 9" ^ crlf
	    ^ crlf
	    ^ "body..." ^ crlf
          in
          test_aux ~request expected
        end

        >>= begin fun () ->
            let body =
	      "10" ^ crlf
	      ^ "0123456789012345" ^ crlf
	      ^ "1" ^ crlf
	      ^ "x" ^ crlf
	      ^ "0" ^ crlf ^ crlf
            in
            let request =
	      "get /path/file.html http/1.1" ^ crlf
	      ^ "host:\twww.host1.com:80" ^ crlf
	      ^ "date:   Fri, 31 Dec 1999 23:59:59 GMT" ^ crlf
	      ^ "content-TYPE:text/plain" ^ crlf
	      ^ "transfer-encoding: chunked" ^ crlf
	      ^ crlf
	      ^ body
            and expected =
	      "GET /path/file.html HTTP/1.1" ^ crlf
	      ^ "Host: www.host1.com:80" ^ crlf
	      ^ "Date: Fri, 31 Dec 1999 23:59:59 GMT" ^ crlf
	      ^ "Content-Type: text/plain" ^ crlf
	      ^ "Transfer-Encoding: chunked" ^ crlf
	      ^ crlf
	      ^ body
            in
            test_aux ~request expected
            end

        >>= begin fun () ->
            let expected =
	      "POST /path/file.html HTTP/1.1" ^ crlf
	      ^ "Host: www.host1.com:80" ^ crlf
	      ^ "Date: Fri, 31 Dec 1999 23:59:59 GMT" ^ crlf
	      ^ "Content-Type: text/plain" ^ crlf
	      ^ crlf
            in
            test_aux expected
            end

      and test_parse_response () =
        let text =
          "HTTP/1.1 200 OK" ^ crlf
          ^ "Date: Fri, 31 Dec 1999 23:59:59 GMT" ^ crlf
          ^ "Server: Apache" ^ crlf
          ^ "content-type: text/html" ^ crlf
          ^ "Content-Encoding: gzip" ^ crlf
          ^ "Content-Length: 3369" ^ crlf
          ^ "Connection: Keep-Alive" ^ crlf
          ^ "Keep-Alive: timeout=15, max=100" ^ crlf
          ^ crlf
          ^ "body..." ^ crlf
        in
        text |> Lwt_bytes.of_string
        |> (Lwt_io.of_bytes ~mode:Lwt_io.input)
        |> HttpResponse.parse

        >>= begin fun actual ->
            let expected =
              let expected_headers =
	        [ Header.Date, "Fri, 31 Dec 1999 23:59:59 GMT" ;
	          Header.Server, "Apache" ;
	          Header.ContentType, "text/html" ;
	          Header.ContentEncoding, "gzip" ;
	          Header.ContentLength, "3369" ;
	          Header.KeepAlive, "timeout=15, max=100" ;
	          Header.Connection, "Keep-Alive" ] |> Headers.from
              in
              { status_line = { response_http_version = HttpVersion.HTTP_1_1 ;
			        response_code = ResponseCode.OK} ;
	        response_headers = expected_headers ;
	        response_body = Body.empty }
            in
            if actual.status_line <> expected.status_line then
              Lwt_io.eprintl "Status lines don't match"
              >>= fun () -> Lwt.fail_with "test_parse_response"
            else if not (hashtbl_eq actual.response_headers expected.response_headers) then
              Lwt_io.eprintl "Headers don't match"
              >>= fun () -> Lwt.fail_with "test_parse_response"
            else
              Lwt.return_unit
            end

      and test_print_response () =
        let print_response r =
          let b = Buffer.create 10 in
          HttpResponse.print b r
          >|= fun () -> Buffer.contents b
        in
        let test_aux response expected =
          response
          |> Lwt_bytes.of_string
          |> (Lwt_io.of_bytes ~mode:Lwt_io.input)
          |> HttpResponse.parse
          >>= fun parsed_response -> print_response parsed_response
          >>= begin fun actual ->
              if expected <> actual
	      then Lwt_io.eprintlf "Response \n%s\nis not equal to\n%s" actual expected
		   >>= fun () -> Lwt.fail_with "test_print_response"
	      else Lwt.return_unit
              end
        in
        begin
          let response =
	    "http/1.1  200  ok" ^ crlf
	    ^ "host:\twww.host1.com:80" ^ crlf
	    ^ "date:   Fri, 31 Dec 1999 23:59:59 GMT" ^ crlf
	    ^ "content-TYPE:text/plain" ^ crlf
	    ^ "content-length:9" ^ crlf
	    ^ crlf
	    ^ "body..." ^ crlf
          and expected =
	    "HTTP/1.1 200 OK" ^ crlf
	    ^ "Host: www.host1.com:80" ^ crlf
	    ^ "Date: Fri, 31 Dec 1999 23:59:59 GMT" ^ crlf
	    ^ "Content-Type: text/plain" ^ crlf
	    ^ "Content-Length: 9" ^ crlf
	    ^ crlf
	    ^ "body..." ^ crlf
          in
          test_aux response expected
        end

      in
      Lwt_main.run begin
          Lwt.return_unit
          >>= test_parse_request
          >>= test_print_request
          >>= test_parse_response
          >>= test_print_response
        end;
      true
    end
