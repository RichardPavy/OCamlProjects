open Lwt.Infix
module ContentTypes = HttpHandler.ContentTypes

type operation =
  | Add of id * tag
  | Replace of id * tag
  | Remove of id
  | Nothing
 and id = string
 and tag = string

 and url = { handler : string ;
	     parameters : (string * string) list }

let handler_name = "dynamic"

let server_header = HttpProtocol.Header.Server, "OCaml"

let page_aux =
  HtmlParser.compile
    ~template: {|<!DOCTYPE html>
<html>
  <head>
    <title>$html:title</title>
    <script src=$expr:jquery></script>
    <script src=$expr:dynamic-pages></script>
  </head>
  <body>$html:body</body>
</html>|}
    ~const: [ "title", fst ; "body", snd ]
    ~expr: [ "jquery", (fun _ -> Printf.sprintf "/%s/jquery.js" handler_name) ;
	     "dynamic-pages", (fun _ -> Printf.sprintf "/%s/jquery.js" handler_name) ]
    ()

(** Template for dynamic pages. *)
let page ~title ~body =
  let b = Buffer.create 80 in
  page_aux (title, body) b;
  HtmlParser.compile ~template: (Buffer.contents b)

let with_active_tags (fn : 'a HtmlParser.template)
		     ?(active_tag_name = "div")
		     ?(active_tag = [])
		     ?(widget = [])
		     ?const
		     ?name
		     ?(expr = []) =
  let make_active_tag name =
    Printf.sprintf {|<%s class="dynamic-tag" data-dynamic-url=$expr:%s-url>$expr:%s</%s>|}
		   active_tag_name name name active_tag_name
  in
  fn ~widget: begin
       List.fold_left
	 (fun accu (name, _) -> (name, make_active_tag) :: accu)
	 widget
	 active_tag
     end
     ?const
     ?name
     ~expr: begin
       List.fold_left
	 (fun accu (name, url) ->
           (name ^ "-url", fun _ -> UrlEncode.string_of_target url)
           :: accu)
	 expr
	 active_tag
     end

let dynamic_page_handlers = Hashtbl.create 10

let register_handler name handler =
  if Hashtbl.mem dynamic_page_handlers name
  then failwith ("Dynamic page handler %s already defined." ^ name)
  else Hashtbl.add dynamic_page_handlers name handler

let dynamic_page_handler =
  let requested_handler_name_format =
    Scanf.format_from_string (Printf.sprintf "/%s/%%s" handler_name) "%s"
  in
  let get_requested_handler_name request =
    let open HttpProtocol in
    request.start_line.request_target
    |> UrlEncode.target_of_string
    |> UrlEncode.path (* get the path from the query. *)
    |> Utils_File.to_string
    |> (fun target -> Scanf.sscanf target requested_handler_name_format
				   (fun target -> target))
  in
  fun request connection ->
  let handler =
    request
    |> get_requested_handler_name
    |> Hashtbl.find dynamic_page_handlers
  in handler request
     >>= fun response ->
     let module P = HttpProtocol in
     { P.status_line =
	 { P.response_http_version = P.HttpVersion.HTTP_1_1 ;
	   P.response_code = P.ResponseCode.OK } ;
       P.response_headers =
	 P.Headers.from [ P.Header.ContentType, ContentTypes.html ;
			  P.Header.chunked ;
			  P.Header.date () ;
			  server_header ] ;
       P.response_body = response }
     |> Lwt.return

let register () = HttpHandler.register_handler handler_name dynamic_page_handler
