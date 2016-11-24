open Lwt.Infix
open SyncTypes

module ContentTypes = HttpHandler.ContentTypes
module File = Utils_File
module Iterable = Utils_Iterable

let handler_name = "files"

let server_header = HttpProtocol.Header.Server, "OCaml"

let root =
  let root = ref "." in
  let specs = [ "files-root", Arg.Set_string root,
		"<root> Root folder served by the HttpFileHandler" ]
  in
  Sync_Utils_CommandLine.parse specs;
  !root

let get_requested_filepath request =
  let open HttpProtocol in
  request.start_line.request_target
  |> UrlEncode.target_of_string
  |> UrlEncode.path (* get the path from the query. *)
  |> File.chroot 1 (* strip out the handler name. *)

let is_forbidden_file = function
  | "." | ".." | ".DS_Store" -> true
  | _ -> false

let get_content_type =
  let h = Hashtbl.create 16 in
  List.iter (fun (k,v) -> Hashtbl.add h k v)
	    [ "css", "text/css" ;
	      "gif", "image/gif" ;
	      "html", ContentTypes.html ;
	      "jpeg", "image/jpeg" ;
	      "jpg", "image/jpeg" ;
	      "js", "application/javascript; charset=utf-8" ;
	      "json", "application/json; charset=utf-8" ;
	      "ml", ContentTypes.plain_text ;
	      "pdf", "application/pdf" ;
	      "png", "image/png" ;
	      "txt", ContentTypes.plain_text ];
  fun file_extension ->
  HttpProtocol.Header.ContentType,
  try Hashtbl.find h (file_extension |> String.lowercase_ascii)
  with Not_found -> ContentTypes.plain_text

let read_file file =
  Lwt_io.open_file ~flags: [Unix.O_RDONLY]
		   ~mode: Lwt_io.Input
		   (root ^ File.to_string file)
  >|= fun channel ->
  begin fun () ->
  Lwt.catch
    begin fun () -> Lwt_io.read ~count: 1000 channel
                    >>= (function
	                 | "" -> Lwt_io.close channel >>= fun () -> Lwt.return_none
	                 | chunk -> Lwt.return_some chunk)
    end
    begin fun e -> Lwt_io.close channel
                   >>= fun () -> Lwt.fail e
    end
  end |> Lwt_stream.from

let reply_not_found, reply_bad_request =
  let module P = HttpProtocol in
  let aux response_code message =
    { P.status_line =
	{ P.response_http_version = P.HttpVersion.HTTP_1_1 ;
	  P.response_code = response_code } ;
      P.response_headers =
	P.Headers.from [ P.Header.ContentType, ContentTypes.plain_text ;
			 P.Header.chunked ;
			 P.Header.date () ;
			 server_header ] ;
      P.response_body = Lwt_stream.of_list [message] ;
    } |> Lwt.return
  in
  (fun _ -> aux P.ResponseCode.NotFound "Not found"),
  (fun _ -> aux P.ResponseCode.BadRequest "BadRequest")

let reply_regular_file file =
  read_file file
  >>= fun body ->
  let content_type = file |> File.extension |> get_content_type in
  let module P = HttpProtocol in
  { P.status_line =
      { P.response_http_version = P.HttpVersion.HTTP_1_1 ;
	P.response_code = P.ResponseCode.OK } ;
    P.response_headers =
      P.Headers.from [ content_type ;
		       P.Header.chunked ;
		       P.Header.date () ;
		       server_header ] ;
    P.response_body = body ;
  } |> Lwt.return

let file_template =
  let module H = HtmlParser in
  H.compile ~template: {|<li><a href=$expr:href>$expr:filename</a></li>|}
	    ~expr: [ "href", begin fun (_, path) ->
                             Printf.sprintf
                               "/%s%s"
                               handler_name
			       (path |> File.map UrlEncode.encode |> File.to_string)
                             end ;
		     "filename", fst]
	    ()

let files_template (dir, files) b =
  if File.is_root dir |> not then
    file_template ("..", dir |> File.parent) b;
  List.iter
    (fun file -> file_template (file, File.child dir file) b)
    files

let breadcrumb_template =
  let module H = HtmlParser in
  H.compile ~template: {| :: <a href=$expr:href>$expr:filename</a>|}
	    ~expr: [ "href", begin fun path ->
                             Printf.sprintf
                               "/%s%s"
                               handler_name
                               (path |> File.map UrlEncode.encode |> File.to_string)
                             end ;
		     "filename", File.filename]
	    ()

let breadcrumbs_template dir b =
  let rec aux accu dir =
    if File.is_root dir
    then (File.parse "/") :: accu
    else aux (dir :: accu) (File.parent dir)
  in
  List.iter
    (fun leg -> breadcrumb_template leg b)
    (aux [] dir)

let directory_template =
  let module H = HtmlParser in
  H.page ~title: "$expr:title"
	 ~body: {|<h1>$expr:title</h1>
		 <div>$html:breadcrumbs</div>
		 <div><ul>$html:files</ul></div>|}
	 ~html: [ "files", (fun data -> data |> files_template) ;
		  "breadcrumbs", (fun data -> data |> fst |> breadcrumbs_template) ]
	 ~expr: [ "title", (fun data -> match fst data
					with file when File.is_root file -> "root"
                                           | file -> File.filename file) ]
	 ()

let reply_dir dir =
  Lwt_unix.files_of_directory (root ^ File.to_string dir)
  |> Lwt_stream.filter (fun file -> not (is_forbidden_file file))
  |> Lwt_stream.to_list
  >>= fun files ->
  let body =
    let b = Buffer.create 1024 in
    directory_template (dir, files) b;
    Lwt_stream.of_list [ Buffer.contents b ]
  in
  let module P = HttpProtocol in
  { P.status_line =
      { P.response_http_version = P.HttpVersion.HTTP_1_1 ;
	P.response_code = P.ResponseCode.OK } ;
    P.response_headers =
      P.Headers.from [ P.Header.ContentType, ContentTypes.html ;
		       P.Header.chunked ;
		       P.Header.date () ;
		       server_header ] ;
    P.response_body = body }
  |> Lwt.return

let file_handler request connection =
  let file = get_requested_filepath request in
  if file |> File.to_iterable |> Iterable.any is_forbidden_file
  then reply_bad_request ()
  else
    Lwt.catch
      begin fun () -> Lwt_unix.stat (root ^ File.to_string file)
                      >>= fun stat ->
                      match stat.Unix.st_kind with
                      | Unix.S_REG -> reply_regular_file file
                      | Unix.S_DIR -> reply_dir file
                      | _ -> reply_not_found ()
      end
      reply_not_found

let () = HttpHandler.register_handler handler_name file_handler

(* TODO
let static_file_handler request connection =
  (let open Static.JQuery in if file <> file then raise Exit);
  Hashtbl.fold (fun k v t -> t >>= fun () -> Lwt_io.eprintlf "static file: %s" k)
	       Static.File.files
	       Lwt.return_unit
  >>= fun () -> Lwt_io.eprintlf "handle static file!" >>= fun () ->
  match get_requested_filepath request with
  | [ name ] ->
     let content =
       (Hashtbl.find Static.File.files name).Static.File.content in
     let content_type = [ name ] |> get_file_extension |> get_content_type in
     let module P = HttpProtocol in
     { P.status_line =
	 { P.response_http_version = P.HttpVersion.HTTP_1_1 ;
	   P.response_code = P.ResponseCode.OK } ;
       P.response_headers =
	 P.Headers.from [ content_type ;
			  P.Header.chunked ;
			  P.Header.date () ;
			  server_header ] ;
       P.response_body = Lwt_stream.of_list [ content ] ;
     } |> Lwt.return
  | _ -> reply_not_found ()

let register () = HttpHandler.register_handler "static" static_file_handler
*)
