type operation =
  | Add of id * tag
  | Replace of id * tag
  | Remove of id
  | Nothing
 and id = string
 and tag = string

val page : title:string -> body:string -> 'a HtmlParser.template

val with_active_tags :
  'a HtmlParser.template ->
  ?active_tag_name: string ->
  ?active_tag: (string * UrlEncode.target) list ->
  'a HtmlParser.template

val register_handler : string -> (HttpProtocol.http_request -> HttpProtocol.body Lwt.t) -> unit

(** Adds the dynamic html pages handler to the http server. *)
val register : unit -> unit
