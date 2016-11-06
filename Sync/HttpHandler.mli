val register_handler :
  string ->
  (HttpProtocol.http_request -> SyncTypes.connection -> HttpProtocol.http_response Lwt.t) ->
  unit

module ContentTypes :
sig
  val plain_text : string
  val html : string
end
