module File = Utils_File

(** Name of the HttpFileHandler.
    Handles requests starting with /files/xxx *)
val handler_name : string

(** Returns the requested file path. *)
val get_requested_filepath : HttpProtocol.http_request -> File.t

(** Returns whether the file name should not show in the UI at all. *)
val is_forbidden_file : string -> bool

(** Returns the 'Content-Type' header for the requested file. *)
val get_content_type : string -> HttpProtocol.Header.t * string

(** Callback used to handle regular files. *)
val reply_regular_file : File.t -> HttpProtocol.http_response Lwt.t

(** Callback used to handle folders. *)
val reply_dir : File.t -> HttpProtocol.http_response Lwt.t

(** Http file handler. Handles requests starting with /files/ *)
val file_handler : HttpProtocol.http_request ->
		   SyncTypes.connection ->
		   HttpProtocol.http_response Lwt.t

(** TODO Adds the file handler to the http server.
val register : unit -> unit
*)
