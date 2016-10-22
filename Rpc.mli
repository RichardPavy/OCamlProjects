(** RPC server-side response.
    Uses the first letter to determine the protocol implementation to use. *)
val respond : SyncTypes.connection -> unit Lwt.t

(** Starts an RPC server that listens on the given IP and port number. *)
val establish_server :
  ?buffer_size: int ->
  ?backlog: int ->
  ip: string ->
  port: int ->
  SyncTypes.root -> (SyncTypes.connection -> unit) -> SyncTypes.root

(** Opens a connection to the given IP and port number. *)
val open_connection :
  ?buffer_size: int ->
  ip: string ->
  port: int ->
  SyncTypes.root ->
  SyncTypes.connection Lwt.t
