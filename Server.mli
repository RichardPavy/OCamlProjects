(** Unique server name.
    Can be used to uniquely identify this server. *)
val name : string

(** Default server IP to listen to. *)
val ip : string

(** Default server port number to listen to. *)
val port : int

(** Serve RPC's comming from the given connection. *)
val serve : SyncTypes.connection -> unit

(** Starts a server.
    Listens to the given ip/port and runs [serve] on all connections. *)
val start : ?name:string -> ?ip:string -> ?port:int -> unit -> SyncTypes.root

(** For testing purposes. *)
val create_root : ip:string -> port:int -> SyncTypes.root
