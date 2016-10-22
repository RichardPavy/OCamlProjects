type quota

(** Creates a quota with the initial capacity. *)
val create : int -> quota

(** [with_quota q n f] runs [f] using [n] resources of quota [q].
    Will not run until there is sufficient quota available in [q]. *)
val with_quota : quota -> int -> (unit -> 'a Lwt.t) -> 'a Lwt.t
