type ('k, 'v) t
val create : ?finalize:('k -> 'v -> unit) -> int -> ('k, 'v) t
val clear : ('k, 'v) t -> unit
val get : ('k, 'v) t -> 'k -> 'v
val remove : ('k, 'v) t -> 'k -> unit
val put : ('k, 'v) t -> 'k -> 'v -> unit
val to_iterable : ('k, 'v) t -> ('k * 'v) Iterable.t
