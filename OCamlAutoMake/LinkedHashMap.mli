type ('k, 'v) t
val create : unit -> ('k, 'v) t
val add : ('k, 'v) t -> 'k -> 'v -> unit
val mem : ('k, 'v) t -> 'k -> bool
val get : ('a, 'b) t -> 'a -> 'b
val to_iterable : ('k, 'v) t -> ('k * 'v) Iterable.t
val keys : ('k, 'v) t -> 'k Iterable.t
val values : ('k, 'v) t -> 'v Iterable.t
