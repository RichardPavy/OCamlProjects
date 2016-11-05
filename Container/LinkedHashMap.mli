type ('k, 'v) t
val create : unit -> ('k, 'v) t
val add : ('k, 'v) t -> 'k -> 'v -> unit
val mem : ('k, 'v) t -> 'k -> bool
val get : ('a, 'b) t -> 'a -> 'b
val to_iterable : ('k, 'v) t -> ('k * 'v) Utils_Iterable.t
val keys : ('k, 'v) t -> 'k Utils_Iterable.t
val values : ('k, 'v) t -> 'v Utils_Iterable.t
