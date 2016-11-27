type 'a t
val create : unit -> 'a t
val add : 'a t -> 'a -> unit
val mem : 'a t -> 'a -> bool
val of_iterable : 'a Utils_Iterable.t -> 'a t
val to_iterable : 'a t -> 'a Utils_Iterable.t
