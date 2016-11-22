type 'a t

val create : ?capacity : int -> unit -> 'a t
val of_array : 'a array -> 'a t
val of_list : 'a list -> 'a t
val length : 'a t -> int
val get : 'a t -> int -> 'a
val add : 'a t -> 'a -> unit
val add_all : 'a t -> 'a Utils_Iterable.t -> unit
val sub : 'a t -> int -> int -> 'a t
val to_iterable : 'a t -> 'a Utils_Iterable.t
val to_foldable : 'a t -> 'a Utils_Foldable.t
val to_list : 'a t -> 'a list
val to_array : 'a t -> 'a array
val equals : ?eq: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
