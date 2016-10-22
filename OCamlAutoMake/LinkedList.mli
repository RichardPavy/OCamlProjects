type 'a t
type 'a handle

val create : unit -> 'a t
val add : 'a t -> 'a -> 'a handle
val get_value : 'a handle -> 'a
val length : 'a t -> int
val remove : 'a handle -> unit

val get_first : 'a t -> 'a
val get_last : 'a t -> 'a
val remove_first : 'a t -> unit
val remove_last : 'a t -> unit

val of_list : 'a list -> 'a t
val of_iterable : 'a Iterable.t -> 'a t

val to_iterable : 'a t -> 'a Iterable.t
val to_forward_iterable : 'a t -> 'a Iterable.t
val to_backward_iterable : 'a t -> 'a Iterable.t

val to_list : 'a t -> 'a list
val to_forward_list : 'a t -> 'a list
val to_backward_list : 'a t -> 'a list
