type 'a t

val filter : ('a -> bool) -> 'a t -> 'a t
val any : ('a -> bool) -> 'a t -> bool
val all : ('a -> bool) -> 'a t -> bool
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val mem : 'a -> 'a t -> bool
val concat : 'a t -> 'a t -> 'a t
val flatten : 'a t t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val top : int -> 'a t -> 'a list

val make : (('a -> unit) -> unit) -> 'a t
val singleton : 'a -> 'a t
val empty : unit -> 'a t

val of_list : 'a list -> 'a t
val of_array : 'a array -> 'a t
val of_queue : 'a Queue.t -> 'a t
val of_hashtbl : ('k, 'v) Hashtbl.t -> ('k * 'v) t
val of_lazy : 'a t Lazy.t -> 'a t

val to_list : 'a t -> 'a list
val to_array : 'a t -> 'a array
