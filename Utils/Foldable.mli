type 'e t
val fold : 'e t -> ('accu -> 'e -> 'accu) -> 'accu -> 'accu
val cycle : 'e -> 'e t
val of_array : 'e array -> 'e t
val of_queue : 'e Queue.t -> 'e t
val of_hashtbl : ('k, 'v) Hashtbl.t -> ('k * 'v) t
val concat : 'e t -> 'e t -> 'e t
val filter : ('e -> bool) -> 'e t -> 'e t
val map : ('e1 -> 'e2) -> 'e1 t -> 'e2 t
val top : int -> 'e t -> 'e t
val any : ('e -> bool) -> 'e t -> bool
val all : ('e -> bool) -> 'e t -> bool
val to_list : 'e t -> 'e list
val to_array : 'e t -> 'e array
