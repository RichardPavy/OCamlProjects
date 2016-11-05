type 'a t
val create : unit -> 'a t
val add : 'a t -> 'a -> unit
val mem : 'a t -> 'a -> bool
val of_iterable : 'a Utils_Iterable.t -> 'a t
val to_iterable : 'a t -> 'a Utils_Iterable.t
val of_list : 'a list -> 'a t
val of_array : 'a array -> 'a t
val eq : 'a t -> 'a t -> bool
val union : 'a Utils_Iterable.t -> 'a Utils_Iterable.t -> 'a t
