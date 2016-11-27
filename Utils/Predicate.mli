type 'a t = 'a -> bool

val always_true : unit -> 'a t
val always_false : unit -> 'a t
val regexp : string -> string t
val equals : 'a -> 'a t
val oneof : 'a list -> 'a t
val basename : string -> File.t t
val extension : string -> File.t t
val filename : string -> File.t t
val full_base : string -> File.t t
val parent : string -> File.t t

module Infix : sig
  val ( &&$ ) : 'a t -> 'a t -> 'a t
  val ( ||$ ) : 'a t -> 'a t -> 'a t
  val ( !$ ) : 'a t -> 'a t
  val ( !&&$ ) : 'a t Iterable.t -> 'a t
  val ( !||$ ) : 'a t Iterable.t -> 'a t
end
