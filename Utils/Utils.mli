val join : string -> string Iterable.t -> string
val split : char -> string -> string list
val starts_with : string -> string -> bool
val ends_with : string -> string -> bool
val fail : ('a, unit, string, exn) format4 -> 'a
val check : bool -> ('a, unit, string, unit) format4 -> 'a
val dcheck : bool -> ('a, unit, string, bool) format4 -> 'a
val toggle : 'a ref -> 'a -> (unit -> 'b) -> 'b

module Option :
sig
  val ( |?> ) : 'a option -> ('a -> 'b) -> 'b option
  val ( ?> ) : ('a -> unit) -> 'a option -> unit
  val ( !? ) : 'a option -> 'a
end
