val encode : ?plus:bool -> string -> string
val decode : ?plus:bool -> string -> string

(** Target of an HTTP request. *)
type target = { path : path ;
		query : query }
 and path = Utils_File.t
 and query = (string * string) list

val path : target -> path
val query : target -> query

val target_of_string : ?plus:bool -> string -> target
val string_of_target : ?plus:bool -> target -> string
