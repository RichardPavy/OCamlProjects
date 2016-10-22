(** Module to translate exceptions to JSON. *)

module Y = Yojson.Basic

type builtin_exn = Unknown of string | System of string | Unexpected
exception BuiltinException of builtin_exn

val register_translator : exn -> to_exn:(Y.json -> exn) -> to_json:(exn -> Y.json) -> unit
val to_exn : Y.json -> exn
val to_json : exn -> Y.json

(** Returns a json message representing just the exception *)
val repr : Y.json -> Y.json
