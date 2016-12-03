type kind = ..
type kind += Unknown_Kind

val add :
  kind: kind ->
  ?package: Utils_File.t ->
  ?predicate: Utils_File.t Utils_Predicate.t ->
  string OCamlMake_Common_Property.generator ->
  string OCamlMake_Common_Property.handle

val add_file :
  kind: kind ->
  file: Utils_File.t ->
  flags: string Utils_Iterable.t ->
  string OCamlMake_Common_Property.handle

val get : kind -> ?sep:string -> Utils_File.t -> string
