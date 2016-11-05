type kind = ..

val add :
  kind: kind ->
  ?package: Utils_File.t ->
  ?predicate: Utils_File.t Utils_Predicate.t ->
  string Property.generator ->
  string Property.handle

val add_file :
  kind: kind ->
  file: Utils_File.t ->
  generator: string Property.generator ->
  string Property.handle

val get : kind -> ?sep:string -> Utils_File.t -> string
