type kind = ..

val add :
  kind: kind ->
  ?package: File.t ->
  ?predicate: File.t Predicate.t ->
  string Property.generator ->
  string Property.handle

val add_file :
  kind: kind ->
  file: File.t ->
  generator: string Property.generator ->
  string Property.handle

val get : kind -> ?sep:string -> File.t -> string
