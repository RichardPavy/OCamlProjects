type kind = ..

val add :
  kind: kind ->
  ?package: Utils.File.t ->
  ?predicate: Utils.File.t Utils.Predicate.t ->
  string Property.generator ->
  string Property.handle

val add_file :
  kind: kind ->
  file: Utils.File.t ->
  generator: string Property.generator ->
  string Property.handle

val get : kind -> ?sep:string -> Utils.File.t -> string
