type OCamlMake_Common_Flag.kind += Interface
type OCamlMake_Common_Flag.kind += Object
type OCamlMake_Common_Flag.kind += Executable

val include_dirs :
  kind: OCamlMake_Common_Flag.kind ->
  target: Utils.File.t ->
  sources: Utils.File.t Utils.Iterable.t ->
  string OCamlMake_Common_Property.handle

val packages :
  kind: OCamlMake_Common_Flag.kind ->
  source: Utils.File.t ->
  target: Utils.File.t ->
  string OCamlMake_Common_Property.handle
