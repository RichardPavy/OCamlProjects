val get_dependencies :
  string ->
  Utils_File.t ->
  Utils_File.t Utils_Iterable.t
val get_transitive_dependencies :
  string ->
  Utils_File.t ->
  Utils_File.t Utils_Iterable.t
val get_all_modules : Utils_File.t -> string Container_HashSet.t

val dcheck_extension : Utils_File.t -> string -> bool
val flag_include_dirs : Utils_File.t Utils_Iterable.t -> string
val add_package_flag :
  kind: Flag.kind ->
  source: Utils_File.t ->
  target: Utils_File.t ->
  string Property.handle option

type Flag.kind += Interface
type Flag.kind += Object
type Flag.kind += Executable

val cmi_rule : Utils_File.t -> OCamlMake.rule
val cmo_rule : Utils_File.t -> OCamlMake.rule
val cmx_rule : Utils_File.t -> OCamlMake.rule
val cma_rule : Utils_File.t -> OCamlMake.rule
val cmxa_rule : Utils_File.t -> OCamlMake.rule
val exe_rule : Utils_File.t -> OCamlMake.rule
val byte_rule : Utils_File.t -> OCamlMake.rule

val ocaml_private_rules_generator : OCamlMake.rule_generator
val ocaml_public_rules_generator : OCamlMake.rule_generator
val ocaml_rules_generator : OCamlMake.rule_generator
