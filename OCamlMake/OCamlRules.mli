val get_dependencies : string -> File.t -> File.t Iterable.t
val get_transitive_dependencies : string -> File.t -> File.t Iterable.t
val get_all_modules : File.t -> string HashSet.t

val dcheck_extension : File.t -> string -> bool
val flag_include_dir : File.t -> string
val add_package_flag :
  kind: Flag.kind ->
  source: File.t ->
  target: File.t ->
  string Property.handle option

type Flag.kind += Interface
type Flag.kind += Object
type Flag.kind += Executable

val cmi_rule : File.t -> OCamlMake.rule
val cmo_rule : File.t -> OCamlMake.rule
val cmx_rule : File.t -> OCamlMake.rule
val cma_rule : File.t -> OCamlMake.rule
val cmxa_rule : File.t -> OCamlMake.rule
val exe_rule : File.t -> OCamlMake.rule
val byte_rule : File.t -> OCamlMake.rule

val ocaml_private_rules_generator : OCamlMake.rule_generator
val ocaml_public_rules_generator : OCamlMake.rule_generator
val ocaml_rules_generator : OCamlMake.rule_generator
