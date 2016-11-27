val cmi_rule : Utils.File.t -> OCamlMake_OCamlMake.rule
val cmo_rule : Utils.File.t -> OCamlMake_OCamlMake.rule
val cmx_rule : Utils.File.t -> OCamlMake_OCamlMake.rule
val cma_rule : Utils.File.t -> OCamlMake_OCamlMake.rule
val cmxa_rule : Utils.File.t -> OCamlMake_OCamlMake.rule
val exe_rule : Utils.File.t -> OCamlMake_OCamlMake.rule
val byte_rule : Utils.File.t -> OCamlMake_OCamlMake.rule

val ocaml_private_rules_generator : OCamlMake_OCamlMake.rule_generator
val ocaml_public_rules_generator : OCamlMake_OCamlMake.rule_generator
val ocaml_rules_generator : OCamlMake_OCamlMake.rule_generator
