val canonical_module_name : Utils.File.t -> string -> string
val module_to_canonical_ml_file : Utils.File.t -> string -> Utils.File.t
val canonical_module_to_canonical_ml_file : string -> Utils.File.t
val module_to_dependency : Utils.File.t -> string -> Utils.File.t
val file_to_canonical_file : Utils.File.t -> Utils.File.t
val canonical_file_to_file : Utils.File.t -> Utils.File.t
val private_ml_file_rule : Utils.File.t -> OCamlMake_OCamlMake.rule
val public_ml_file_rule : Utils.File.t -> OCamlMake_OCamlMake.rule
