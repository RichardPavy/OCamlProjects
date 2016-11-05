val canonical_module_name : Utils_File.t -> string -> string
val module_to_canonical_ml_file : Utils_File.t -> string -> Utils_File.t
val canonical_module_to_canonical_ml_file : string -> Utils_File.t
val module_to_dependency : Utils_File.t -> string -> Utils_File.t
val file_to_canonical_file : Utils_File.t -> Utils_File.t
val canonical_file_to_file : Utils_File.t -> Utils_File.t
val private_ml_file_rule : Utils_File.t -> OCamlMake.rule
val public_ml_file_rule : Utils_File.t -> OCamlMake.rule
