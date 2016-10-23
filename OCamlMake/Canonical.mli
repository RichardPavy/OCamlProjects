val canonical_module_name : File.t -> string -> string
val module_to_canonical_ml_file : File.t -> string -> File.t
val canonical_module_to_canonical_ml_file : string -> File.t
val module_to_dependency : File.t -> string -> File.t
val file_to_canonical_file : File.t -> File.t
val canonical_file_to_file : File.t -> File.t
val private_ml_file_rule : File.t -> OCamlMake.rule
val public_ml_file_rule : File.t -> OCamlMake.rule
