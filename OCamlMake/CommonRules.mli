val noop_rule : Utils_File.t -> OCamlMake_OCamlMake.rule
val folder_rule : Utils_File.t -> OCamlMake_OCamlMake.rule
val private_file_rule : Utils_File.t -> OCamlMake_OCamlMake.rule
val public_file_rule : Utils_File.t -> OCamlMake_OCamlMake.rule
val private_folder_rules_generator :
  folder: Utils.File.t ->
  OCamlMake_OCamlMake.rule_generator_result
val build_folder_rules_generator :
  folder: Utils.File.t ->
  OCamlMake_OCamlMake.rule_generator_result
