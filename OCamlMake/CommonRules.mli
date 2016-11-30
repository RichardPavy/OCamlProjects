val noop_rule : Utils_File.t -> OCamlMake.rule
val folder_rule : Utils_File.t -> OCamlMake.rule
val private_file_rule : Utils_File.t -> OCamlMake.rule
val public_file_rule : Utils_File.t -> OCamlMake.rule
val private_folder_rules_generator :
  folder: Utils.File.t ->
  OCamlMake.rule_generator_result
val build_folder_rules_generator :
  folder: Utils.File.t ->
  OCamlMake.rule_generator_result
