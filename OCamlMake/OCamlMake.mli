type rule = {
    targets : Utils_File.t Utils_Iterable.t ;
    sources : Utils_File.t Utils_Iterable.t ;
    command : unit -> unit ;
  }

 and rule_generator = folder:Utils_File.t -> rule_generator_result

 and rule_generator_result = {
     rules : rule Utils_Iterable.t ;
     other_generators : rule_generator Utils_Iterable.t ;
   }

val rule_generator_result :
  ?rules: rule Utils_Iterable.t ->
  ?other_generators: rule_generator Utils_Iterable.t ->
  unit -> rule_generator_result

val add_root_rule_generator : rule_generator -> unit

val has_rule : Utils_File.t -> bool
val get_targets : Utils_File.t -> Utils_File.t Utils_Iterable.t
val build : Utils_File.t -> unit
