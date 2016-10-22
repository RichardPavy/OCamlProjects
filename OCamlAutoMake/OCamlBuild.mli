type rule = {
    targets : File.t Iterable.t ;
    sources : File.t Iterable.t ;
    command : unit -> unit ;
  }

 and rule_generator = folder:File.t -> rule_generator_result

 and rule_generator_result = {
     rules : rule Iterable.t ;
     other_generators : rule_generator Iterable.t ;
   }

val rule_generator_result :
  ?rules: rule Iterable.t ->
  ?other_generators: rule_generator Iterable.t ->
  unit -> rule_generator_result

val add_root_rule_generator : rule_generator -> unit

val has_rule : File.t -> bool
val build : File.t -> unit
