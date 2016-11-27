type rule = {
    targets : Utils.File.t Utils.Iterable.t ;
    sources : Utils.File.t Utils.Iterable.t ;
    command : unit -> unit ;
  }

 and rule_generator = folder:Utils.File.t -> rule_generator_result

 and rule_generator_result = {
     rules : rule Utils.Iterable.t ;
     other_generators : rule_generator Utils.Iterable.t ;
   }

val rule_generator_result :
  ?rules: rule Utils.Iterable.t ->
  ?other_generators: rule_generator Utils.Iterable.t ->
  unit -> rule_generator_result

val add_root_rule_generator : rule_generator -> unit

val has_rule : Utils.File.t -> bool
val get_targets : Utils.File.t -> Utils.File.t Utils.Iterable.t
val build : Utils.File.t -> unit
