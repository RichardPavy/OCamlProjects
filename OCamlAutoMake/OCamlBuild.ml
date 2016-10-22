type rule = { targets: File.t Iterable.t;
	      sources: File.t Iterable.t;
	      command: unit -> unit }

 and memoized_rule = { mutable executed: bool ;
                       rule: rule }

 and rule_generator = folder: File.t -> rule_generator_result
 and rule_generator_result = {
     rules: rule Iterable.t ;
     other_generators: rule_generator Iterable.t ;
   }

let rule_generator_result
      ?(rules = Iterable.empty ())
      ?(other_generators = Iterable.empty ())
      () =
  { rules ; other_generators }

let root_rule_generators = Queue.create ()

let add_root_rule_generator rule = Queue.add rule root_rule_generators

let get_rule_name { sources ; targets } =
  Printf.sprintf
    "{%s} -> {%s}"
    (sources |> Iterable.map File.to_string |> Utils.join " ")
    (targets |> Iterable.map File.to_string |> Utils.join " ")

let memoized_rule rule =
  { executed = false ;
    rule = { rule with
             command = fun () -> Log.log "Running <%s>" (get_rule_name rule);
                                 rule.command () }
  }

let try_get_rule =
  (** Generates all the rules in a folder and indexes them by target. *)
  let aux folder =
    Log.block
      "Generating rules for folder: <%s>" (File.to_string folder)
      begin fun () ->
      let rules_by_target = Hashtbl.create 16 in
      let record_rule rule =
        let mrule = memoized_rule rule in
        mrule.rule.targets
	|> Iterable.iter
	     begin fun target ->
	     assert (Utils.dcheck (Hashtbl.mem rules_by_target target |> not)
				  "Duplicate rule for target <%s>"
				  (File.to_string target));
	     assert (Utils.dcheck (target = folder || (File.parent target) = folder)
				  "Target <%s> is not in the generated folder <%s>"
				  (File.to_string target) (File.to_string folder));
	     assert (Log.dlog "Adding rule for target <%s>" (File.to_string target));
	     Hashtbl.add rules_by_target target mrule
	     end
      in
      let rec aux generators =
	generators
	|> Iterable.map (fun rule_generator -> rule_generator ~folder)
	|> Iterable.iter (fun { rules ; other_generators } ->
	       rules |> Iterable.iter record_rule;
	       aux other_generators)
      in
      root_rule_generators |> Iterable.of_queue |> aux;
      fun target ->
      assert (Log.dlog "Getting rule for target <%s>" (File.to_string target));
      try Some (Hashtbl.find rules_by_target target)
      with Not_found -> None
      end
  in
  let aux_cached = Cache.fn ~max_size: 100 aux
  in
  fun target -> aux_cached (File.parent target) target

let get_rule target =
  match try_get_rule target with
  | Some rule -> rule
  | None -> Utils.fail "Rule not found for target <%s>"
		       (File.to_string target) |> raise

let has_rule target = try_get_rule target <> None

(** Executes a rule
    if the timestamps of the sources are after the timestamps of the targets. *)
let execute rule =
  let target_timestamp =
    rule.targets
    |> Iterable.map Timestamp.get
    |> Iterable.fold min max_float
  and source_timestamp =
    rule.sources
    |> Iterable.map Timestamp.get
    |> Iterable.fold max 0.
  in
  if target_timestamp < source_timestamp
  then begin
      rule.command ();
      rule.targets
      |> Iterable.iter Timestamp.clear;
      assert begin
	  rule.targets
	  |> Iterable.map (fun target -> target, Timestamp.get target)
	  |> Iterable.all
               begin fun (target, target_timestamp) ->
               Utils.dcheck (target_timestamp +. 0.001 > source_timestamp)
                            "Source timestamp (%f) > target timestamp (%s:%f)."
                            source_timestamp
                            (File.to_string target) target_timestamp
               end
	end
    end

(** Builds a target by recursively building all its dependencies first. *)
let rec build target =
  let mrule = get_rule target in
  if not mrule.executed
  then
    let rule = mrule.rule in
    Log.block "Building <%s>" (File.to_string target)
              begin fun () ->
              rule.sources |> Iterable.iter build;
              execute rule;
              assert (Utils.dcheck (not mrule.executed)
                                   "Cyclic dependency on <%s>?"
                                   (File.to_string target));
              mrule.executed <- true
              end
