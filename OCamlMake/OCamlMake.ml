open OCamlMake_Common
open Utils

module It = Iterable

type rule = { targets: File.t It.t;
	      sources: File.t It.t;
	      command: unit -> unit }

 and memoized_rule = { mutable executed: bool ;
                       rule: rule }

 and rule_generator = folder: File.t -> rule_generator_result
 and rule_generator_result = {
     rules: rule It.t ;
     other_generators: rule_generator It.t ;
   }

let rule_generator_result
      ?(rules = It.empty ())
      ?(other_generators = It.empty ())
      () =
  { rules ; other_generators }

let root_rule_generators = Queue.create ()

let add_root_rule_generator rule = Queue.add rule root_rule_generators

let get_rule_name { sources ; targets } =
  Printf.sprintf
    "{%s} -> {%s}"
    (sources |> It.map File.to_string |> Utils.join " ")
    (targets |> It.map File.to_string |> Utils.join " ")

let memoized_rule rule =
  { executed = false ;
    rule = { rule with
             command = fun () -> Log.log "Running <%s>" (get_rule_name rule);
                                 rule.command () }
  }

(** Generates all the rules in a folder and indexes them by target. *)
let get_rules_nocache folder =
  begin fun () ->
  let rules_by_target = Hashtbl.create 16 in
  let record_rule rule =
    let mrule = memoized_rule rule in
    mrule.rule.targets
    |> It.iter
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
    |> It.map (fun rule_generator -> rule_generator ~folder)
    |> It.iter (fun { rules ; other_generators } ->
	   rules |> It.iter record_rule;
	   aux other_generators)
  in
  root_rule_generators |> It.of_queue |> aux;
  rules_by_target
  end |> Log.block "Generating rules for folder: <%s>"
                   (File.to_string folder)

let get_rules = Cache.fn ~max_size: 100 get_rules_nocache

let get_targets folder =
  folder
  |> get_rules
  |> It.of_hashtbl
  |> It.map fst

let try_get_rule target =
  assert (Log.dlog "Getting rule for target <%s>" (File.to_string target));
  try Some (Hashtbl.find (target |> File.parent |> get_rules) target)
  with Not_found -> None

let get_rule target =
  match try_get_rule target with
  | Some rule -> rule
  | None -> Utils.fail "Rule not found for target <%s>"
		       (File.to_string target) |> raise

let has_rule target = try_get_rule target <> None

let bootstrap_mode = ref false

(** Executes a rule
    if the timestamps of the sources are after the timestamps of the targets. *)
let execute rule =
  let target_timestamp =
    rule.targets
    |> It.map Timestamp.get
    |> It.fold min max_float
  and source_timestamp =
    rule.sources
    |> It.map Timestamp.get
    |> It.fold max 0.
  in
  if target_timestamp < source_timestamp
  then begin
      assert (Utils.dcheck
                (not !Process.enable_command_log)
                "Command log should not be enabled.");
      rule.command
      |> Utils.toggle
           Process.enable_command_log
           !bootstrap_mode;
      assert (Utils.dcheck
                (not !Process.enable_command_log)
                "Command log should not be enabled.");
      rule.targets |> It.iter Timestamp.clear;
      assert begin
	  rule.targets
	  |> It.map (fun target -> target, Timestamp.get target)
	  |> It.all
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
    begin fun () ->
    rule.sources |> It.iter build;
    execute rule;
    assert (Utils.dcheck (not mrule.executed)
                         "Cyclic dependency on <%s>?"
                         (File.to_string target));
    mrule.executed <- true
    end |> Log.block "Building <%s>" (File.to_string target)
