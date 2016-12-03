(** Rules that generate scripts to build a target. *)

open Utils
open OCamlMake_Common

module It = Iterable

let bootstrap_folder_name = "make"
let bootstrap_rule_extension = ".make.sh"

(**
 * A rule to build a *.make.sh script.
 * "*.make.sh" scripts run all the commands required to
 * build a target. *)
let bootstrap_rule target =
  assert (Utils.dcheck
            (target |> File.filename |> Utils.ends_with bootstrap_rule_extension)
            "target <%s> does not have the extension <%s>"
            (File.to_string target) bootstrap_rule_extension);
  assert (Utils.dcheck
            (target |> File.parent |> File.filename = bootstrap_folder_name)
            "target <%s> is not in a folder called <%s>"
            (File.to_string target) bootstrap_folder_name);
  let targets = It.singleton target in
  let source =
    (* "x/y/z/make/target.xxx.make.sh -> x/y/z/target.xxx" *)
    File.child
      (target |> File.parent |> File.parent)
      (let filename = File.filename target in
       let bsl = String.length bootstrap_rule_extension in
       String.sub filename 0
                  (String.length filename - bsl))
  in
  let sources =
    lazy begin
        assert (Utils.dcheck
                  (not !OCamlMake.bootstrap_mode)
                  "Bootstrap mode should not be enabled.");
        assert (Log.dlog "Enabling bootstrap mode.");
        OCamlMake.bootstrap_mode := true;
        It.of_array [| source ; target |> File.parent |]
      end |> It.of_lazy
  in
  let command () =
    let ch =
      open_out_gen
        [ Open_wronly; Open_creat; Open_append; Open_trunc; Open_text ]
        0o755
        (File.to_string target)
    in
    assert (Utils.dcheck
              !OCamlMake.bootstrap_mode
              "Bootstrap mode should be enabled.");
    assert (Log.dlog "Disabling bootstrap mode.");
    OCamlMake.bootstrap_mode := false;
    Process.command_log |> It.iter (fun cmd -> output_string ch cmd;
                                               output_string ch "\n");
    close_out ch
  in
  let open OCamlMake in
  { targets ; sources ; command }

(**
 * A rule generator to build *.make.sh scripts.
 *
 * "*.make.sh" scripts build a target, record all the command lines executed
 * while doing it, and produce a script that can build a target from scratch
 * without using the OCamlMake binary.
 *
 * This is useful to distribute source code to third-parties. *)
let bootstrap_rules_generator ~folder =
  let rules =
    if Private.is_private folder then
      It.empty ()
    else if folder |> File.filename <> bootstrap_folder_name then
      let bootstrap_folder = File.child folder bootstrap_folder_name in
      if Timestamp.get bootstrap_folder < 0. then
        CommonRules.folder_rule bootstrap_folder
        |> begin fun rule ->
           { rule with OCamlMake.command =
                         begin fun () ->
                         rule.OCamlMake.command
                         |> Utils.toggle Process.enable_command_log false
                         end }
           end
        |> It.singleton
      else
        It.empty ()
    else
      folder
      |> File.parent
      |> OCamlMake.get_targets
      |> It.map begin fun target ->
                File.child
                  (File.child
                     (File.parent target)
                     bootstrap_folder_name)
                  (File.filename target ^ bootstrap_rule_extension)
                end
      |> It.map bootstrap_rule
  in
  OCamlMake.rule_generator_result ~rules ()
