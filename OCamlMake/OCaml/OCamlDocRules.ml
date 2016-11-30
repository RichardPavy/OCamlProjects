open Utils
open OCamlMake_Common

module It = Iterable
module OCamlMake = OCamlMake_OCamlMake
module CommonRules = OCamlMake_CommonRules

type Flag.kind += OCamlDoc

let ocamldoc_folder_name = "doc"

let ocamldoc_rule ml_file =
  begin fun () ->
  assert (Asserts.extension ml_file "ml");
  let ocamldoc_folder =
    File.child (File.parent ml_file) ocamldoc_folder_name in
  let ocamldoc_folder_string = File.to_string ocamldoc_folder in
  let private_ml_file = Canonical.file_to_canonical_file ml_file in
  let target = File.child ocamldoc_folder (File.basename ml_file ^ ".html") in
  let targets = target |> It.singleton
  and sources =
    It.concat
      (private_ml_file |> File.with_ext "exe" |> It.singleton)
      (lazy (private_ml_file |> Dependency.get_transitive "mli") |> It.of_lazy)
    |> It.filter OCamlMake_OCamlMake.has_rule
  and kind = OCamlDoc
  in
  let command () =
    begin fun () ->
    Process.run_command "rm -rf %s" ocamldoc_folder_string |> ignore;
    Process.run_command "mkdir -p %s" ocamldoc_folder_string |> ignore;
    [ Flags.packages ~kind ~source: private_ml_file ~target ;
      Flags.include_dirs ~kind ~target ~sources ;
    ] |> Property.process
           begin fun () ->
           Process.run_command
             "ocamlfind ocamldoc -html -colorize-code -m A -stars %s -d %s %s"
             (Flag.get kind target)
             ocamldoc_folder_string
             (It.concat
                (private_ml_file |> Dependency.get_transitive "mli")
                (private_ml_file |> Dependency.get_transitive "ml")
              |> It.map File.to_string
              |> Utils.join " ")
           |> ignore
           end
    end |> Log.block "OCamlDoc rule command for <%s>"
                     (File.to_string ml_file)
  in
  let open OCamlMake in
  { targets ; sources ; command }
  end |> Log.block "OCamlDoc rule generator for <%s>"
                   (File.to_string ml_file)

let ocamldoc_rules_generator ~folder =
  begin fun () ->
  let rules =
    if Private.is_public folder then
      if folder |> File.filename <> ocamldoc_folder_name then
        let ocamldoc_folder = File.child folder ocamldoc_folder_name in
        if Timestamp.get ocamldoc_folder < 0. then
          CommonRules.folder_rule ocamldoc_folder
          |> It.singleton
        else
          It.empty ()
      else
        OCamlMake.get_targets (File.parent folder)
        |> It.filter (Predicate.extension "ml")
        |> It.map ocamldoc_rule
    else
      It.empty ()
  in
  OCamlMake.rule_generator_result ~rules ()
  end |> Log.block "OCamlDoc rules generator for <%s>"
                   (File.to_string folder)
