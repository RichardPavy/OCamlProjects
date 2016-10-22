open OCamlBuild
module It = Iterable
module Private = CommonRules.Private

(**
 * Returns the canonical module name.
 *
 * Example:
 * - folder = "build/Some/Package"
 * - module_name = "ModuleX"
 * -> "Some_Package_ModuleX" *)
let canonical_module_name folder module_name =
  match folder
        |> Private.to_public
        |> File.to_string
        |> String.map (function '/' -> '_' | c -> c)
  with
  | "" -> module_name
  | s -> s ^ "_" ^ module_name

(**
 * Returns the canonical source file (a *.ml file).
 *
 * Example:
 * - folder = "build/Some/Package"
 * - module_name = "ModuleX"
 * -> "build/Some/Package/Some_Package_ModuleX.ml" *)
let module_to_canonical_ml_file folder module_name =
  File.child folder ((canonical_module_name folder module_name) ^ ".ml")

let () =
  let open Private in
  assert (canonical_module_name
            (File.parsef "%s/Some/Package" private_folder)
            "ModuleX"
          = "Some_Package_ModuleX");
  assert (canonical_module_name
            (File.parse private_folder)
            "ModuleX"
          = "ModuleX");
  assert (module_to_canonical_ml_file
            (File.parsef "%s/Some/Package" private_folder)
            "ModuleX"
          = File.parsef "%s/Some/Package/Some_Package_ModuleX.ml" private_folder);
  assert (module_to_canonical_ml_file
            (File.parse private_folder)
            "ModuleX"
          = File.parsef "%s/ModuleX.ml" private_folder)

(**
 * Returns the canonical source file (a *.ml file) from a canonical module name.
 * Example: Some_Package_ModuleX -> "build/Some/Package/Some_Package_ModuleX.ml" *)
let canonical_module_to_canonical_ml_file canonical_module_name =
  Private.to_private
    begin
      match Utils.split '_' canonical_module_name with
      | [] -> Utils.fail "Utils.split should not return empty for '%c' and %S."
                         '_' canonical_module_name |> raise
      | [ _ ] -> File.parsef "%s.ml" canonical_module_name
      | module_name :: folder ->
         File.parsef "%s/%s.ml"
                     (folder |> List.rev |> It.of_list |> Utils.join "/")
                     canonical_module_name
    end

let () =
  let open Private in
  assert (canonical_module_to_canonical_ml_file "ModuleX"
          = File.parsef "%s/ModuleX.ml" private_folder);
  assert (canonical_module_to_canonical_ml_file "Some_Package_ModuleX"
          = File.parsef "%s/Some/Package/Some_Package_ModuleX.ml" private_folder)

type module_naming_convention = Relative | Absolute

(** Returns:
 * a. whether the module name is canonical,
 * b. the name of the source file for the module. *)
let module_to_dependency folder module_name =
  let canonical_ml_file = module_to_canonical_ml_file folder module_name in
  if OCamlBuild.has_rule canonical_ml_file then
    (* Module from the same package. *)
    Relative, canonical_ml_file
  else
    let canonical_ml_file = canonical_module_to_canonical_ml_file module_name in
    if OCamlBuild.has_rule canonical_ml_file then
      (* Canonical package name used: Some_Package_ModuleX. *)
      Absolute, canonical_ml_file
    else
      Utils.fail "Unable to resolve filename for dependency <%s>"
                 module_name |> raise

let file_to_canonical_file file =
  module_to_canonical_ml_file
    (File.parent file)
    (File.basename file)
  |> File.with_ext (File.extension file)

let canonical_file_to_file file =
  match file |> File.basename |> Utils.split '_' with
  | [] -> Utils.fail "Utils.split should not return empty for '%c' and %S."
                     '_' (file |> File.basename) |> raise
  | module_name :: _ -> File.child (File.parent file) module_name
                        |> File.with_ext (File.extension file)

let private_ml_file_rule target =
  assert (Utils.dcheck (Private.is_private target)
		       "Target %s is not in the private folder (%s/...)"
		       (File.to_string target) Private.private_folder);
  let source = target |> canonical_file_to_file |> Private.to_public in
  let mli_file = source |> File.with_ext "mli" in
  let has_mli_file () = File.extension source = "ml" && OCamlBuild.has_rule mli_file in
  let targets = It.singleton target
  and sources =
    It.concat
      ([ File.parent target ; source ] |> It.of_list)
      (lazy (if has_mli_file ()
             then It.singleton mli_file
             else It.empty ())
       |> It.of_lazy)
  and command () =
    let module_aliases =
      (if has_mli_file ()
       then HashSet.union (OCamlDep.ocamldep source |> It.of_list)
                          (OCamlDep.ocamldep mli_file |> It.of_list)
            |> HashSet.to_iterable
       else OCamlDep.ocamldep source |> It.of_list)
      |> It.map begin fun module_name ->
                let module_naming_convention, module_source =
                  module_to_dependency (File.parent target) module_name
                in
                module_name, module_naming_convention, module_source
                end
      |> It.filter begin fun (_, module_naming_convention, _) ->
                   module_naming_convention = Relative
                   end
      |> It.map begin fun (module_name, _, module_source) ->
                Printf.sprintf "module %s = %s"
                               module_name (File.basename module_source)
                end
      |> Utils.join "\n"
    in
    Process.run_command
      "echo \"%s\" > %s ; cat %s >> %s"
      module_aliases
      (File.to_string target)
      (File.to_string source)
      (File.to_string target)
    |> ignore
  in { targets ; sources ; command }

let public_ml_file_rule target =
  assert (Utils.dcheck (Private.is_public target)
		       "Target %s is in the private folder (%s/...)"
		       (File.to_string target) Private.private_folder);
  let source = target |> Private.to_private |> file_to_canonical_file in
  let targets = It.singleton target
  and sources = if File.is_toplevel target
                then It.singleton source
                else [ File.parent target ; source ] |> It.of_list
  and command () = Process.run_command
		     "cp %s %s"
		     (File.to_string source)
		     (File.to_string target)
		   |> ignore
  in { targets ; sources ; command }
