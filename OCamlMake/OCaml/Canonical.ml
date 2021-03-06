open Utils
open OCamlMake_Common

module It = Iterable
module HashSet = Container_HashSet

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
  | prefix -> Printf.sprintf "%s_%s" prefix module_name

(**
 * Returns the canonical source file (a *.ml file).
 *
 * Example:
 * - folder = "build/Some/Package"
 * - module_name = "ModuleX"
 * -> "build/Some/Package/Some_Package_ModuleX.ml" *)
let module_to_canonical_ml_file folder module_name =
  canonical_module_name folder module_name
  |> File.child folder
  |> File.with_ext "ml"

(**
 * Returns the canonical source file (a *.ml file) from a canonical module name.
 * Example: Some_Package_ModuleX -> "build/Some/Package/Some_Package_ModuleX.ml" *)
let canonical_module_to_canonical_ml_file canonical_module_name =
  begin
    match Utils.split '_' canonical_module_name with
    | [] -> Utils.fail "Utils.split should not return empty for '%c' and %S."
                       '_' canonical_module_name |> raise
    | [ _ ] -> File.parsef "%s.ml" canonical_module_name
    | module_name :: folder ->
       File.parsef "%s/%s.ml"
                   (folder |> List.rev |> It.of_list |> Utils.join "/")
                   canonical_module_name
  end |> Private.to_private

(**
 * Returns the canonical folder from a canonical "container" module name.
 * Example: Some_Package -> "build/Some/Package/" *)
let canonical_module_to_dir canonical_module_name =
  match Utils.split '_' canonical_module_name with
  | [] -> Utils.fail "Utils.split should not return empty for '%c' and %S."
                     '_' canonical_module_name |> raise
  | folder -> folder
              |> List.rev |> It.of_list |> Utils.join "/"
              |> File.parse
              |> Private.to_private

type module_naming_convention = Relative | Absolute | Package | Builtin

(** Returns:
 * a. whether the module name is canonical,
 * b. the name of the source file for the module. *)
let module_to_convention_and_dependency folder module_name =
  let module OM = OCamlMake_OCamlMake in
  let canonical_ml_file = module_to_canonical_ml_file folder module_name in
  if OM.has_rule canonical_ml_file then
    (* Module from the same package. *)
    Relative, Some canonical_ml_file
  else
    let canonical_ml_file = canonical_module_to_canonical_ml_file module_name in
    if OM.has_rule canonical_ml_file then
      (* Canonical package name used: Some_Package_ModuleX. *)
      Absolute, Some canonical_ml_file
    else
      let canonical_dir = canonical_module_to_dir module_name in
      if OM.has_rule canonical_dir then
        Package, Some canonical_dir
      else
        Builtin, None

let module_to_dependency folder module_name =
  module_to_convention_and_dependency folder module_name
  |> snd

(** Returns the private canonical file from the public source file. *)
let file_to_canonical_file file =
  module_to_canonical_ml_file
    (file |> Private.to_private |> File.parent)
    (File.basename file)
  |> File.with_ext (File.extension file)

(** Returns the public source file from the private canonical file. *)
let canonical_file_to_file file =
  match file |> File.basename |> Utils.split '_' with
  | [] -> Utils.fail "Utils.split should not return empty for '%c' and %S."
                     '_' (file |> File.basename) |> raise
  | module_name :: _ -> File.child (File.parent file) module_name
                        |> File.with_ext (File.extension file)
                        |> Private.to_public

let private_ml_file_rule target =
  let module OM = OCamlMake_OCamlMake in
  assert (Utils.dcheck (Private.is_private target)
		       "Target %s is not in the private folder (%s/...)"
		       (File.to_string target) Private.private_folder);
  let source = target |> canonical_file_to_file in
  let mli_file = source |> File.with_ext "mli" in
  let has_mli_file = lazy (File.extension source = "ml"
                           && OM.has_rule mli_file) in
  let targets = It.singleton target
  and sources =
    It.concat
      begin [ File.parent target ; source ] |> It.of_list end
      begin lazy (if Lazy.force has_mli_file
                  then It.singleton mli_file
                  else It.empty ())
            |> It.of_lazy
      end
  and command () =
    let print_relative_module_alias module_name module_source =
      Printf.sprintf "module %s = %s"
                     module_name (File.basename module_source)
    and print_package_alias module_name module_source =
      Printf.sprintf
        "module %s %s %s end"
        module_name
        (if File.extension source = "ml"
         then "= struct"
         else ": sig")
        (module_source
         |> OM.get_targets
         |> Iterable.filter (Predicate.extension "cmo")
         |> Iterable.map File.basename
         |> Iterable.map
              begin fun sub_module ->
              Printf.sprintf
                "  module %s = %s"
                (canonical_module_to_dir sub_module |> File.basename)
                sub_module
              end
         |> Utils.join " ")
    in
    let module_aliases =
      begin if Lazy.force has_mli_file
            then HashSet.union (OCamlDep.ocamldep source |> It.of_list)
                               (OCamlDep.ocamldep mli_file |> It.of_list)
                 |> HashSet.to_iterable
            else OCamlDep.ocamldep source |> It.of_list
      end
      |> It.map begin fun module_name ->
                let module_naming_convention, module_source =
                  module_to_convention_and_dependency
                    (File.parent target)
                    module_name
                in
                module_name, module_naming_convention, module_source
                end
      |> It.map
           begin fun (module_name, module_naming_convention, module_source) ->
           let open Utils.Option in
           match module_naming_convention with
           | Absolute | Builtin -> None
           | Relative -> Some (print_relative_module_alias
                                 module_name !?module_source)
           | Package -> Some (print_package_alias
                                module_name !?module_source)
           end
      |> It.filter (fun x -> x <> None)
      |> It.map (fun x -> match x with Some y -> y | None -> failwith "Impossible")
      |> Utils.join " "
    in
    begin if module_aliases = ""
          then Process.run_command
                 "cp %s %s"
                 (File.to_string source)
                 (File.to_string target)
          else Process.run_command
                 "echo \"%s\" > %s ; cat %s >> %s"
                 module_aliases
                 (File.to_string target)
                 (File.to_string source)
                 (File.to_string target)
    end |> ignore
  in
  let open OM in
  { targets ; sources ; command }

let public_ml_file_rule target =
  let module OM = OCamlMake_OCamlMake in
  assert (Utils.dcheck (Private.is_public target)
		       "Target %s is in the private folder (%s/...)"
		       (File.to_string target) Private.private_folder);
  let source = target |> file_to_canonical_file in
  let targets = It.singleton target
  and sources = if File.is_toplevel target
                then It.singleton source
                else [ File.parent target ; source ] |> It.of_list
  and command () = Process.run_command
		     "cp %s %s"
		     (File.to_string source)
		     (File.to_string target)
		   |> ignore
  in
  let open OM in
  { targets ; sources ; command }

let () =
  assert (Log.dlog "Testing Canonical");
  let open Private in
  assert
    ([ "canonical_module_name sub-package",
       (fun () ->
         canonical_module_name
           (File.parsef "%s/Some/Package" private_folder)
           "ModuleX"
         = "Some_Package_ModuleX") ;

       "canonical_module_name root",
       (fun () ->
         canonical_module_name
           (File.parse private_folder)
           "ModuleX"
         = "ModuleX") ;

       "module_to_canonical_ml_file sub-package",
       (fun () ->
         module_to_canonical_ml_file
           (File.parsef "%s/Some/Package" private_folder)
           "ModuleX"
         = File.parsef "%s/Some/Package/Some_Package_ModuleX.ml"
                       private_folder) ;

       "module_to_canonical_ml_file root",
       (fun () ->
         module_to_canonical_ml_file
           (File.parse private_folder)
           "ModuleX"
         = File.parsef "%s/ModuleX.ml" private_folder) ;

       "canonical_module_to_canonical_ml_file sub-package",
       (fun () ->
         canonical_module_to_canonical_ml_file "Some_Package_ModuleX"
         = File.parsef "%s/Some/Package/Some_Package_ModuleX.ml"
                       private_folder) ;

       "canonical_module_to_canonical_ml_file root",
       (fun () ->
         canonical_module_to_canonical_ml_file "ModuleX"
         = File.parsef "%s/ModuleX.ml" private_folder) ;
     ] |> Asserts.test)
