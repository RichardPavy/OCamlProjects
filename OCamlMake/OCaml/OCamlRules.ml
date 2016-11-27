(**
 * How dependencies work across folders.
 *
 * 1. Example:
 * ./Main.ml
 * ./Server.ml
 * ./Worker.ml
 * ./Protocol/Rpc.ml
 * ./Protocol/JsonProtocol.ml
 * ./Protocol/HtmlProtocol.ml
 * ./Protocol/NativeProtocol.ml
 * ./Protocol/Html/Parser.ml
 * ./Protocol/Html/HtmlEntities.ml
 * ./Utils/Log.ml
 * ./Utils/UrlEncode.ml
 *
 *
 * 2. How a module can be referenced:
 *
 * 2.a) by relative name, for modules in the same package
 * This means that for any module using other modules in the same package, the
 * following declarations are added at the top:
 * file <Protocol/Rpc.ml> depends on module names 'JsonProtocol' and 'HtmlProtocol',
 * file <build/Protocol/Rpc.ml> will be like:
 *   <<<
 *   module JsonProtocol = Protocol_JsonProtocol
 *   module HtmlProtocol = Protocol_HtmlProtocol
 *   ... rest of the Protocol/Rpc.ml file
 *   >>>
 *
 * 2.b) by absolute name: Protocol_Html_HtmlEntities
 *
 * 2.c) using folders-as-packages: Protocol.Html.Parser, Protocol_Html.Parser
 * !! building folders-as-packages create lots of dependencies !!
 * file <Protocol.ml> is generated as:
 *   <<<
 *   module Rpc = Protocol_Rpc
 *   module JsonProtocol = Protocol_JsonProtocol
 *   module HtmlProtocol = Protocol_HtmlProtocol
 *   module NativeProtocol = Protocol_NativeProtocol
 *   module Html = struct
 *       module Parser = Protocol_Html_Parser
 *       module HtmlEntities = Protocol_Html_HtmlEntities
 *     end
 *   >>>
 *
 * file <Protocol_Html.ml> is generated as:
 *   <<<
 *   module Parser = Protocol_Html_Parser
 *   module HtmlEntities = Protocol_Html_HtmlEntities
 *   >>>
 *)

open Utils
open OCamlMake_Common

module It = Iterable
module OCamlMake = OCamlMake_OCamlMake
module CommonRules = OCamlMake_CommonRules

(**
 * A rule to build a *.cmi file
 * - Sources: the corresponding *.mli file
 *            all the *.cmi files of dependencies
 * - Target: the *.cmi file *)
let cmi_rule cmi_file =
  assert (Asserts.extension cmi_file "cmi");
  let kind = Flags.Interface in
  let mli_file = cmi_file |> File.with_ext "mli" in
  let targets = It.singleton cmi_file
  and sources =
    It.concat
      (It.singleton mli_file)
      (mli_file |> Dependency.get "cmi")
  in
  let command () =
    [ Flags.packages ~kind
                     ~source: mli_file
                     ~target: cmi_file ;
      Flags.include_dirs ~kind
                         ~target: cmi_file
                         ~sources ;
    ] |> Property.process
           begin fun () ->
           Process.run_command
             "ocamlfind ocamlc %s -c %s"
             (Flag.get kind cmi_file)
             (File.to_string mli_file)
           |> ignore
           end
  in
  let open OCamlMake in
  { targets ; sources ; command }

(**
 * A rule to build *.cmo and *.cmx files
 * - Sources:
 *   - the corresponding *.ml file,
 *   - and *.cmi files,
 *   - all the *.cmo files of dependencies.
 * - Target: the *.cmo or *.cmx file *)
let make_compiled_object_rule kind =
  let extension, compiler = match kind with
    | `Bytecode -> "cmo", "ocamlc"
    | `Native -> "cmx", "ocamlopt"
  and kind = Flags.Object
  in
  fun target ->
  assert (Asserts.extension target extension);
  let ml_file = target |> File.with_ext "ml" in
  let mli_file = target |> File.with_ext "mli" in
  let targets = It.singleton target
  and sources =
    [ It.singleton ml_file ;
      lazy begin if OCamlMake.has_rule mli_file
                 then target |> File.with_ext "cmi" |> It.singleton
                 else It.empty ()
           end |> It.of_lazy ;
      ml_file |> Dependency.get extension ]
    |> It.of_list
    |> It.flatten
  in
  let command () =
    [ Flags.packages ~kind ~source: ml_file ~target ;
      Flags.include_dirs ~kind ~target ~sources ;
    ] |> Property.process
           begin fun () ->
           Process.run_command
             "ocamlfind %s %s -c %s"
             compiler
             (Flag.get kind target)
             (File.to_string ml_file)
           |> ignore
           end
  in
  let open OCamlMake in
  { targets ; sources ; command }

let cmo_rule = make_compiled_object_rule `Bytecode
let cmx_rule = make_compiled_object_rule `Native

let make_ocaml_target_file_rule kind =
  let target_extension, object_extension, compiler =
    match kind with
    | `CMA -> "cma", "cmo", "ocamlc -a"
    | `CMXA -> "cmxa", "cmx", "ocamlopt -a"
    | `BYTE -> "byte", "cmo", "ocamlc"
    | `EXE -> "exe", "cmx", "ocamlopt"
  and kind = Flags.Executable
  in
  fun target ->
  assert (Asserts.extension target target_extension);
  let ml_file = target |> File.with_ext "ml" in
  let targets = It.singleton target
  and sources =
    It.concat
      (ml_file |> File.with_ext object_extension |> It.singleton)
      (lazy (ml_file |> Dependency.get object_extension) |> It.of_lazy)
  in
  let command () =
    [ Flags.packages ~kind ~source: ml_file ~target ;
      Flags.include_dirs ~kind ~target ~sources ;
    ] |> Property.process
           begin fun () ->
           Process.run_command
             "ocamlfind %s %s -o %s %s"
             compiler
             (Flag.get kind target)
             (File.to_string target)
             (ml_file |> Dependency.get_transitive object_extension
              |> It.map File.to_string
              |> Utils.join " ")
           |> ignore
           end
  in
  let open OCamlMake in
  { targets ; sources ; command }

let cma_rule = make_ocaml_target_file_rule `CMA
let cmxa_rule = make_ocaml_target_file_rule `CMXA
let byte_rule = make_ocaml_target_file_rule `BYTE
let exe_rule = make_ocaml_target_file_rule `EXE

(** Generates rules under the build/... folder. *)
let ocaml_private_rules_generator ~folder =
  begin fun () ->
  assert (Utils.dcheck (Private.is_private folder)
		       "Folder %s is not a private folder (%s/...)"
		       (File.to_string folder) Private.private_folder);
  let public_folder = Private.to_public folder in
  FolderContent.list public_folder
  |> It.of_array
  |> It.map (File.child public_folder)
  |> It.map begin fun file ->
	    match File.extension file with
	    | "mli" -> let file = Canonical.file_to_canonical_file file in
                       [ Canonical.private_ml_file_rule file ;
			 cmi_rule (File.with_ext "cmi" file) ;
		       ] |> It.of_list
	    | "ml" -> let file = Canonical.file_to_canonical_file file in
                      [ Canonical.private_ml_file_rule file ;
			cmo_rule (File.with_ext "cmo" file) ;
			cmx_rule (File.with_ext "cmx" file) ;
			cma_rule (File.with_ext "cma" file) ;
			cmxa_rule (File.with_ext "cmxa" file) ;
			byte_rule (File.with_ext "byte" file) ;
			exe_rule (File.with_ext "exe" file) ;
                      ] |> It.of_list
            | _ when (File.is_root file |> not)
                     && (Timestamp.kind file = Timestamp.Folder)
                     && (File.to_string file <> Private.private_folder) ->
               file |> Private.to_private
               |> CommonRules.folder_rule
               |> It.singleton
	    | _ -> It.empty ()
	    end
  |> It.flatten
  |> fun rules -> OCamlMake.rule_generator_result ~rules ()
  end |> Log.block "OCaml private rules generator for <%s>"
                   (File.to_string folder)

(** Generates rules for the source folders. *)
let ocaml_public_rules_generator ~folder =
  begin fun () ->
  assert (Utils.dcheck (Private.is_public folder)
		       "Folder %s is a private folder (%s/...)"
		       (File.to_string folder) Private.private_folder);
  let folder_string = if File.is_root folder
		      then ""
		      else (File.to_string folder) ^ "/" in
  FolderContent.list folder
  |> It.of_array
  |> (if File.is_root folder
      then It.filter (fun file -> file <> Private.private_folder)
      else fun it -> it)
  |> It.map (fun file -> File.parsef "%s%s" folder_string file)
  |> It.map begin fun file ->
	    begin match File.extension file with
	    | "ml" -> [ CommonRules.noop_rule file;
                        Canonical.public_ml_file_rule (File.with_ext "cma" file) ;
			Canonical.public_ml_file_rule (File.with_ext "cmxa" file) ;
			Canonical.public_ml_file_rule (File.with_ext "byte" file) ;
			Canonical.public_ml_file_rule (File.with_ext "exe" file) ]
	    | "mli" -> [ CommonRules.noop_rule file ]
            | "cma" | "cmxa" | "byte" | "exe" ->
               if (file |> (File.with_ext "ml") |> Timestamp.kind) = Timestamp.Null
               then [ CommonRules.noop_rule file ]
               else []
            | _ -> [ CommonRules.noop_rule file ]
            end |> It.of_list
	    end
  |> It.flatten
  |> fun rules -> OCamlMake.rule_generator_result ~rules ()
  end |> Log.block "OCaml public rules generator for <%s>"
                   (File.to_string folder)

let build_folder_rule_generator ~folder =
  assert (Utils.dcheck (File.is_root folder)
                       "The build/ folder should be generated at the root only.");
  let rules = CommonRules.folder_rule (File.parse Private.private_folder)
              |> It.singleton in
  OCamlMake.rule_generator_result ~rules ()

let ocamldoc_folder_name = "doc"

let ocamldoc_rule_generator ~folder =
  let rules =
    if folder |> File.filename <> ocamldoc_folder_name then
      CommonRules.folder_rule (File.child folder ocamldoc_folder_name)
      |> It.singleton
    else
      let folder_string = File.to_string folder in
      let targets = File.child folder "index.html" |> It.singleton
      and sources = OCamlMake.get_targets (File.parent folder)
                    |> It.filter
                         (let open Predicate in
                          let open Predicate.Infix in
                          extension "ml" ||$ extension "mli")
                    |> It.to_array |> It.of_array
      in
      let command () =
        Process.run_command "rm -rf %s" folder_string |> ignore;
        Process.run_command "mkdir %s" folder_string |> ignore;
        Process.run_command "ocamlfind ocamldoc -html -keep-code -all-params -colorize-code -d %s %s"
                            folder_string
                            (sources
                             |> It.map File.to_string
                             |> Utils.join " ")
        |> ignore;
      in
      let open OCamlMake in
      { targets ; sources ; command }
      |> It.singleton
  in
  OCamlMake.rule_generator_result ~rules ()

let ocaml_rules_generator ~folder =
  begin fun () ->
  let other_generators =
    begin
      if Private.is_private folder then
        [ ocaml_private_rules_generator ;
          ocamldoc_rule_generator ]
      else if File.is_root folder then
        [ ocaml_public_rules_generator ;
          build_folder_rule_generator ;
          ocamldoc_rule_generator ]
      else
        [ ocaml_public_rules_generator ;
          ocamldoc_rule_generator ]
    end |> It.of_list
  in
  OCamlMake.rule_generator_result ~other_generators ()
  end |> Log.block "OCaml rules generator for <%s>"
                   (File.to_string folder)
