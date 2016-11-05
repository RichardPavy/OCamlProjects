open Utils
open Container

(* #load "OCamlBuild.cma";; #directory "build";; *)

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

module It = Iterable

(** Verifies that a file has the right extension. *)
let dcheck_extension target extension =
  Utils.dcheck (target |> File.extension = extension)
	       "Unexpected *.%s target file: %s"
	       extension (File.to_string target)

let { Cache.fn = list_folder_content ;
      Cache.clear = clear_folder_content_cache } =
  Cache.make
    ~max_size: 10
    begin fun folder ->
    match Timestamp.kind folder with
    | Timestamp.Null when not (File.is_root folder) -> []
    | Timestamp.Null | Timestamp.Folder ->
       folder
       |> File.to_string
       |> Process.run_command "ls %s"
    | _ -> Utils.fail "Not a folder: %s" (File.to_string folder) |> raise
    end

(**
 * Runs ocamldep to figure out the dependencies of a file.
 * Does not include the file itself.
 * - source: an OCaml source file (either *.ml or *.mli)
 * - extension: the extension with which dependencies are returned.
 *   Can be any extension. *)
let get_dependencies extension source =
  lazy begin
      source
      |> OCamlDep.ocamldep
      |> Iterable.of_list
      |> It.map (Canonical.module_to_dependency (File.parent source))
      |> It.filter ((<>) File.root)
      |> It.map (File.with_ext extension)
    end |> It.of_lazy

(**
 * Returns all the dependencies of a source file (either *.ml or *.mli). *)
let get_transitive_dependencies extension source =
  let module LHS = LinkedHashSet in
  let deps = LHS.create () in
  let source_ext = File.extension source in
  let rec process dep =
    if LHS.mem deps dep |> not
    then begin
        dep
        |> File.with_ext source_ext
        |> get_dependencies extension
        |> It.iter process;
        LHS.add deps dep
      end
  in
  process (source |> File.with_ext extension);
  deps |> LHS.to_iterable

(** Returns all the modules that a source depends on, including system modules. *)
let get_all_modules source =
  let all_modules = HashSet.create () in
  source
  |> get_transitive_dependencies (File.extension source)
  |> It.map OCamlDep.modules
  |> It.map It.of_list
  |> It.flatten
  |> It.iter begin fun m -> if HashSet.mem all_modules m |> not
                            then HashSet.add all_modules m
             end;
  all_modules

(** Include the source file's directory if necessary. *)
let flag_include_dirs sources =
  let flags = HashSet.create () in
  sources
  |> Iterable.iter begin fun source_file ->
                   if not (File.is_toplevel source_file)
                   then let flag = "-I " ^ (source_file
                                            |> File.parent
                                            |> File.to_string)
                        in
                        if not (HashSet.mem flags flag)
                        then HashSet.add flags flag
                   end;
  HashSet.to_iterable flags |> Utils.join " "

let () =
  assert ("" = (File.parse "File.ml" |> Iterable.singleton |> flag_include_dirs));
  assert ("" = (File.parse "/File.ml" |> Iterable.singleton |> flag_include_dirs));
  assert ("-I package/folder" = (File.parse "package/folder/File.ml"
                                 |> Iterable.singleton |> flag_include_dirs))

type Flag.kind += Interface
type Flag.kind += Object
type Flag.kind += Executable

(**
 * Returns a flag to include all required packages when linking an executable.
 * - kind: flag kind namespace.
 * - source: source file to determine all transitive dependencies.
 * - target: file to which the flag should be attached to. *)
let add_package_flag =
  let module_to_package =
    [ "Lwt", "lwt" ;
      "Unix", "unix" ;
      "Str", "str" ;
      "Yojson", "yojson" ] |> It.of_list
  in fun ~kind ~source ~target ->
     Log.block
       "add_package_flag <%s -> %s>" (File.to_string source) (File.to_string target)
       begin fun () ->
       let modules = get_all_modules source in
       match module_to_package
             |> It.filter (fun (m, _) -> HashSet.mem modules m)
             |> It.map snd
             |> It.to_list
       with
       | [] -> None
       | packages ->
          let generator =
            let packages_flags =
              packages
              |> It.of_list
              |> It.map (fun package -> "-package " ^ package)
            in
            if kind = Executable
            then fun _ -> It.concat (It.singleton "-linkpkg") packages_flags
            else fun _ -> packages_flags
          in
          Some (Flag.add_file ~kind
                              ~file: target
                              ~generator)
       end

(**
 * A rule to build a *.cmi file
 * - Sources: the corresponding *.mli file
 *            all the *.cmi files of dependencies
 * - Target: the *.cmi file *)
let cmi_rule cmi_file =
  assert (dcheck_extension cmi_file "cmi");
  let mli_file = cmi_file |> File.with_ext "mli" in
  let targets = It.singleton cmi_file
  and sources =
    It.concat
      (It.singleton mli_file)
      (mli_file |> get_dependencies "cmi")
  in
  let command () =
    let handle = add_package_flag ~kind: Interface
                                  ~source: mli_file
                                  ~target: cmi_file in
    Process.run_command
      "ocamlfind ocamlc %s %s -c %s"
      (flag_include_dirs sources)
      (Flag.get Interface cmi_file)
      (File.to_string mli_file)
    |> ignore;
    (let open Utils.Option in ?>LinkedList.remove handle)
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
  in fun target ->
     assert (dcheck_extension target extension);
     let ml_file = target |> File.with_ext "ml" in
     let mli_file = target |> File.with_ext "mli" in
     let targets = It.singleton target
     and sources =
       [ It.singleton ml_file ;
	 lazy begin if OCamlMake.has_rule mli_file
                    then target |> File.with_ext "cmi" |> It.singleton
                    else It.empty ()
              end |> It.of_lazy ;
         ml_file |> get_dependencies extension ]
       |> It.of_list
       |> It.flatten
     in
     let command () =
       let handle = add_package_flag ~kind: Object
                                     ~source: ml_file
                                     ~target in
       Process.run_command
	 "ocamlfind %s %s %s -c %s"
	 compiler
         (flag_include_dirs sources)
         (Flag.get Object target)
	 (File.to_string ml_file)
       |> ignore;
       (let open Utils.Option in ?>LinkedList.remove handle)
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
  in
  fun target ->
  assert (dcheck_extension target target_extension);
  let ml_file = target |> File.with_ext "ml" in
  let targets = It.singleton target
  and sources =
    It.concat
      (ml_file |> File.with_ext object_extension |> It.singleton)
      (lazy (ml_file |> get_dependencies object_extension) |> It.of_lazy)
  in
  let command () =
    let handle = add_package_flag ~kind: Executable
                                  ~source: ml_file
                                  ~target in
    let flags = Flag.get Executable target in
    Process.run_command
      "ocamlfind %s %s %s -o %s %s"
      compiler
      (flag_include_dirs sources)
      flags
      (File.to_string target)
      (ml_file
       |> get_transitive_dependencies object_extension
       |> It.map File.to_string
       |> Utils.join " ")
    |> ignore;
    (let open Utils.Option in ?>LinkedList.remove handle)
  in
  let open OCamlMake in
  { targets ; sources ; command }

let cma_rule = make_ocaml_target_file_rule `CMA
let cmxa_rule = make_ocaml_target_file_rule `CMXA
let byte_rule = make_ocaml_target_file_rule `BYTE
let exe_rule = make_ocaml_target_file_rule `EXE

(** Generates rules under the build/... folder. *)
let ocaml_private_rules_generator ~folder =
  Log.block
    "OCaml private rules generator for <%s>" (File.to_string folder)
    begin fun () ->
    assert (Utils.dcheck (Private.is_private folder)
			 "Folder %s is not a private folder (%s/...)"
			 (File.to_string folder) Private.private_folder);
    let public_folder = Private.to_public folder in
    list_folder_content public_folder
    |> It.of_list
    |> It.map (File.child public_folder)
    |> It.map begin fun file ->
	      match File.extension file with
	      | "mli" -> let file = Canonical.file_to_canonical_file file in
                         [ Canonical.private_ml_file_rule file ;
			   cmi_rule (File.with_ext "cmi" file) ]
			 |> It.of_list
	      | "ml" -> let file = Canonical.file_to_canonical_file file in
                        [ Canonical.private_ml_file_rule file ;
			  cmo_rule (File.with_ext "cmo" file) ;
			  cmx_rule (File.with_ext "cmx" file) ;
			  cma_rule (File.with_ext "cma" file) ;
			  cmxa_rule (File.with_ext "cmxa" file) ;
			  byte_rule (File.with_ext "byte" file) ;
			  exe_rule (File.with_ext "exe" file) ]
                        |> It.of_list
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
    end

(** Generates rules for the source folders. *)
let ocaml_public_rules_generator ~folder =
  Log.block
    "OCaml public rules generator for <%s>" (File.to_string folder)
    begin fun () ->
    assert (Utils.dcheck (Private.is_public folder)
			 "Folder %s is a private folder (%s/...)"
			 (File.to_string folder) Private.private_folder);
    let folder_string = if File.is_root folder
			then ""
			else (File.to_string folder) ^ "/" in
    list_folder_content folder
    |> It.of_list
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
    end

let build_folder_rule_generator ~folder =
  assert (Utils.dcheck (File.is_root folder)
                       "The build/ folder should be generated at the root only.");
  let rules = CommonRules.folder_rule (File.parse Private.private_folder)
              |> It.singleton in
  OCamlMake.rule_generator_result ~rules ()

let ocaml_rules_generator ~folder =
  Log.block
    "OCaml rules generator for <%s>" (File.to_string folder)
    begin fun () ->
    let other_generators =
      if Private.is_private folder then
        It.singleton ocaml_private_rules_generator
      else if File.is_root folder then
        [ ocaml_public_rules_generator ;
          build_folder_rule_generator ] |> It.of_list
      else It.singleton ocaml_public_rules_generator
    in
    OCamlMake.rule_generator_result ~other_generators ()
    end
