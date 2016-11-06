module Config = OCamlBuild.Config
module Dependency = OCamlBuild.Dependency
module File = OCamlBuild.File
module Iterable = OCamlBuild.Iterable
module Predicate = OCamlBuild.Predicate
module Process = OCamlBuild.Process
module Timestamp = OCamlBuild.Timestamp
module Utils = OCamlBuild.Utils

(** Root folder with all the static stuff. *)
let static_dir = "Static"

(** Compiler that translates static source files into OCaml modules. *)
let static_file_compiler = static_dir ^ "/Compiler/MakeStaticFile.exe" |> File.parse

(** Folder that contains all the static source files. *)
let static_files_dir = static_dir ^ "/Files"

(** Module that contains a hash table with all the source files.
    (base name without .cmo or .cmx extension) *)
let static_repo = static_dir ^ "/Library/File"

(** Library into which all the static source files and the repository library are built.
    (base name without .cma or .cmxa extension) *)
let static_files_lib = static_dir ^ "/Static" |> File.parse

(** List of file extensions that can be included as static files.
    Basically, this should exclude OCaml files. *)
let valid_static_file_extensions = [ "js" ]

(** Returns whether the given file is a valid static source file. *)
let is_valid_static_file =
  let l = valid_static_file_extensions in
  let h = Hashtbl.create (List.length l) in
  List.iter (fun ext -> Hashtbl.replace h ext ()) l;
  let aux file = Hashtbl.mem h (File.extension file)
  in aux

(** Returns all the static source files in the given dir. *)
let read_static_files_dir =
  let read_dir_nocache dir =
    let handle = Unix.opendir dir in
    let rec aux accu =
      begin
	match Unix.readdir handle with
	| file ->
	   let file = File.parse (dir ^ "/" ^ file) in
	   if is_valid_static_file file
	   then aux (file :: accu)
	   else aux accu
	| exception End_of_file -> accu
      end
    in aux []
  in
  OCamlBuild.Cache.fn read_dir_nocache

(** Returns the source file for the given static target. *)
let get_static_source_file static_target =
  let base = static_target |> File.base in
  try List.find
	(fun file -> File.base static_target = base)
	(read_static_files_dir static_files_dir)
  with Not_found -> "Static resource file not found for: " ^ (File.to_string static_target) |> failwith

(** Returns the list of modules that the given target depends on. *)
let get_dependent_modules target =
  let ml_file = target |> match File.extension target with
			  | "cmx" | "cmo" -> File.with_ext "ml"
			  | "cmi" -> File.with_ext "mli"
			  | _ -> "Not an OCaml file: " ^ (File.to_string target) |> failwith
  in
  let parse_dep_line line =
    let target, dependencies =
      let start = 1 + String.index line ':' in
      let deps_substr = String.sub line start (String.length line - start) in
      (String.sub line 0 (start - 1) |> String.trim |> File.parse),
      (Str.split (Str.regexp "[ \r\n\t]+") deps_substr)
    in
    target, dependencies
  in
  match
    Process.run_command_cached
      ~command: (Printf.sprintf "ocamlfind dep -one-line -modules %s"
				(File.to_string ml_file))
      ~cache_file: (ml_file |> File.with_ext (File.extension ml_file ^ ".modules"))
      ~timestamp: (Timestamp.get ml_file)
    |> List.map parse_dep_line
  with
  | [ target, modules ] when target = ml_file -> modules
  | _ -> "Unable to compute dependent modules for " ^ File.to_string target |> failwith 

(** Adds dependencies on [static_files_lib].
    By default, we only add dependencies for files in the same directory. Since
    static files are in a separate folder, we need to manually add dependencies
    here. *)
let add_dependencies_on_static target =
  let static_module = static_files_lib |> File.base in
  if List.mem static_module (get_dependent_modules target)
  then
    let extension = File.extension target in
    (* i.e.: target = "blabla.cmx", dependency = "Static.cmx" *)
    let static_target = static_files_lib |> File.with_ext extension in
    Dependency.add target static_target

let () =
  let open Predicate.Infix in

  (* Configure Static/MakeStaticFile.exe *)
  begin
    let static_file_compiler_scope =
      static_file_compiler
      |> File.strip_ext |> File.to_string
      |> Predicate.full_base in
    Config.OCaml.configure ~predicate: static_file_compiler_scope;
    Config.configure ~predicate: static_file_compiler_scope
		     ~flags: "-package unix"
		     ()
  end;

  (* Configure Static/Library/File.(cmo|cmx) *)
  Config.OCaml.configure ~predicate: (Predicate.full_base static_repo);
  Config.configure
    ~predicate: ((Predicate.full_base static_repo)
		 &&$ (Predicate.extension "cmo" ||$ Predicate.extension "cmx"))
    ~flags: ("-for-pack " ^ (File.base static_files_lib))
    ();

  (* Configure static files. *)
  begin
    (* Static/Files/*.ml -> Static/Files/*.(cmo|cmx) *)
    Config.OCaml.configure ~predicate: (Predicate.package static_files_dir);

    (* Static/Files/*.(txt|js|...) -> Static/Files/*.ml *)
    Config.configure
      ~predicate: (Predicate.package static_files_dir &&$ Predicate.extension "ml")
      ~dependency: [ `File (fun target -> static_file_compiler) ;
		     `File (fun target -> get_static_source_file target) ]
      ~compiler: (let command ~flags ~target =
		    Printf.sprintf "./%s -source-file %s -ml-file %s.ml"
				   (static_file_compiler |> File.to_string)
				   (target |> get_static_source_file |> File.to_string)
				   (target |> File.strip_ext |> File.to_string)
		  in `Command command)
      ();

    let get_static_repo_dependency =
      let static_repo = static_repo |> File.parse in
      fun target -> static_repo |> File.with_ext (File.extension target)
    in
    (* Static/Files/*.(cmo|cmx) should
       - be compiled with "-for-pack Static",
       - have a dependency on "Static/Library/File.(cmo|cmx)". *)
    Config.configure
      ~predicate: (Predicate.package static_files_dir
		   &&$ (Predicate.extension "cmo" ||$ Predicate.extension "cmx"))
      ~dependency: [ `File get_static_repo_dependency ]
      (* TODO: make it so that -I is automatically added for all folders where there are dependencies. *)
      ~flags: ("-I " ^ (static_repo |> File.parse |> File.parent |> File.to_string) 
	       ^ " -for-pack " ^ (File.base static_files_lib))
      ();

    (* Static/Files/*.(cmo|cmx) -> Static/Static.(cmo|cmx) *)
    Config.configure
      ~predicate: (Predicate.oneof [ static_files_lib |> File.with_ext "cmo" ;
				     static_files_lib |> File.with_ext "cmx" ])
      ~dependency: [ `Custom (fun target ->
			      Iterable.of_list (read_static_files_dir static_files_dir)
			      |> Iterable.transform (File.with_ext (File.extension target))
			      |> Iterable.iter (Dependency.add target)) ;
		     `File get_static_repo_dependency ]
      ~compiler: (let command ~flags ~target =
		    let extension = File.extension target in
		    let ocaml_compiler = match extension with
		      | "cmx" -> "opt" | "cmo" -> "ocamlc"
		      | _ -> "Unexpected extension for static target: " ^ (File.to_string target) |> failwith
		    in
		    let static_target = static_files_lib |> File.with_ext extension |> File.to_string in
		    Printf.sprintf
		      "ocamlfind %s %s -pack -o %s %s"
		      ocaml_compiler
		      flags
		      static_target
		      (List.map (fun file -> file |> File.with_ext extension |> File.to_string)
				((File.parse static_repo) :: read_static_files_dir static_files_dir)
		       |> Utils.join " ")
		  in `Command command)
      ();

    (* Configure dependencies on Static/Static.(cma|cmxa) *)
    let configure_static extension =
      Config.configure
	~predicate: (Predicate.package ""
		     &&$ Predicate.extension extension)
	~dependency: [ `Custom add_dependencies_on_static ]
	~flags: ("-I " ^ static_dir)
	()
    in
    List.iter configure_static [ "cmo" ; "cmx" ]
  end;

  Config.OCaml.configure ~predicate: (Predicate.package "");

  (* Configure flags *)
  Config.configure
    ~predicate: (Predicate.package "" &&$ Config.OCaml.any_ocaml_target)
    ~flags: "-thread -package unix,lwt,lwt.unix,lwt.ppx,yojson,str"
    ()

let () =
  let target, debug =
    let target = ref ""
    and debug = ref false in
    Arg.parse
      ([ "-target", Arg.Set_string target, "<file.exe> file to build." ;
	 "-debug", Arg.Set debug, "true|false whether to add flag noassert" ]
       |> Arg.align)
      (fun anon_arg -> raise (Arg.Bad anon_arg))
      "Builder.exe -target <target> [-debug]";
    File.parse !target, !debug
  in
  if not debug then
    let open Predicate.Infix in
    Config.configure
      ~predicate: (Predicate.package "" &&$ Config.OCaml.any_ocaml_target)
      ~flags: "-noassert"
      ();
  OCamlBuild.compile target
