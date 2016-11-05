module Cache = Utils_Cache
module File = Utils_File
module HashSet = Container_HashSet
module Iterable = Utils_Iterable
module Predicate = Utils_Predicate
module Utils = Utils_Utils

type Flag.kind += OCamlDep

let is_system_module =
  let ocaml_system_modules = [
      "Arg" ;                       "Complex" ;      "List" ;        "Ratio" ;
      "Arith_status" ;              "Condition" ;    "ListLabels" ;  "Scanf" ;
      "Array" ;                     "Digest" ;       "Map" ;         "Set" ;
      "ArrayLabels" ;               "Dynlink" ;      "Marshal" ;     "Sort" ;
      "Big_int" ;                   "Event" ;        "MoreLabels" ;  "Stack" ;
      "Bigarray" ;                  "Filename" ;     "Mutex" ;       "StdLabels" ;
      "Buffer" ;                    "Format" ;       "Nativeint" ;   "Str" ;
      "Bytes" ;                     "Gc" ;           "Num" ;         "Stream" ;
      "BytesLabels" ;               "Genlex" ;       "Obj" ;         "String" ;
      "Callback" ;                  "Graphics" ;     "Oo" ;          "StringLabels" ;
      "CamlinternalFormat" ;        "GraphicsX11" ;  "Parsing" ;     "Sys" ;
      "CamlinternalFormatBasics" ;  "Hashtbl" ;      "Pervasives" ;  "Thread" ;
      "CamlinternalLazy" ;          "Int32" ;        "Printexc" ;    "ThreadUnix" ;
      "CamlinternalMod" ;           "Int64" ;        "Printf" ;      "Unix" ;
      "CamlinternalOO" ;            "Lazy" ;         "Queue" ;       "UnixLabels" ;
      "Char" ;                      "Lexing" ;       "Random" ;      "Weak" ;
      "Lwt" ;
      "Yojson"
    ] |> HashSet.of_list
  in
  fun module_name ->
  HashSet.mem ocaml_system_modules module_name
  || (module_name |> Utils.starts_with "Lwt_")

(** Returns all the modules referenced by a .ml or .mli file. *)
let modules =
  Cache.fn
    ~max_size: 100
    begin fun ml_file ->
    (match File.extension ml_file with
     | "ml" | "mli" ->  ()
     | _ -> Utils.fail "Not an OCaml file: <%s>"
		       (File.to_string ml_file)
	    |> raise);
    let line =
      match
        let f =
          if Private.is_private ml_file then
            fun command ->
            Process.run_command_cached
	      ~cache_file: (ml_file |> File.with_ext (File.extension ml_file ^ ".modules"))
	      ~timestamp: (Timestamp.get ml_file)
	      ~command
         else
           Process.run_command
        in f "ocamlfind dep -one-line -modules %s %s"
	     (Flag.get OCamlDep ml_file)
	     (File.to_string ml_file)
      with
      | [ line ] -> line
      | _ -> Utils.fail "Unable to compute OCamlDep modules for <%s>"
		        (File.to_string ml_file)
	     |> raise
    in
    let start = 1 + String.index line ':' in
    Str.split
      (Str.regexp "[ \r\n\t]+")
      (String.sub line start (String.length line - start))
    end

let ocamldep ml_file =
  ml_file
  |> modules
  |> Iterable.of_list
  |> Iterable.filter (let open Predicate.Infix in !$is_system_module)
  |> Iterable.to_list
