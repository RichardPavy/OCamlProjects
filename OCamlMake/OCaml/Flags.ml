open Utils
open OCamlMake_Common

module It = Iterable
module HashSet = Container_HashSet
module LinkedHashSet = Container_LinkedHashSet

type Flag.kind += Interface
type Flag.kind += Object
type Flag.kind += Executable

(** Include the source file's directory if necessary. *)
let include_dirs ~kind ~target ~sources =
  let flags =
    sources
    |> It.filter (let open Predicate.Infix in
                  !$File.is_toplevel)
    |> It.map begin fun source_file ->
              "-I " ^ (source_file
                       |> File.parent
                       |> File.to_string)
              end
    |> LinkedHashSet.of_iterable |> LinkedHashSet.to_iterable
  in
  Flag.add_file ~kind ~file: target ~flags

let module_to_package =
  [ "Lwt", "lwt" ;
    "Lwt", "lwt.unix" ;
    "Unix", "unix" ;
    "Str", "str" ;
    "Yojson", "yojson" ;
  ] |> It.of_list

(**
 * Returns a flag to include all required packages when linking an executable.
 * - kind: flag kind namespace.
 * - source: source file to determine all transitive dependencies.
 * - target: file to which the flag should be attached to. *)
let packages ~kind ~source ~target =
  begin fun () ->
  let modules = Dependency.modules source in
  let packages = module_to_package
                 |> It.filter (fun (m, _) -> HashSet.mem modules m)
                 |> It.map snd
                 |> It.to_list
  in
  let packages_flags = packages
                       |> It.of_list
                       |> It.map (fun package -> "-package " ^ package)
  in
  let flags = if kind = Executable
              then It.concat (It.singleton "-linkpkg") packages_flags
              else packages_flags
  in
  Flag.add_file ~kind
                ~file: target
                ~flags
  end
  |> Log.block "add_package_flag <%s -> %s>"
               (File.to_string source) (File.to_string target)


let () =
  assert (Log.dlog "Testing Flags");
  assert begin
      [ "test include_dirs, with includes",
        begin fun () ->
        let target = File.parse "X/Y/Z.ml"
        and kind = Flag.Unknown_Kind in
        [ include_dirs
            ~kind
            ~target
            ~sources: ([ File.parse "File.ml" ;
                         File.root ;
                         File.parse "Package1/A.ml" ;
                         File.parse "Package2/B.ml" ;
                         File.parse "Package2/X.ml" ;
                         File.parse "Package2/YB.ml" ;
                       ] |> It.of_list)
        ] |> Property.process
               begin fun () ->
               Flag.get kind target = "-I Package1 -I Package2"
               end
        end ;

        "test include_dirs, no includes",
        begin fun () ->
        let target = File.parse "X/Y/Z.ml"
        and kind = Flag.Unknown_Kind in
        [ include_dirs
            ~kind
            ~target
            ~sources: ([ File.parse "File.ml" ; File.root ] |> It.of_list)
        ] |> Property.process
               begin fun () ->
               Flag.get kind target = ""
               end
        end ;
      ] |> Asserts.test
    end
