open Utils
open Container

module It = Iterable

(**
 * Runs ocamldep to figure out the dependencies of a file.
 * Does not include the file itself.
 * - source: an OCaml source file (either *.ml or *.mli)
 * - extension: the extension with which dependencies are returned.
 *   Can be any extension. *)
let get extension source =
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
let get_transitive extension source =
  let module LHS = LinkedHashSet in
  let deps = LHS.create () in
  let source_ext = File.extension source in
  let rec process dep =
    if LHS.mem deps dep |> not
    then begin
        LHS.add deps dep;
        dep
        |> File.with_ext source_ext
        |> get extension
        |> It.iter process
      end
  in
  process (source |> File.with_ext extension);
  deps |> LHS.to_iterable

(** Returns all the modules that a source depends on, including system modules. *)
let modules source =
  let all_modules = HashSet.create () in
  source
  |> get_transitive (File.extension source)
  |> It.map OCamlDep.modules
  |> It.map It.of_list
  |> It.flatten
  |> It.iter begin fun m -> if HashSet.mem all_modules m |> not
                            then HashSet.add all_modules m
             end;
  all_modules
