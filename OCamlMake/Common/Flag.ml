(** Flags for command lines used to build targets. *)

open Utils

(** Namespace for flags. *)
type kind = ..
type kind += Unknown_Kind

let flags = Cache.fn (fun (* kind *) _ -> Property.create ())

(**
 * Register a flag generator to add flags for all the files matching
 * the given predicate. *)
let add ~kind
        ?package
        ?predicate
        generator =
  let generator =
    match predicate with
    | None -> generator
    | Some p -> fun file -> if p file
                            then generator file
                            else Iterable.empty ()
  in
  (flags kind).Property.add ?package generator

(** Register flags for the given file only. *)
let add_file ~kind ~file ~flags =
  add ~kind
      ~package: (File.parent file)
      ~predicate: (Predicate.equals file)
      begin fun f ->
      assert (Utils.dcheck (f = file)
                           "File generator for <%s> used on file <%s>"
                           (File.to_string file) (File.to_string f));
      flags
      end

(** Returns all the flags that apply to a file. *)
let get kind ?(sep = " ") target =
  (flags kind).Property.get target
  |> Utils.join sep
