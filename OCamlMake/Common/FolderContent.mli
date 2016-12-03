(** Module to list files in a folder. *)

(**
 * Returns all the files in the given folder.
 *
 * The results are cached using an LRU cache *)
val list : Utils_File.t -> string array

(** Clears the cache for the given folder. *)
val clear_cache : Utils_File.t -> unit
