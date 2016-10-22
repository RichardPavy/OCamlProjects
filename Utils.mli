(** [async name callback] launches lwt thread [callback].
    If [callback] raises an exception, an error message is shown with [name] *)
val async : string -> (unit -> unit Lwt.t) -> unit

(** An lwt thread that logs an error and fails.  *)
val fail : string -> 'a Lwt.t

(** Generates a unique string *)
val gen_uuid : unit -> string

(** Runs the given method and returns the execution wall time *)
val time : (unit -> unit) -> float

(** Splits a string representing a path.
    [split_path "/root/path/to/a/file.txt"] returns [["file.txt"; "a"; "to"; "path"; "root"]] *)
val split_path : string -> string list

(** Reverse operation from [split_path] *)
val join : string list -> string
