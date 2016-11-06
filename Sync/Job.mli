(** Type of method that runs a thread inside the current job. *)
type executor = (unit -> unit Lwt.t) -> unit

(** A job is a method that runs threads with an executor.
    It returns a thread that finishes when all the threads
    created with the executor are finished. *)
type job = executor -> unit Lwt.t

(** Runs the given job. *)
val run : job -> unit Lwt.t
