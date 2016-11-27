type 'a generator = Utils.File.t -> 'a Utils.Iterable.t
type 'a handle = 'a generator Container.LinkedList.handle
type 'a t =
  { add: ?package: Utils.File.t ->
         'a generator ->
         'a handle ;
    get: 'a generator }

val create : unit -> 'a t

(** Runs the given function and then removes all handles. *)
val process : (unit -> 'result) -> 'x handle list -> 'result
