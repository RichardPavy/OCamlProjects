type 'a generator = Utils_File.t -> 'a Utils_Iterable.t
type 'a handle = 'a generator Container.LinkedList.handle
type 'a t =
  { add: ?package: Utils_File.t ->
         'a generator ->
         'a handle ;
    get: Utils_File.t -> 'a Utils_Iterable.t }

val create : unit -> 'a t

val process : (unit -> 'result) -> 'x handle list -> 'result
