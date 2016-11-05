type 'a generator = Utils_File.t -> 'a Utils_Iterable.t
type 'a handle = 'a generator Container_LinkedList.handle
type 'a t =
  { add: ?package: Utils_File.t ->
         'a generator ->
         'a handle ;
    get: 'a generator }

val create : unit -> 'a t
