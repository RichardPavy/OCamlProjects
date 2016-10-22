type 'a generator = File.t -> 'a Iterable.t
type 'a handle = 'a generator LinkedList.handle
type 'a t =
  { add: ?package: File.t ->
         'a generator ->
         'a handle ;
    get: 'a generator }

val create : unit -> 'a t
