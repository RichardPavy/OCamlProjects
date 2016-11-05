module Cache = Utils_Cache
module File = Utils_File
module Iterable = Utils_Iterable
module LL = Container_LinkedList

type 'a generator = File.t -> 'a Iterable.t
type 'a handle = 'a generator LL.handle
type 'a t =
  { add: ?package: File.t ->
         'a generator ->
         'a handle ;
    get: 'a generator }

let create () =
  let generators = Cache.fn (fun (* package option *) _ -> LL.create()) in
  let add ?package generator =
    LL.add
      (generators package)
      generator
  and get target =
    let queue = Queue.create () in
    Iterable.iter
      (fun generator -> generator target
			|> Iterable.iter (fun e -> Queue.add e queue))
      (Iterable.concat
         (None |> generators |> LL.to_iterable)
         (Some (target |> File.parent) |> generators |> LL.to_iterable));
    Iterable.of_queue queue
  in { add ; get }
