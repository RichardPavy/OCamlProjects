type 'a t = ('a -> unit) -> unit

let filter predicate it f = it (fun e -> if predicate e then f e)

exception Break
let any predicate it = try it (fun e -> if predicate e then raise Break); false
		       with Break -> true

let all predicate it = try it (fun e -> if not (predicate e) then raise Break); true
		       with Break -> false

let iter f it = it f
let fold f init it =
  let accu = ref init in
  it (fun e -> accu := f !accu e);
  !accu

let mem x = any ((=) x)
let concat (it1 : 'a t) (it2 : 'a t) f = it1 f; it2 f
let flatten it f = it (fun it' -> it' f)
let map transfo it f = it (fun e -> f (transfo e))

let top n it =
  let l = ref [] in
  let n = ref n in
  begin
    try it (fun e -> if !n = 0
		     then raise Break
		     else l := e :: !l;
		     decr n)
    with Break -> ()
  end;
  !l

let make it = it
let singleton e f = f e
let empty () = ignore

let of_list l f = List.iter f l
let of_array a f = Array.iter f a
let of_queue q f = Queue.iter f q
let of_hashtbl t f = Hashtbl.iter (fun k v -> f (k, v)) t
let of_lazy l f = (Lazy.force l) f

let to_list it = fold (fun accu e -> e :: accu) [] it |> List.rev
let to_array it = it |> to_list |> Array.of_list
