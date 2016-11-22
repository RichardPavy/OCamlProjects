module type Impl = sig
  type element
  val fold : ('accu -> element -> 'accu)
	     -> 'accu
	     -> 'accu
end

type 'a t = (module Impl with type element = 'a)

let (make : (module Impl with type element = 'a) -> 'a t) = fun foldable -> foldable

let fold (type e) f init (module M : Impl with type element = e) = M.fold f init

let cycle (type e) e =
  let module M = struct
      type element = e
      let rec fold f accu = fold f (f accu e)
    end
  in (module M : Impl with type element = e)

let singleton (type e) e =
  let module M = struct
      type element = e
      let fold f accu = f accu e
    end
  in (module M : Impl with type element = e)

let of_list (type e) l =
  let module M = struct
      type element = e
      let fold f accu = List.fold_left f accu l
    end
  in (module M : Impl with type element = e)

let of_array (type e) a =
  let module M = struct
      type element = e
      let fold f accu = Array.fold_left f accu a
    end
  in (module M : Impl with type element = e)

let of_queue (type e) q =
  let module M = struct
      type element = e
      let fold f accu = Queue.fold f accu q
    end
  in (module M : Impl with type element = e)

let of_hashtbl (type k) (type v) t =
  let module M = struct
      type element = k * v
      let fold f accu =
	Hashtbl.fold (fun k v accu -> f accu (k, v)) t accu
    end
  in (module M : Impl with type element = k * v)

let concat (type e)
	   (module A : Impl with type element = e)
	   (module B : Impl with type element = e) =
  let module M = struct
      type element = e
      let fold f accu = accu |> A.fold f |> B.fold f
    end
  in (module M : Impl with type element = e)

let filter (type e)
	   predicate
	   (module M : Impl with type element = e) =
  let module M' = struct
      type element = e
      let fold f accu = M.fold (fun accu e -> if predicate e
					      then f accu e
					      else accu)
			       accu
    end
  in (module M' : Impl with type element = e)

let map (type e) (type e')
	(transfo : e -> e')
	(module M : Impl with type element = e) =
  let module M' = struct
      type element = e'
      let fold f accu = M.fold (fun accu e -> f accu (transfo e))
			       accu
    end
  in (module M' : Impl with type element = e')

exception Top
let top (type e)
	n
	(module M : Impl with type element = e) =
  let module M' = struct
      type element = e
      let fold f accu =
	let result = ref accu in
	try
	  M.fold begin fun (accu, i) e ->
		       if i = n
		       then (result := accu; raise Top)
		       else (f accu e), (i + 1)
		 end
		 (accu, 0)
	  |> fst
	with Top -> !result
    end
  in (module M' : Impl with type element = e)

exception Break

let any predicate foldable =
  try fold (fun _ e -> if predicate e
		       then raise Break
		       else false)
	   false
	   foldable
  with Break -> true

let all predicate foldable =
  any (fun e -> e |> predicate |> not)
      foldable
  |> not

let to_list (type e) (module M : Impl with type element = e) =
  M.fold (fun accu e -> e :: accu) []

let to_array foldable = foldable |> to_list |> Array.of_list

let () =
  assert (Log.dlog "Testing Foldable");
  assert begin
      assert begin
          let l1 = [ 1 ; 2 ; 3 ] |> of_list
          and l2 = [ 4 ; 5 ; 6 ] |> of_list in
          concat l1 l2 |> to_list
          = [ 6 ; 5 ; 4 ; 3 ; 2 ; 1 ]
        end;
      assert begin
          let l1 = [ 1 ; 2 ; 3 ] |> of_list
          and l2 = [ 4 ; 5 ; 6 ] |> of_list in
          concat l1 l2 |> filter (fun e -> 0 = e mod 2) |> to_list
          = [ 6 ; 4 ; 2 ]
        end;
      assert begin [ 1 ; 2 ; 3] |> of_list |> any ((=) 2) end;
      assert begin [ 1 ; 2 ; 3] |> of_list |> any ((=) 4) |> not end;
      assert begin [ 1 ; 2 ; 3] |> of_list |> all (fun e -> e < 4) end;
      assert begin [ 1 ; 2 ; 3] |> of_list |> all (fun e -> e < 2) |> not end;
      assert begin
          [ 1 ; 2 ; 3 ; 4 ; 5 ] |> of_list |> top 3 |> fold (+) 0
          = 6
        end;
      assert begin
          [ 1 ; 2 ; 3 ; 4 ; 5 ] |> of_list |> top 10 |> fold (+) 0
          = 15
        end;
      assert begin
          cycle "X" |> top 10 |> fold ( ^ ) ""
          = "XXXXXXXXXX"
        end;
      true
    end
