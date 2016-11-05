module Iterable = Utils_Iterable
module Utils = Utils_Utils

type 'a t = ('a, unit) Hashtbl.t
let create () = Hashtbl.create 16
let add set value =
  Utils.check (not (Hashtbl.mem set value))
	      "Duplicate value";
  Hashtbl.add set value ()
let mem set value = Hashtbl.mem set value
let of_iterable it =
  let set = create () in
  it |> Iterable.iter (add set);
  set
let to_iterable set = set |> Iterable.of_hashtbl |> Iterable.map fst
let of_list l = l |> Iterable.of_list |> of_iterable
let of_array a = a |> Iterable.of_array |> of_iterable
let eq a b =
  a == b
  || (Hashtbl.length a = Hashtbl.length b
      && try Hashtbl.iter (fun value () -> if not (mem b value)
					   then raise Exit)
			  a;
	     true
	 with Exit -> false)
let union a b =
  let u = create () in
  let add_if_missing e =
    if mem u e |> not then add u e
  in
  a |> Iterable.iter add_if_missing;
  b |> Iterable.iter add_if_missing;
  u
