(** Hash set implementation. *)

module Iterable = Utils_Iterable
module Utils = Utils_Utils

(** Type of hash sets. *)
type 'a t = ('a, unit) Hashtbl.t

(** Creates a new hash set. *)
let create () = Hashtbl.create 16

(**
 * Adds a value to the hash set.
 *
 * Fails if the set already contains the given value. *)
let add set value =
  Utils.check (not (Hashtbl.mem set value))
	      "Duplicate value";
  Hashtbl.add set value ()

(** Returns whether the hash set contains the given value. *)
let mem set value = Hashtbl.mem set value

(**
 * Creates a hash set containing all the elements in the iterable.
 *
 * If the iterable contains duplicate elements, they are de-duplicated. *)
let of_iterable it =
  let set = create () in
  it |> Iterable.iter begin fun e ->
                      if not (mem set e)
                      then add set e
                      end;
  set

(** Returns an iterable that lists all the elements in the hash set. *)
let to_iterable set = set |> Iterable.of_hashtbl |> Iterable.map fst

(** Creates a hash set containing all the elements in the list. *)
let of_list l = l |> Iterable.of_list |> of_iterable

(** Creates a hash set containing all the elements in the array. *)
let of_array a = a |> Iterable.of_array |> of_iterable

(** Returns true of the two hash sets contain the same elements. Order does not matter. *)
let eq a b =
  a == b
  || (Hashtbl.length a = Hashtbl.length b
      && try Hashtbl.iter (fun value () -> if not (mem b value)
					   then raise Exit)
			  a;
	     true
	 with Exit -> false)

(** Returns a hash set that contains the union of all elements. *)
let union a b =
  let u = create () in
  let add_if_missing e =
    if mem u e |> not then add u e
  in
  a |> Iterable.iter add_if_missing;
  b |> Iterable.iter add_if_missing;
  u
