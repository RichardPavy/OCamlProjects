(** Linked hash set implementation. *)

module Iterable = Utils_Iterable
module Utils = Utils_Utils

(** Type of linked hash sets. *)
type 'a t = { queue : 'a Queue.t ;
	      set : ('a, unit) Hashtbl.t }

(** Creates a new linked hash set *)
let create () =
  { queue = Queue.create () ;
    set = Hashtbl.create 10 }

(** Returns whether the linked hash set contains the given value. *)
let mem { set } value = Hashtbl.mem set value

(**
 * Adds a value to the linked hash set.
 *
 * Fails if the set already contains the given value. *)
let add { queue ; set } value =
  Utils.check (not (Hashtbl.mem set value))
	      "Duplicate value";
  Hashtbl.add set value ();
  Queue.add value queue

(**
 * Creates a linked hash set containing all the elements in the iterable,
 * in the same order.
 *
 * If the iterable contains duplicate elements, they are de-duplicated. *)
let of_iterable it =
  let set = create () in
  it |> Iterable.iter begin fun e ->
                      if not (mem set e)
                      then add set e
                      end;
  set

(**
 * Returns an iterable that lists all the elements in the linked hash set,
 * in insertion order. *)
let to_iterable { queue } = Iterable.of_queue queue

(** Creates a linked hash set containing all the elements in the list. *)
let of_list l = l |> Iterable.of_list |> of_iterable

(** Creates a linked hash set containing all the elements in the array. *)
let of_array a = a |> Iterable.of_array |> of_iterable

let () =
  assert begin
      let lhs = create () in
      add lhs 1;
      add lhs 2;
      add lhs 3;
      lhs |> to_iterable |> Iterable.to_list
      = [ 1 ; 2 ; 3 ]
    end;
  assert begin
      let lhs = create () in
      add lhs 1;
      add lhs 2;
      try add lhs 1; false
      with _ -> true
    end
