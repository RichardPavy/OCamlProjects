(** Linked hash map implementation. *)

module Iterable = Utils_Iterable
module Utils = Utils_Utils

(** Type of linked hash maps. *)
type ('k, 'v) t = { queue : ('k * 'v) Queue.t ;
		    map : ('k, 'v) Hashtbl.t }

(** Creates a new linked hash map. *)
let create () =
  { queue = Queue.create () ;
    map = Hashtbl.create 10 }

(**
 * Adds a key-value pair to the linked hash map.
 *
 * Fails if the map already contains a binding with the given key. *)
let add { queue ; map } key value =
  Utils.check (not (Hashtbl.mem map key))
	      "Duplicate key";
  Hashtbl.add map key value;
  Queue.add (key, value) queue

(** Returns whether the map contains a value under the given key. *)
let mem { map } key = Hashtbl.mem map key

(**
 * Returns the value stored under the given key.
 *
 * Raises [Not_found] if no value is stored under the given key. *)
let get { map } key = Hashtbl.find map key

(** Returns an iterable that lists all the key-value pairs in insertion order. *)
let to_iterable { queue } = Iterable.of_queue queue

(** Returns an iterable that lists all the keys in insertion order. *)
let keys lhm = lhm |> to_iterable |> Iterable.map fst

(** Returns an iterable that lists all the values in insertion order. *)
let values lhm = lhm |> to_iterable |> Iterable.map snd

let () =
  assert begin
      let lhm = create () in
      add lhm 1 "a";
      add lhm 2 "b";
      add lhm 3 "a";
      lhm |> keys |> Iterable.to_list
      = [ 1 ; 2 ; 3 ]
    end;
  assert begin
      let lhm = create () in
      add lhm 1 "a";
      add lhm 2 "b";
      add lhm 3 "a";
      lhm |> values |> Iterable.to_list
      = [ "a" ; "b" ; "a" ]
    end;
  assert begin
      let lhm = create () in
      add lhm 1 "a";
      add lhm 2 "b";
      try add lhm 1 "c"; false
      with _ -> true
    end
