module Iterable = Utils_Iterable
module Utils = Utils_Utils

type ('k, 'v) t = { queue : ('k * 'v) Queue.t ;
		    map : ('k, 'v) Hashtbl.t }

let create () =
  { queue = Queue.create () ;
    map = Hashtbl.create 10 }

let add { queue ; map } key value =
  Utils.check (not (Hashtbl.mem map key))
	      "Duplicate key";
  Hashtbl.add map key value;
  Queue.add (key, value) queue

let mem { map } key = Hashtbl.mem map key

let get { map } key = Hashtbl.find map key

let to_iterable { queue } = Iterable.of_queue queue
let keys lhm = lhm |> to_iterable |> Iterable.map fst
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
