module Iterable = Utils_Iterable
module Utils = Utils_Utils

type 'a t = { queue : 'a Queue.t ;
	      set : ('a, unit) Hashtbl.t }

let create () =
  { queue = Queue.create () ;
    set = Hashtbl.create 10 }

let mem { set } value = Hashtbl.mem set value

let add { queue ; set } value =
  Utils.check (not (Hashtbl.mem set value))
	      "Duplicate value";
  Hashtbl.add set value ();
  Queue.add value queue

let to_iterable { queue } = Iterable.of_queue queue

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
