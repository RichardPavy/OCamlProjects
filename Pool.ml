module Bits =
  struct
    let bits =
      let rec bits x b =
	if x = 0
	then b
	else bits (x lsl 1) (b + 1)
      in bits 1 0

    let half_bits = bits / 2

    let low =
      let mask = (1 lsl half_bits) - 1
      in fun n -> n land mask

    let high = fun n -> n lsr half_bits

    let int ~high ~low = (high lsl half_bits) lor low
  end

module Key : sig
    type 'a t
    val index : 'a t -> int
    val key : int -> 'a t
  end = struct
    type 'a t = int
    let index key = key
    let key index = index
  end

type 'a key = 'a Key.t
type 'a bucket = {
    key : 'a key;
    resource : 'a;
  }

exception UnavailableResource
exception SeedMismatch

module type ResourceType =
  sig
    type 'a t
    val create : unit -> 'a t
  end

module type Sig =
  sig
    type 'a resource
    val alloc : unit -> 'a resource bucket
    val free : 'a resource key -> unit
    val get : 'a resource key -> 'a resource
  end

module Make =
  functor (Resource : ResourceType) ->
  (struct
      type 'a resource = 'a Resource.t
      type 'a cell = Cell of int * 'a | Empty

      let pool = ref ([| Empty |])
      let available = ref [ 0 ]

      let free key =
	let index = Key.index key in
	let i = Bits.high index
	and seed = Bits.low index
	and pool = !pool in
	match pool.(i) with
	| Empty -> raise UnavailableResource
	| Cell (expectedSeed, _) ->
	   if expectedSeed != seed then raise SeedMismatch;
	   pool.(i) <- Empty;
	   available := i :: !available

      let get key =
	let index = Key.index key in
	let i = Bits.high index
	and seed = Bits.low index in
	match !pool.(i) with
	| Empty -> raise UnavailableResource
	| Cell (expectedSeed, resource) ->
	   if expectedSeed != seed then raise SeedMismatch;
	   Obj.magic resource

      let rec alloc () =
	match !available with
	| i :: q ->
	   available := q;
	   let resource = Resource.create () in
	   let seed = Bits.low (Random.bits ()) in
	   !pool.(i) <- Cell (seed, Obj.magic resource);
	   { key = Key.key (Bits.int ~high:i ~low:seed) ; resource }
	| [] ->
	   let size = Array.length !pool in
	   let new_pool = Array.make (2 * size) Empty in
	   for i = size to 2 * size - 1 do
	     available := i :: !available
	   done;
	   Array.blit !pool 0 new_pool 0 size;
	   pool := new_pool;
	   alloc ()

    end : Sig with type 'a resource = 'a Resource.t)
