(**
 * Implementation of dynamically growing arrays.
 *
 * - On creation, a certain amount of space is reserved for the array.
 * - On insertion, if the underlying [array] is full, a new 2x larger array is
 *   created, slices grow dynamically.
 * - Sub-slices can be created, as 'views' on the original slice.
 *)

module Foldable = Utils_Foldable
module It = Utils_Iterable
module Log = Utils_Log
module Utils = Utils_Utils

(** Type of Slices. *)
type 'a t = {
    (** Dynamically allocated array. *)
    mutable buffer : 'a array ;

    (** First index. *)
    start : int ;

    (** Past end index. *)
    mutable stop : int }

let default_capacity = 8

(**
 * Creates a new slice.
 * @param capacity The initial size of the underlying array. *)
let create ?(capacity = default_capacity) () =
  { buffer = Array.make (max 1 capacity) (Obj.magic None) ;
    start = 0 ;
    stop = 0 }

(** Removes all elements from the slice. *)
let clear ?(capacity = default_capacity) slice =
  slice.buffer <- Array.make (max 1 capacity) (Obj.magic None);
  slice.stop <- 0

(**
 * Creates a new slice from the given array.
 *
 * The array is copied and not used directly. *)
let of_array a =
  { buffer = Array.copy a ;
    start = 0 ;
    stop = Array.length a }

(** Creates a new slice from the given list. *)
let of_list l =
  let a = Array.of_list l in
  { buffer = a ;
    start = 0 ;
    stop = Array.length a }

(** Returns the size of the slice (not the capacity). *)
let length { start ; stop } = stop - start

(** Returns the i{^th} element. *)
let get slice i =
  assert (Utils.dcheck
            (0 <= i && i < length slice)
            "Slice.get <Slice length:%i> i:%i"
            (length slice) i);
  slice.buffer.(slice.start + i)

(** Inserts an element at the end of the slice. *)
let add slice e =
  begin
    let l = Array.length slice.buffer in
    if slice.stop = l then
      slice.buffer <-
        Array.init
          (2 * l)
          begin fun i -> if i < l
                         then slice.buffer.(i)
                         else Obj.magic None
          end;
  end;
  slice.buffer.(slice.stop) <- e;
  slice.stop <- slice.stop + 1

(** Inserts all the element in the iterable. *)
let add_all slice it = It.iter (add slice) it

(**
 * Returns a sub-slice.
 *
 * Modifying the returned sub-slice may affect the original slice. *)
let sub slice start count =
  assert (Utils.dcheck
            begin
              start >= 0
              && count >= 0
              && length slice - start >= count
            end
            "Slice.sub <Slice length:%i> start:%i count:%i"
            (length slice) start count);
  let start = slice.start + start in
  { slice with start ; stop = start + count }

(** Returns an {Utils_Iterable.t} that lists all elements in the slice. *)
let to_iterable slice =
  begin fun f ->
  let { buffer ; start ; stop } = slice in
  for i = start to stop - 1 do
    f buffer.(i)
  done
  end |> It.make

(** Returns an {Utils_Foldable.t} that lists all elements in the slice. *)
let to_foldable (type e) { buffer ; start ; stop } =
  let module M = struct
      type element = e
      let fold f accu =
        let rec fold_aux i accu =
          if i = stop then
            accu
          else
            fold_aux (i+1) (f accu buffer.(i))
        in
        fold_aux start accu
    end
  in (module M : Foldable.Impl with type element = e)
     |> Foldable.make

(** Returns list that contains all elements in the slice. *)
let to_list { buffer ; start ; stop } =
  let rec aux i accu =
    if i < start
    then accu
    else aux (i-1) (buffer.(i) :: accu)
  in aux (stop - 1) []

(** Returns a new array that contains all elements in the slice. *)
let to_array { buffer ; start ; stop } =
  Array.init
    (stop - start)
    (fun i -> buffer.(i + start))

(** Returns whether two slices are equal. *)
let equals ?(eq = ( = ))a b =
  let l = length a in
  l = length b
  && try for i = 0 to l-1 do
           if not (eq (get a i) (get b i))
           then raise Exit
         done;
         true
     with Exit -> false

let () =
  assert (Log.dlog "Testing Slice");
  assert begin
      let s = create () in
      add s 1;
      add s 2;
      [ 3 ; 4 ] |> It.of_list |> add_all s;
      assert (sub s 1 2 |> to_iterable |> It.to_list
              = [ 2 ; 3 ]);
      assert (s |> to_foldable |> Foldable.fold (fun t h -> h :: t) []
              = [ 4 ; 3 ; 2 ; 1 ]);
      assert (sub s 1 2 |> to_foldable |> Foldable.fold (fun t h -> h :: t) []
              = [ 3 ; 2 ]);
      assert (s |> to_list = [ 1 ; 2 ; 3 ; 4 ]);
      assert (s |> to_array = [| 1 ; 2 ; 3 ; 4 |]);
      assert (sub s 1 2 |> to_list = [ 2 ; 3 ]);
      assert (sub s 1 2 |> to_array = [| 2 ; 3 |]);
      true
    end
