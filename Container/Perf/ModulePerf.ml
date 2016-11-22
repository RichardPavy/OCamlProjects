(** Evaluate performance of using modules as virtual classes. *)

module F = Utils_Foldable

let iterations = 1000 * 1000 * 600

let a = Array.make 1 1
let f = F.of_array a
let x = ref ignore

let time f =
  let start = Unix.gettimeofday () in
  f ();
  Unix.gettimeofday () -. start

let perf () =
  let a_time = time begin fun () ->
                    for i = 0 to iterations do
                      Array.fold_left ( + ) 0 a |> ignore
                    done
                    end
  in
  let f_time = time begin fun () ->
                    for i = 0 to iterations do
                      F.fold f ( + ) 0 |> ignore
                    done
                    end
  in
  let x_time = time begin fun () ->
                    for i = 0 to iterations do
                      !x ()
                    done
                    end
  in
  Printf.printf
    "Array: %f\nFoldable: %f\nFn: %f\n"
    a_time
    f_time
    x_time

let () = x := begin fun () ->
              Array.fold_left ( + ) 0 a |> ignore
              end

let mem =
  let last = ref 0. in
  fun () ->
  let open Gc in
  full_major ();
  compact ();
  let { minor_words ; major_words ; promoted_words ; heap_words } = Gc.stat () in
  let mem = (minor_words +. major_words -. promoted_words) /. 8. /. 1000000. in
  let diff = mem -. !last in
  last := mem;
  diff
  (* float_of_int heap_words /. 8. /. 1000000. *)

let instances = 1000 * 1000

let print_mem name f =
  time begin fun () ->
       let arrays = Array.init instances f in
       Printf.printf "%s: %.2fMb" name (mem ());
       Printf.printf "  (%i)" (Array.length arrays)
       end
  |> Printf.printf "  (%.2f seconds)\n"

module type P1 = sig
  val x : int -> int
end

module type P2 = sig
  val x : int -> int
  val y : int -> int
end

module type P3 = sig
  val x : int -> int
  val y : int -> int
  val z : int -> int
end

let make1 p =
  let module M = struct
      let x i = i + p
    end
  in (module M : P1)

let make2 p =
  let module M = struct
      let x i = i + p
      let y i = i * p
    end
  in (module M : P2)

let make3 p =
  let module M = struct
      let x i = i + p
      let y i = i * p
      let z i = i / p
    end
  in (module M : P3)

module type Arg = sig
  val p : int
end

module MakeP1 =
  functor (A : Arg) -> struct
    let x i = i + A.p
  end

module MakeP2 =
  functor (A : Arg) -> struct
    let x i = i + A.p
    let y i = i * A.p
  end

module MakeP3 =
  functor (A : Arg) -> struct
    let x i = i + A.p
    let y i = i * A.p
    let z i = i / A.p
  end

let make1' p =
  let module A = struct let p = p end in
  (module MakeP1(A) : P1)

let make2' p =
  let module A = struct let p = p end in
  (module MakeP2(A) : P2)

let make3' p =
  let module A = struct let p = p end in
  (module MakeP3(A) : P3)

let make1'' p = let (module R) = make3 p in (module R : P1)
let make2'' p = let (module R) = make3 p in (module R : P2)
let make3'' p = let (module R) = make3 p in (module R : P3)

let mem_usage () =
  for i = 0 to 1 do
    print_mem "Arrays" (fun x -> Array.make 1 "hello" |> ref);
    print_mem "Foldables" (fun x -> Array.make 1 "hello" |> F.of_array);
    print_mem "P1" make1;
    print_mem "P2" make2;
    print_mem "P3" make3;
    print_mem "P1'" make1';
    print_mem "P2'" make2';
    print_mem "P3'" make3';
    print_mem "P1''" make1'';
    print_mem "P2''" make2'';
    print_mem "P3''" make3''
  done

let () = mem_usage ()
let x x = x |> Obj.magic |> Obj.size
module type E = (module type of Set)
let _ = Printf.printf "Size of foldable: %i" (F.of_list [1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3;1;2;3] |> x)
