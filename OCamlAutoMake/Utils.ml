(** Concatenates a list of strings. *)
let join sep it =
  let b = Buffer.create 10 in
  let first = ref true in
  Iterable.iter
    (fun s -> if !first
	      then first := false
	      else Buffer.add_string b sep;
	      Buffer.add_string b s)
    it;
  Buffer.contents b

let split sep string =
  let l = String.length string in
  let rec aux start i accu =
    if i = l then
      String.sub string start (i-start) :: accu
    else if string.[i] = sep then
      aux (i+1) (i+1) (String.sub string start (i-start) :: accu)
    else
      aux start (i+1) accu
  in
  aux 0 0 []

let () = assert begin
             true
             && split ':' "" = [""]
             && split ':' "abc" = ["abc"]
             && split ':' "abc:def" = ["def"; "abc"]
           end

let starts_with string =
  let l = String.length string in
  fun candidate ->
  String.length candidate >= l
  && String.sub candidate 0 l = string

let () = assert ("" = join "-" ([] |> Iterable.of_list));
	 assert ("a" = join "-" (["a"] |> Iterable.of_list));
	 assert ("a-b-c" = join "-" (["a";"b";"c"] |> Iterable.of_list))

let fail format =
  Printf.ksprintf (fun message -> failwith message) format

let kisprintf k (CamlinternalFormatBasics.Format (fmt, _)) =
  let k' () acc = k in
  (CamlinternalFormat.make_printf k' () CamlinternalFormat.End_of_acc fmt)

let check condition =
  if not condition
  then Printf.ksprintf (fun message -> failwith message)
  else kisprintf ()

let dcheck condition =
  if not condition
  then Printf.ksprintf (fun message -> failwith message)
  else kisprintf true

let () =
  assert
    (try check true "%s %i" "abc" 123; true
     with Failure _ -> false);
  assert
    (try check false "%s %i" "abc" 123; false
     with Failure message -> message = "abc 123")

module Option = struct
  let ( |?> ) value f =
    match value with
    | None -> None
    | Some x -> Some (f x)
  let ( ?> ) f value =
    match value with
    | None -> ()
    | Some x -> f x
  end

let () = assert begin
             let open Option in
             let f x = 2 * x in
             assert begin ((Some 3) |?> f) = Some 6 end;
             assert begin (None |?> f) = None end;
             let y = ref 0 in
             let f x = y := x in
             ?>f None;
             assert begin !y = 0 end;
             ?>f (Some 3);
             assert begin !y = 3 end;
             true
           end
