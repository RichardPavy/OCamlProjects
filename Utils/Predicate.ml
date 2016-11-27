type 'a t = 'a -> bool

let always_true () = fun _ -> true
let always_false () = fun _ -> false

let regexp regexp =
  let regexp = Str.regexp regexp in
  fun candidate -> Str.string_match regexp candidate 0
		   && candidate = Str.matched_string candidate

let equals string = (=) string

let oneof list = fun x -> List.mem x list

let basename basename =
  let match_basename = regexp basename in
  fun candidate -> candidate |> File.basename |> match_basename

let extension extension =
  let match_extension = regexp extension in
  fun candidate -> candidate |> File.extension |> match_extension

let filename filename =
  let match_filename = regexp filename in
  fun candidate -> candidate |> File.filename |> match_filename

let full_base full_base =
  let match_full_base = regexp full_base in
  fun candidate -> candidate |> File.strip_ext |> File.to_string |> match_full_base

let parent parent =
  let match_parent = regexp parent in
  fun candidate -> candidate |> File.parent |> File.to_string |> match_parent

module Infix = struct
  let ( &&$ ) p1 p2 = fun candidate -> p1 candidate && p2 candidate
  let ( ||$ ) p1 p2 = fun candidate -> p1 candidate || p2 candidate
  let ( !$ ) p = fun candidate -> not (p candidate)
  let ( !&&$ ) predicates = predicates |> Iterable.fold ( &&$ ) (fun _ -> true)
  let ( !||$ ) predicates = predicates |> Iterable.fold ( ||$ ) (fun _ -> false)
end

let () =
  let open Infix in
  assert begin
      let p = extension "exe" &&$ (parent "binary" ||$ !$(parent "bin.*")) in
      let p path = path |> File.parse |> p in
      true
      && p "binary/file.exe"
      && not (p "binaro/file.exe")
      && not (p "binary/file.exec")
      && not (p "binary/file.Xexe")
      && not (p "binari/file.exe")
      && p "tertiary/file.exe"
    end;
  assert begin
      let p = extension {|mli\|ml|} in
      let p path = path |> File.parse |> p in
      p "file.ml" && p "file.mli" && not (p "file") && not (p "file.mmli")
    end;
  assert begin
      let p = extension {|ml\|mli|} in
      let p path = path |> File.parse |> p in
      not (p "file.mli") (* Matches on 'ml' before even trying 'mli'. *)
    end;
  assert begin
      let p = extension {|\(ml\|mli\)$|} in
      let p path = path |> File.parse |> p in
      p "file.ml" (* Matches on 'ml' before even trying 'mli'. *)
    end
