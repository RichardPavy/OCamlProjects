module Slice = Container_Slice

let () = assert (Log.dlog "Testing File")

let split_path path =
  let slice = Slice.create () in
  let length = String.length path in
  let add_leg start i =
    if i <> start
    then Slice.add slice (String.sub path start (i-start))
  in
  let rec aux start i =
    if i = length then
      add_leg start i
    else if path.[i] = '/' then
      (add_leg start i;
       let start = i + 1 in
       aux start start)
    else if path.[i] = '\\' then
      aux_skip start (i+1)
    else
      aux start (i+1)
  and aux_skip start i =
    if i = length then
      add_leg start i
    else
      aux start (i+1)
  in
  aux 0 0;
  slice

and join ?(prefix = "") ?(suffix = "") ?(absolute = true) foldable =
  let prefixl = String.length prefix
  and suffixl = String.length suffix in
  let length =
    prefixl
    + suffixl
    + let l = Foldable.fold
                (fun n node -> n + 1 + String.length node)
	        (if absolute then 0 else -1)
                foldable
      in max l (if absolute then 1 else 0)
  in
  let result = Bytes.make length '/' in
  Bytes.blit_string prefix 0 result 0 prefixl;
  Bytes.blit_string suffix 0 result (length - suffixl) suffixl;
  foldable
  |> Foldable.fold
       begin fun n node ->
       let node_length = String.length node in
       Bytes.blit_string node 0 result (n + 1) node_length;
       n + 1 + node_length
       end
       (if absolute then prefixl else prefixl - 1)
  |> ignore;
  Bytes.to_string result

let () =
  assert begin
      let assert_split_path path expected =
        assert (split_path path |> Slice.to_list = expected)
      in
      assert_split_path "" [];
      assert_split_path "/" [];
      assert_split_path "//" [];
      assert_split_path "///" [];
      assert_split_path "aa" [ "aa" ];
      assert_split_path "/aa" [ "aa" ];
      assert_split_path "aa/" [ "aa" ];
      assert_split_path "/aa/" [ "aa" ];
      assert_split_path "/aa//" [ "aa" ];
      assert_split_path "aa/bb" [ "aa"; "bb" ];
      assert_split_path "/aa/bb" [ "aa"; "bb" ];
      assert_split_path "/aa/bb/" [ "aa"; "bb" ];
      assert_split_path "aa/bb/" [ "aa"; "bb" ];
      assert_split_path "aa/bb//" [ "aa"; "bb" ];
      assert_split_path "a/b/c/d" [ "a"; "b"; "c"; "d" ];
      assert_split_path "a\\/b/c\\/d" [ "a\\/b"; "c\\/d" ];
      let assert_join ?prefix ?suffix ?absolute path expected =
        assert (path |> Foldable.of_list |> join ?prefix ?suffix ?absolute = expected)
      in
      assert_join [ "aa"; "bb" ] "/aa/bb";
      assert_join [] "/";
      assert_join ~absolute: false [ "aa" ] "aa";
      assert_join ~absolute: false [ "aa"; "bb"] "aa/bb";
      assert_join ~absolute: false [] "";
      assert_join ~prefix: "<<" ~suffix: ">>" [ "aa"; "bb" ] "<</aa/bb>>";
      true
    end

(** Extracts the base file name and the file extension. *)
let split_filename filename =
  match String.rindex filename '.'
  with pos -> String.sub filename 0 pos,
	      Some (String.sub filename (pos + 1) (String.length filename - pos - 1))
     | exception Not_found -> filename, None

let () = assert (("file", Some "ml") = split_filename "file.ml");
	 assert (("file.truc", Some "ml") = split_filename "file.truc.ml");
	 assert (("", Some "ml") = split_filename ".ml");
	 assert (("file", Some "") = split_filename "file.");
	 assert (("file", None) = split_filename "file")

type t = { folder : string Slice.t ;
	   basename : string ;
	   extension : string option ;
	   absolute : bool }

let root = { folder = Slice.create ~capacity:0 () ;
             basename = "" ;
             extension = None ;
             absolute = false }

let is_absolute { absolute } = absolute
let is_root = function
  | { folder ; basename = "" ; extension = None } when Slice.length folder = 0 -> true
  | _ -> false
let is_toplevel path = Slice.length path.folder = 0

let parse path =
  let absolute = String.length path > 0 && path.[0] = '/' in
  let path = split_path path in
  if Slice.length path = 0 then
    { root with absolute }
  else
    let l = Slice.length path - 1 in
    let folder = Slice.sub path 0 l
    and filename = Slice.get path l in
    let basename, extension = split_filename filename in
    { folder ; basename ; extension ; absolute }

let parsef format = Printf.ksprintf parse format

let filename path =
  match path.extension with
  | None -> path.basename
  | Some ext -> path.basename ^ "." ^ ext

let parent ({ folder } as path) =
  if Slice.length folder = 0 then
    { root with absolute = path.absolute }
  else
    let l = Slice.length folder - 1 in
    let folder = Slice.sub folder 0 l
    and filename = Slice.get folder l in
    let basename, extension = split_filename filename in
    { folder ; basename ; extension ; absolute = path.absolute }

let child parent childname =
  let basename, extension = split_filename childname in
  { folder = (if is_root parent
	      then root.folder
	      else let folder = Slice.create ~capacity: (1 + Slice.length parent.folder) () in
                   Slice.add_all folder (Slice.to_iterable parent.folder);
                   Slice.add folder (filename parent);
                   folder) ;
    basename ;
    extension ;
    absolute = parent.absolute }

let strip_ext path = { path with extension = None }
let with_ext ext path = { path with extension = Some ext }

let to_string path =
  join
    ~suffix: (match path.extension with None -> "" | Some ext -> "." ^ ext)
    ~absolute: path.absolute
    (Foldable.concat
       (Slice.to_foldable path.folder)
       (Foldable.singleton path.basename))

let equals a b =
  a.basename = b.basename
  && a.extension = b.extension
  && a.absolute = b.absolute
  && Slice.equals a.folder b.folder

let () =
  assert begin
      child ("/a/b/c" |> parse |> parent)
            "d"
      |> equals ("/a/b/d" |> parse)
    end

let basename path = path.basename
let full_base path = path |> strip_ext |> to_string
let extension path = match path.extension with None -> "" | Some ext -> ext

let chroot levels path =
  { path with folder = Slice.sub path.folder
                                 levels
                                 (Slice.length path.folder - levels) }

let map f path =
  let basename, extension = path |> filename |> f |> split_filename
  and folder = Slice.create ~capacity: (Slice.length path.folder) () in
  path.folder
  |> Slice.to_iterable
  |> Iterable.map f
  |> Slice.add_all folder;
  { path with folder ; basename ; extension }

let () =
  assert begin
      "/a/b/c" |> parse |> map String.uppercase_ascii
      |> equals ("/A/B/C" |> parse)
    end

let () =
  assert (parse "/a/b/c/d" |> chroot 2
          |> equals (parse "/c/d"));
  assert (parse "/a/b/c/d/e" |> chroot 2
          |> equals (parse "/c/d")
          |> not)

let () = assert begin
	     let ident path = to_string (parse path) in
	     let check path = path = ident path in
	     true
	     && check "/"
	     && check "/abc"
	     && check "/abc."
	     && check "/abc/def"
	     && check "/abc/def."
	     && check "/abc/def.exe"
	     && check "/abc/.exe"
	     && check ""
	     && check "abc"
	     && check "abc."
	     && check "abc/def"
	     && check "abc/def."
	     && check "abc/def.exe"
	     && check "abc/.exe"
	   end
