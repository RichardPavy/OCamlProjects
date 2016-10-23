let split_path path =
  let length = String.length path in
  let add_leg accu start i =
    if i == start then
      accu
    else
      (String.sub path start (i-start)) :: accu
  in
  let rec aux accu start i =
    if i = length then
      add_leg accu start i
    else if path.[i] = '/' then
      let accu = add_leg accu start i in
      let start = i + 1 in
      aux accu start start
    else if path.[i] = '\\' then
      aux_skip accu start (i+1)
    else
      aux accu start (i+1)
  and aux_skip accu start i =
    if i = length then
      add_leg accu start i
    else
      aux accu start (i+1)
  in aux [] 0 0

and join ?(prefix = "") ?(suffix = "") ?(absolute = true) = function
  | [] -> prefix ^ if absolute then "/" ^ suffix else suffix
  | path ->
     let prefixl = String.length prefix
     and suffixl = String.length suffix in
     let length =
       prefixl
       + suffixl
       + List.fold_left
	   (fun n node -> n + 1 + String.length node)
	   (if absolute then 0 else -1) path
     in
     let result = Bytes.make length ' ' in
     Bytes.blit_string prefix 0 result 0 prefixl;
     Bytes.blit_string suffix 0 result (length - suffixl) suffixl;
     let _ = List.fold_left
	       begin fun n node ->
		     let node_length = String.length node in
		     let start = n - 1 - node_length in
		     Bytes.blit_string node 0 result (start + 1) node_length;
		     if start >= 0 then Bytes.set result start '/';
		     start
	       end
	       (length - suffixl)
	       path
     in Bytes.to_string result

let () =
  assert (split_path "" = []);
  assert (split_path "/" = []);
  assert (split_path "//" = []);
  assert (split_path "///" = []);
  assert (split_path "aa" = ["aa"]);
  assert (split_path "/aa" = ["aa"]);
  assert (split_path "aa/" = ["aa"]);
  assert (split_path "/aa/" = ["aa"]);
  assert (split_path "/aa//" = ["aa"]);
  assert (split_path "aa/bb" = ["bb"; "aa"]);
  assert (split_path "/aa/bb" = ["bb"; "aa"]);
  assert (split_path "/aa/bb/" = ["bb"; "aa"]);
  assert (split_path "aa/bb/" = ["bb"; "aa"]);
  assert (split_path "aa/bb//" = ["bb"; "aa"]);
  assert (split_path "a/b/c/d" = ["d"; "c"; "b"; "a"]);
  assert (split_path "a\\/b/c\\/d" = ["c\\/d"; "a\\/b"]);
  assert (join ["bb"; "aa"] = "/aa/bb");
  assert (join [] = "/");
  assert (join ~absolute: false ["aa"] = "aa");
  assert (join ~absolute: false ["aa"; "bb"] = "bb/aa");
  assert (join ~absolute: false [] = "");
  assert (join  ~prefix: "<<" ~suffix: ">>" ["bb"; "aa"] = "<</aa/bb>>")

(** Extracts the base file name let the file extension. *)
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

type t = { folder : string list ;
	   basename : string ;
	   extension : string option ;
	   absolute : bool }

let root = { folder = [] ; basename = "" ; extension = None ; absolute = false }

let is_absolute { absolute } = absolute
let is_root = function
  | { folder = [] ; basename = "" ; extension = None } -> true
  | _ -> false
let is_toplevel = function
  | { folder = [] } -> true
  | _ -> false

let parse path =
  let absolute = String.length path > 0 && path.[0] = '/' in
  match split_path path with
  | [] -> { root with absolute }
  | filename :: folder ->
     let basename, extension = split_filename filename in
     { folder ; basename ; extension ; absolute }

let parsef format = Printf.ksprintf parse format

let filename path =
  match path.extension with
  | None -> path.basename
  | Some ext -> path.basename ^ "." ^ ext

let parent path =
  match path.folder with
  | [] -> { root with absolute = path.absolute }
  | filename :: folder ->
     let basename, extension = split_filename filename in
     { folder ; basename ; extension ; absolute = path.absolute }

let child parent childname =
  let basename, extension = split_filename childname in
  { folder = if is_root parent
	     then []
	     else filename parent :: parent.folder ;
    basename ;
    extension ;
    absolute = parent.absolute }

let strip_ext path = { path with extension = None }
let with_ext ext path = { path with extension = Some ext }

let to_string path =
  join
    ~suffix: (match path.extension with None -> "" | Some ext -> "." ^ ext)
    ~absolute: path.absolute
    (path.basename :: path.folder)

let basename path = path.basename
let full_base path = path |> strip_ext |> to_string
let extension path = match path.extension with None -> "" | Some ext -> ext

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
