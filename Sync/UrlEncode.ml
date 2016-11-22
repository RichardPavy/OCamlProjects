module File = Utils_File

let encode ?(plus = true) string =
  let l = String.length string in
  let b = Buffer.create (2 * l) in
  for i = 0 to l - 1 do
    match string.[i] with
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0'  .. '9' as c -> Buffer.add_char b c
    | ' ' when plus -> Buffer.add_char b '+'
    | c -> Printf.bprintf b "%%%X" (int_of_char c)
  done;
  Buffer.contents b

let () =
  assert ("Hello+World%21" = encode ~plus:true "Hello World!");
  assert ("%E2%82%AC" = encode "€")

let decode ?(plus = true) string =
  let l = String.length string in
  let b = Buffer.create l in
  let rec aux i =
    if i = l then
      Buffer.contents b
    else
      match string.[i] with
      | '%' -> Scanf.sscanf
		 (String.sub string (i+1) 2)
		 "%x"
		 char_of_int
	       |> Buffer.add_char b;
	       aux (i+3)
      | '+' when plus -> Buffer.add_char b ' '; aux (i+1)
      | c -> Buffer.add_char b c; aux (i+1)
  in aux 0

let () =
  assert ("Hello World! €" |> encode |> decode = "Hello World! €");
  assert begin
      let s =
	let b = Buffer.create 1000 in
	for i = 0 to 1000 * 100 do
	  Buffer.add_string b "Hello World! €"
	done;
	Buffer.contents b
      in
      s |> encode |> decode = s
    end;
  assert ("Hello World! €" = decode "Hello World! €")

type target = { path : path ;
		query : query }
 and path = File.t
 and query = (string * string) list

let path { path } = path
let query { query } = query

let target_parser =
  let open Parser.Infix in
  let path = !%"[^?]*"
  and key = !%"[^=]+"
  and value = !%"[^&]*" in
  let key_value = key %>. !@"=" %> value in
  let query = !@"?" %.> (key_value %% !@"&") in
  path %> (query %? []) %$ fun (path, query) -> path, query

let target_of_string ?plus q =
  let path, query =
    let open Parser.Infix in
    target_parser %< q
  in
  { path = decode ?plus path |> File.parse ;
    query = List.rev_map
	      (fun (k, v) -> decode ?plus k, decode ?plus v)
	      query
	    |> List.rev }

let string_of_target ?plus { path ; query } =
  let b = Buffer.create 10 in
  path |> File.map (encode ?plus) |> File.to_string |> Buffer.add_string b;
  let add_string s = Buffer.add_string b (encode ?plus s) in
  begin match query with
	| [] -> ()
	| (k, v) :: t ->
	   Buffer.add_char b '?';
	   add_string k;
	   Buffer.add_char b '=';
	   add_string v;
	   List.iter
	     (fun (k, v) -> Buffer.add_char b '&';
			    add_string k;
			    Buffer.add_char b '=';
			    add_string v)
	     t
  end;
  Buffer.contents b

let () =
  assert (Utils_Log.dlog "Testing UrlEncode");
  assert begin
      assert (target_of_string "a/b/c+?def+=0"
              = { path = File.parse "a/b/c " ; query = [ "def ", "0" ] });
      assert (target_of_string "a/b/c%20?"
              = { path = File.parse "a/b/c " ; query = [] });
      assert (target_of_string "a/b/c%20"
              = { path = File.parse "a/b/c " ; query = [] });
      assert (target_of_string "?abc=20&"
              = { path = File.parse "" ; query = [ "abc", "20" ] });
      assert (target_of_string "?abc=20&abcd=21"
              = { path = File.parse "" ; query = [ "abc", "20" ; "abcd", "21"] });
      assert (string_of_target { path = File.parse "a/b/c " ; query = [ "def ", "0" ] }
              = "a/b/c+?def+=0");
      assert (string_of_target { path = File.parse "a/b/c " ;
			         query = [ "def ", "0" ; "abc", "efg" ; "hij", "klm" ] }
	      = "a/b/c+?def+=0&abc=efg&hij=klm");
      assert (string_of_target { path = File.parse "a/b/c " ; query = [] }
              = "a/b/c+");
      let check ?plus x = x = (x |> target_of_string ?plus |> string_of_target ?plus) in
      assert (check ~plus:true "abc+?def+=0&abc=efg&hij=klm");
      assert (check ~plus:false "abc%20?def%20=0&abc%20=efg&hij=klm");
      assert (check "abcdef");
      true
    end
