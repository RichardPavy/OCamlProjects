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
 and path = string
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
  path %> (query %? []) %$ fun (path, query) -> { path ; query }

let target_of_string ?plus q =
  let { path ; query } =
    let open Parser.Infix in
    target_parser %< q
  in
  { path = decode ?plus path ;
    query = List.rev_map
	      (fun (k, v) -> decode ?plus k, decode ?plus v)
	      query
	    |> List.rev }

let string_of_target ?plus { path ; query } =
  let b = Buffer.create 10 in
  let add_string s = Buffer.add_string b (encode ?plus s) in
  add_string path;
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
  assert begin
      true
      && target_of_string "abc+?def+=0" = { path = "abc " ; query = [ "def ", "0" ] }
      && target_of_string "abc%20?" = { path = "abc " ; query = [] }
      && target_of_string "abc%20" =  { path = "abc " ; query = [] }
      && target_of_string "?abc=20&" =  { path = "" ; query = [ "abc", "20" ] }
      && target_of_string "?abc=20&abcd=21" = { path = "" ; query = [ "abc", "20" ; "abcd", "21"] }
      && string_of_target { path = "abc " ; query = [ "def ", "0" ] } = "abc+?def+=0"
      && string_of_target { path = "abc " ;
			    query = [ "def ", "0" ; "abc", "efg" ; "hij", "klm" ] }
	 = "abc+?def+=0&abc=efg&hij=klm"
      && string_of_target { path = "abc " ; query = [] } = "abc+"
      && let check ?plus x = x = (x |> target_of_string ?plus |> string_of_target ?plus) in
	 true
	 && check ~plus:true "abc+?def+=0&abc=efg&hij=klm"
	 && check ~plus:false "abc%20?def%20=0&abc%20=efg&hij=klm"
	 && check "abcdef"
    end
