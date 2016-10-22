exception ParseError of string
let fail text pos =
  let aux text = raise (ParseError text)
  in (String.length text - pos) |> min 80 |> String.sub text pos |> aux

let () =
  assert begin
      try fail (String.make 100 '-') 3
      with ParseError s -> s = String.make 80 '-'
    end

type 'a parser =
  { kind: kind ;
    matching: string -> int -> bool ;
    parse: string -> int ref -> 'a }
 and ('a, 'b) alt_result = Left of 'a | Right of 'b
 and kind =
   | String of string
   | Regex of Str.regexp
   | Alt of kind * kind
   | Transform of kind
   | Seq of kind * kind
   | Star of kind
   | List of kind * kind
   | Before of kind
   | NotBefore of kind
   | Ref
   | Undef
   | Const

module Builder =
  struct
    let parse_string str =
      let str_length = String.length str in
      let matching text pos =
	let matching_length = min str_length (String.length text - pos) in
	String.sub text pos matching_length = str in
      let parse text pos =
	if matching text !pos
	then (pos := !pos + str_length;
	      str)
	else fail text !pos
      in { kind = String str ; matching ; parse }

    and parse_regex regex =
      let matching text pos = Str.string_match regex text pos in
      let parse text pos =
	if matching text !pos
	then (let matching = Str.matched_string text in
	      pos := !pos + String.length matching;
	      matching)
	else fail text !pos
      in { kind = Regex regex ; matching ; parse }

    and parse_alt p1 p2 =
      let matching text pos = p1.matching text pos || p2.matching text pos
      and parse text pos =
	if p1.matching text !pos
	then Left (p1.parse text pos)
	else Right (p2.parse text pos)
      in { kind = Alt (p1.kind, p2.kind) ; matching ; parse }

    and parse_transform f p =
      let matching = p.matching
      and parse text pos = p.parse text pos |> f
      in { kind = Transform p.kind ; matching ; parse }

    and parse_seq p1 p2 =
      let matching =
	match p1.kind with
	| Before _ | NotBefore _ -> fun text pos -> p1.matching text pos && p2.matching text pos
	| _ -> p1.matching
      and parse text pos =
	let l = p1.parse text pos in
	let r = p2.parse text pos in
	l, r
      in { kind = Seq (p1.kind, p2.kind) ; matching ; parse }

    and parse_star p =
      let rec parse_aux text pos accu =
	if p.matching text !pos
	then let pos_before = !pos in
	     let item = p.parse text pos in
	     (* star parser MUST advance pos at each item. *)
	     if pos_before = !pos
	     then accu
	     else parse_aux text pos (item :: accu)
	else accu
      in
      let matching _ _ = true
      and parse text pos = parse_aux text pos [] |> List.rev
      in { kind = Star p.kind ; matching ; parse }

    and parse_const x =
      let matching _ _ = true
      and parse _ _ = x in
      { kind = Const ; matching ; parse }

    let parse_opt p default =
      parse_transform
	(function Left x -> x | Right y -> y)
	(parse_alt p (parse_const default))

    let parse_plus p =
      parse_seq p (parse_star p)
      |> parse_transform (fun (h, t) -> h :: t)

    and parse_ignore p = p |> parse_transform ignore

    and parse_list p sep =
      let rec aux text pos accu =
	if sep.matching text !pos
	then
	  let cur_pos = !pos in
	  sep.parse text pos |> ignore;
	  if p.matching text !pos
	  then
	    let item = p.parse text pos in
	    aux text pos (item :: accu)
	  else (pos := cur_pos; List.rev accu)
	else List.rev accu
      in
      let aux_parser =
	{ kind = List (p.kind, sep.kind) ;
	  matching = sep.matching ;
	  parse = fun text pos -> aux text pos [] }
      in
      parse_opt
	(parse_seq p aux_parser
	 |> parse_transform (fun (t, q) -> t :: q))
	[]

    let parse_before p =
      { kind = Before p.kind ;
	matching = p.matching ;
	parse = fun text pos -> () }

    let parse_not_before p =
      { kind = NotBefore p.kind ;
	matching = (fun text pos -> not (p.matching text pos)) ;
	parse = fun text pos -> () }

    let parse_ref () =
      let fail _ = failwith "Ref parser is not defined." in
      let p_ref = ref { kind = Undef ; matching = fail ; parse = fail } in
      let matching text pos = !p_ref.matching text pos
      and parse text pos = !p_ref.parse text pos
      in { kind = Ref ; matching ; parse }, fun p -> p_ref := p
  end

module Infix =
  struct
    open Builder

    let ( %$ ) p f = parse_transform f p
    let ( !@ ) = parse_string
    and ( !% ) r = parse_regex (Str.regexp r)
    and ( ?= ) p = parse_before p
    and ( ?<> ) p = parse_not_before p
    and ( %| ) = parse_alt
    and ( %|% ) a b = parse_alt a b %$ function Left x -> x | Right x -> x
    and ( %> ) = parse_seq
    and ( !* ) = parse_star
    and ( !+ ) = parse_plus
    and ( %% ) = parse_list
    and ( != ) = parse_const
    and ( %? ) = parse_opt
    let ( %>. ) p1 p2 = p1 %> p2 %$ fst
    and ( %.> ) p1 p2 = p1 %> p2 %$ snd
    and ( %< ) p s = p.parse s (ref 0)

    and space = !%"[ \r\n\t]+"
    and ispace = !%"[ \r\n\t]*"

    let () =
      assert begin
	  let number = !%"[1-9][0-9]*" %$ int_of_string %$ fun x -> `Num x in
	  let variable = !%"[a-z_][a-zA-Z0-9_$]*" %$ fun x -> `Var x in
	  let token = number %|% variable in
	  let op = !%"[-+*/]" %$ fun s -> String.get s 0 in
	  let expr = number %>. ispace %> op %>. ispace %> token
		     %$ fun ((a,op),b) -> a, op, b
	  in
	  (`Num 10, '+', `Var "x") = expr %< "10 + x"
	end

    let string =
      let sq_string = !@"'" %.> !%"\\(\\\\'\\|[^']\\)*" %>. !@"'"
      and dq_string = !@"\"" %.> !%"\\(\\\\\"\\|[^\"]\\)*" %>. !@"\""
      in (sq_string %|% dq_string)
	 %$ fun escaped_string ->
	    match String.index escaped_string '\\' with
	    | _ ->
	       let l = String.length escaped_string in
	       let b = Buffer.create l in
	       let rec aux i =
		 if i < l then
		   match escaped_string.[i] with
		   | '\\' when i < l - 1 ->
		      let c, next =
			match escaped_string.[i + 1] with
			| 'r' -> '\r', i + 2
			| 'n' -> '\n', i + 2
			| 't' -> '\t', i + 2
			| 'b' -> '\b', i + 2
			| ' ' | '\\' | '\'' | '"' as c -> c, i + 2
			| 'x' when i < l - 3 ->
			   Scanf.sscanf (String.sub escaped_string (i+2) 2) "%x" char_of_int,
			   i + 4
			| '0' .. '9' when i < l - 3 ->
			   String.sub escaped_string (i+1) 3 |> int_of_string |> char_of_int,
			   i + 4
			| _ -> failwith ("Undefined escape sequence in " ^ (String.sub escaped_string i (l-i)))
		      in Buffer.add_char b c; aux next
		   | c -> Buffer.add_char b c; aux (i+1)
	       in aux 0; Buffer.contents b
	    | exception Not_found -> escaped_string

    let () =
      assert begin
	  true
	  && "" = string %< {|""|}
	  && "" = string %< {|''|}
	  && "Hello World!" = string %< {|'Hello World!'|}
	  && {|'"Hello World"'|}
	     = string %< {|"\'\"Hello\ World\"\'"|}
	  && {|'"Hello World"'|}
	     = string %< {|'\'\"Hello\ World\"\''|}
	  && "Hello World!"
	     = string %< {|"Hello\x20World\x21"|}
	  && "Hello World!"
	     = string %< {|"Hello\032World\033"|}
	  && (try string %< {|"Unknown \sequence"|} |> ignore; false
	      with Failure error -> error = {|Undefined escape sequence in \sequence|})
	  && (try string %< {|"Unknown sequence\x1"|} |> ignore; false
	      with Failure error -> error = {|Undefined escape sequence in \x1|})
	  && (try string %< {|"Unknown sequence\10"|} |> ignore; false
	      with Failure error -> error = {|Undefined escape sequence in \10|})
	end

    let integer =
      !%"-?0b[0-1][0-1_]*"
      %|% !%"-?0[xX][0-9a-fA-F][0-9a-fA-F_]*"
      %|% !%"-?0o[0-7][0-7_]*"
      %|% !%"-?[0-9][0-9_]*"
      %$ fun n -> Scanf.sscanf n "%i" (fun n -> n)

    let () =
      assert begin
	  List.for_all
	    (fun s -> 999 = integer %< (s ^ "zzz") && -999 = integer %< ("-" ^ s ^ "zzz"))
	    [ "999" ; "000999" ; "0__0__9__9__9__" ;
	      "0x3e7" ; "0X3E7" ; "0x3E7" ; "0X3e7" ; "0X3_e_7" ; "0x003e7" ; "0X00__3e7" ;
	      "0o1747" ; "0o001_7__4__7_" ;
	      "0b1111100111" ; "0b0011111001_11_" ]
	end

    let float =
      !%"-?[0-9][0-9_]*\\(\\.[0-9_]*\\)?\\([eE][+-]?[0-9][0-9_]*\\)?"
      %$ float_of_string

    let () =
      assert begin
	  List.for_all
	    (fun s -> (float %< (s ^ "zzz")) = (float_of_string s)
		      && (float %< ("-" ^ s ^ "zzz")) = (float_of_string ("-" ^ s)))
	    [ "0" ; "1" ; "10" ; "1_0_" ;
	      "1.0" ; "1.1" ; "1_.0_" ;
	      "1e6" ; "1e+6" ; "1E-6" ; "1E1_0" ; "1e+1_0" ;
	      "1.25E3_3" ]
	end
  end

module Template =
  struct
    open Infix

    type token = { scope: string ; name: string }
    type 'a t =
      | Static of 'a
      | Token of token

    let token_start = "$"
    let trim_token_start =
      let l = String.length token_start in
      fun matched_scope -> String.sub matched_scope l (String.length matched_scope - l)

    let ident = !%"[a-zA-Z_][a-zA-Z0-9_-]*"
    let token scope = !@(token_start ^ scope) %>. !@":" %> ident
		      %$ fun (scope, name) -> Token { scope = trim_token_start scope ; name }
    let cst v = Static v
    let template scope p = (token scope) %|% (p %$ cst)
  end

module Rope =
  struct
    type 'a t = Item of 'a
	      | List of 'a t list
	      | Concat of ('a t * 'a t)

    let rec fold f accu rope =
      match rope with
      | Item item -> f accu item
      | List l -> List.fold_left (fold f) accu (List.rev l)
      | Concat (a, b) -> fold f (fold f accu b) a

    module Infix =
      struct
	let ( ++ ) a b = Concat (a, b)
	let ( ++. ) a b = Concat (a, Item b)
	let ( ++@ ) a b = Concat (a, List b)
	let ( !. ) x = Item x
	let ( !@ ) l = List l
      end
  end
