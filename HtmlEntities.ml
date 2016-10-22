let substr_scanning_channel string start count =
  let cur = ref start in
  let endpos = min (start + count) (String.length string) in
  Scanf.Scanning.from_function
    begin fun () ->
	  if !cur >= endpos then
	    raise End_of_file
	  else
	    let c = string.[!cur] in
	    incr cur; c
    end
let () = assert ("cde" = Scanf.bscanf (substr_scanning_channel "abcdefghijk" 2 3) "%s" (fun x->x))

let encode string =
  let l = String.length string in
  let b = Buffer.create (2 * l) in
  for i = 0 to l - 1 do
    match string.[i] with
    | '<' -> Buffer.add_string b "&lt;"
    | '>' -> Buffer.add_string b "&gt;"
    | '&' -> Buffer.add_string b "&amp;"
    | '\'' -> Buffer.add_string b "&apos;"
    | '"' -> Buffer.add_string b "&quot;"
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b
let () =
  assert (encode "<>&'\"é" = "&lt;&gt;&amp;&apos;&quot;é");
  assert (encode "&amp;" = "&amp;amp;")

let codepoint_to_string x =
  if x < 0x00000080 then
    String.make 1 (char_of_int x)
  else
    let bytes, prefix =
      if x < 0x00000800 then
	2, 0b11000000
      else if x < 0x00010000 then
	3, 0b11100000
      else if x < 0x00200000 then
	4, 0b11110000
      else if x < 0x04000000 then
	5, 0b11111000
      else if x < 0x80000000 then
	6, 0b11111100
      else failwith ("Invalid codepoint " ^ string_of_int x)
    in
    let s = Bytes.create bytes in
    let rec aux i x =
      let q, r = x / 0b1000000, x mod 0b1000000 in
      if i <> 0 then
	(Bytes.set s i (char_of_int (0b10000000 + r));
	 aux (i-1) q)
      else
	Bytes.set s i (char_of_int (prefix + r));
    in
    aux (bytes-1) x;
    Bytes.unsafe_to_string s
let () =
  assert ("©" = codepoint_to_string 0xA9);
  assert ("🤓" = codepoint_to_string 0x1F913)

let entity_to_utf8 = function
  | "amp" -> "&"
  | "lt" -> "<"
  | "gt" -> ">"
  | "Agrave" -> "À"
  | "Aacute" -> "Á"
  | "Acirc" -> "Â"
  | "Atilde" -> "Ã"
  | "Auml" -> "Ä"
  | "Aring" -> "Å"
  | "AElig" -> "Æ"
  | "Ccedil" -> "Ç"
  | "Egrave" -> "È"
  | "Eacute" -> "É"
  | "Ecirc" -> "Ê"
  | "Euml" -> "Ë"
  | "Igrave" -> "Ì"
  | "Iacute" -> "Í"
  | "Icirc" -> "Î"
  | "Iuml" -> "Ï"
  | "ETH" -> "Ð"
  | "Ntilde" -> "Ñ"
  | "Ograve" -> "Ò"
  | "Oacute" -> "Ó"
  | "Ocirc" -> "Ô"
  | "Otilde" -> "Õ"
  | "Ouml" -> "Ö"
  | "Oslash" -> "Ø"
  | "Ugrave" -> "Ù"
  | "Uacute" -> "Ú"
  | "Ucirc" -> "Û"
  | "Uuml" -> "Ü"
  | "Yacute" -> "Ý"
  | "THORN" -> "Þ"
  | "szlig" -> "ß"
  | "agrave" -> "à"
  | "aacute" -> "á"
  | "acirc" -> "â"
  | "atilde" -> "ã"
  | "auml" -> "ä"
  | "aring" -> "å"
  | "aelig" -> "æ"
  | "ccedil" -> "ç"
  | "egrave" -> "è"
  | "eacute" -> "é"
  | "ecirc" -> "ê"
  | "euml" -> "ë"
  | "igrave" -> "ì"
  | "iacute" -> "í"
  | "icirc" -> "î"
  | "iuml" -> "ï"
  | "eth" -> "ð"
  | "ntilde" -> "ñ"
  | "ograve" -> "ò"
  | "oacute" -> "ó"
  | "ocirc" -> "ô"
  | "otilde" -> "õ"
  | "ouml" -> "ö"
  | "oslash" -> "ø"
  | "ugrave" -> "ù"
  | "uacute" -> "ú"
  | "ucirc" -> "û"
  | "uuml" -> "ü"
  | "yacute" -> "ý"
  | "thorn" -> "þ"
  | "yuml" -> "ÿ"
  | "nbsp" -> " "
  | "iexcl" -> "¡"
  | "cent" -> "¢"
  | "pound" -> "£"
  | "curren" -> "¤"
  | "yen" -> "¥"
  | "brvbar" -> "¦"
  | "sect" -> "§"
  | "uml" -> "¨"
  | "copy" -> "©"
  | "ordf" -> "ª"
  | "laquo" -> "«"
  | "not" -> "¬"
  | "shy" -> ""
  | "reg" -> "®"
  | "macr" -> "¯"
  | "deg" -> "°"
  | "plusmn" -> "±"
  | "sup2" -> "²"
  | "sup3" -> "³"
  | "acute" -> "´"
  | "micro" -> "µ"
  | "para" -> "¶"
  | "cedil" -> "¸"
  | "sup1" -> "¹"
  | "ordm" -> "º"
  | "raquo" -> "»"
  | "frac14" -> "¼"
  | "frac12" -> "½"
  | "frac34" -> "¾"
  | "iquest" -> "¿"
  | "times" -> "×"
  | "divide" -> "÷"
  | "forall" -> "∀"
  | "part" -> "∂"
  | "exist" -> "∃"
  | "empty" -> "∅"
  | "nabla" -> "∇"
  | "isin" -> "∈"
  | "notin" -> "∉"
  | "ni" -> "∋"
  | "prod" -> "∏"
  | "sum" -> "∑"
  | "minus" -> "−"
  | "lowast" -> "∗"
  | "radic" -> "√"
  | "prop" -> "∝"
  | "infin" -> "∞"
  | "ang" -> "∠"
  | "and" -> "∧"
  | "or" -> "∨"
  | "cap" -> "∩"
  | "cup" -> "∪"
  | "int" -> "∫"
  | "there4" -> "∴"
  | "sim" -> "∼"
  | "cong" -> "≅"
  | "asymp" -> "≈"
  | "ne" -> "≠"
  | "equiv" -> "≡"
  | "le" -> "≤"
  | "ge" -> "≥"
  | "sub" -> "⊂"
  | "sup" -> "⊃"
  | "nsub" -> "⊄"
  | "sube" -> "⊆"
  | "supe" -> "⊇"
  | "oplus" -> "⊕"
  | "otimes" -> "⊗"
  | "perp" -> "⊥"
  | "sdot" -> "⋅"
  | "Alpha" -> "Α"
  | "Beta" -> "Β"
  | "Gamma" -> "Γ"
  | "Delta" -> "Δ"
  | "Epsilon" -> "Ε"
  | "Zeta" -> "Ζ"
  | "Eta" -> "Η"
  | "Theta" -> "Θ"
  | "Iota" -> "Ι"
  | "Kappa" -> "Κ"
  | "Lambda" -> "Λ"
  | "Mu" -> "Μ"
  | "Nu" -> "Ν"
  | "Xi" -> "Ξ"
  | "Omicron" -> "Ο"
  | "Pi" -> "Π"
  | "Rho" -> "Ρ"
  | "Sigma" -> "Σ"
  | "Tau" -> "Τ"
  | "Upsilon" -> "Υ"
  | "Phi" -> "Φ"
  | "Chi" -> "Χ"
  | "Psi" -> "Ψ"
  | "Omega" -> "Ω"
  | "alpha" -> "α"
  | "beta" -> "β"
  | "gamma" -> "γ"
  | "delta" -> "δ"
  | "epsilon" -> "ε"
  | "zeta" -> "ζ"
  | "eta" -> "η"
  | "theta" -> "θ"
  | "iota" -> "ι"
  | "kappa" -> "κ"
  | "lambda" -> "λ"
  | "mu" -> "μ"
  | "nu" -> "ν"
  | "xi" -> "ξ"
  | "omicron" -> "ο"
  | "pi" -> "π"
  | "rho" -> "ρ"
  | "sigmaf" -> "ς"
  | "sigma" -> "σ"
  | "tau" -> "τ"
  | "upsilon" -> "υ"
  | "phi" -> "φ"
  | "chi" -> "χ"
  | "psi" -> "ψ"
  | "omega" -> "ω"
  | "thetasym" -> "ϑ"
  | "upsih" -> "ϒ"
  | "piv" -> "ϖ"
  | "OElig" -> "Œ"
  | "oelig" -> "œ"
  | "Scaron" -> "Š"
  | "scaron" -> "š"
  | "Yuml" -> "Ÿ"
  | "fnof" -> "ƒ"
  | "circ" -> "ˆ"
  | "tilde" -> "˜"
  | "ensp" -> " "
  | "emsp" -> " "
  | "thinsp" -> " "
  | "zwnj" -> "‌"
  | "zwj" -> "‍"
  | "lrm" -> "‎"
  | "rlm" -> "‏"
  | "ndash" -> "–"
  | "mdash" -> "—"
  | "lsquo" -> ""
  | "rsquo" -> ""
  | "sbquo" -> ""
  | "ldquo" -> "“"
  | "rdquo" -> "”"
  | "bdquo" -> "„"
  | "dagger" -> "†"
  | "Dagger" -> "‡"
  | "bull" -> "•"
  | "hellip" -> "…"
  | "permil" -> "‰"
  | "prime" -> "′"
  | "Prime" -> "″"
  | "lsaquo" -> "‹"
  | "rsaquo" -> "›"
  | "oline" -> "‾"
  | "euro" -> "€"
  | "trade" -> "™"
  | "larr" -> "←"
  | "uarr" -> "↑"
  | "rarr" -> "→"
  | "darr" -> "↓"
  | "harr" -> "↔"
  | "crarr" -> "↵"
  | "lceil" -> "⌈"
  | "rceil" -> "⌉"
  | "lfloor" -> "⌊"
  | "rfloor" -> "⌋"
  | "loz" -> "◊"
  | "spades" -> "♠"
  | "clubs" -> "♣"
  | "hearts" -> "♥"
  | "diams" -> "♦"
  | _ -> raise Not_found

let decode html =
  let l = String.length html in
  let b = Buffer.create l in
  let rec parse_normal i =
    begin match html.[i] with
	  | '&' -> parse_special (i+1)
	  | c -> Buffer.add_char b c; parse_normal (i+1)
	  | exception (Invalid_argument _) -> ()
    end
  and parse_special i =
    begin match String.index_from html i ';' with
	  | p -> parse_special_character i p
	  | exception Not_found -> Buffer.add_char b '&'; parse_normal i
    end
  and parse_special_character i end_of_special =
    match html.[i] with
    | '#' -> parse_special_number (i+1) end_of_special
    | _ ->
       let entity = String.sub html i (end_of_special - i) in
       begin match entity_to_utf8 entity with
	     | c -> Buffer.add_string b c; parse_normal (end_of_special + 1)
	     | exception Not_found -> Buffer.add_char b '&'; parse_normal i
       end
  and parse_special_number i end_of_special =
    try
      let utf8_character =
	try match html.[i] with
	    | 'x' | 'X' -> Scanf.bscanf (substr_scanning_channel html (i+1) 10) "%x;" codepoint_to_string
	    | _ -> Scanf.bscanf (substr_scanning_channel html i 10) "%u;" codepoint_to_string
	with _ -> raise Exit
      in
      Buffer.add_string b utf8_character;
      parse_normal (end_of_special + 1)
    with Exit -> Buffer.add_string b "&#"; parse_normal i
  in
  parse_normal 0;
  Buffer.contents b
let _ =
  assert (decode "abcd &amp; +-&times;&#247; &Omega;" = "abcd & +-×÷ Ω");
  assert (decode "&amp; &hkl&amp; &#abc; &amp" = "& &hkl& &#abc; &amp")
