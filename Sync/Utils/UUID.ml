let gen_uuid =
  let base = 62
  and _0 = int_of_char '0'
  and _A = int_of_char 'A'
  and _a = int_of_char 'a'
  in

  let rec append_chars buffer n =
    let digit = n mod base
    and n' = n / base in
    if digit < 10
    then Buffer.add_char buffer (char_of_int (_0 + digit))
    else if digit < 36
    then Buffer.add_char buffer (char_of_int (_A + digit - 10))
    else Buffer.add_char buffer (char_of_int (_a + digit - 36));

    if n' <> 0
    then append_chars buffer n'
  in

  function () ->
	   let buffer = Buffer.create 30 in
	   append_chars buffer (Random.bits ());
	   for i = 1 to 5 do
	     Buffer.add_char buffer '-';
	     append_chars buffer (Random.bits ());
	   done;
	   Buffer.contents buffer

let () =
  assert begin
      let module Foldable = Utils_Foldable in
      Foldable.cycle ()
      |> Foldable.top 10
      |> Foldable.map gen_uuid
      |> Foldable.map
           begin fun uuid ->
           Str.string_match
             (let chunk = "\\([A-Za-z0-9]+\\)" in
              Printf.sprintf "^%s-%s-%s-%s-%s-%s$"
                             chunk chunk chunk chunk chunk chunk
              |> Str.regexp)
             uuid
             0
           end
      |> Foldable.all (fun t -> t)
    end
