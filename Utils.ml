open Lwt.Infix

let () = Random.self_init ()
(* let () = Lwt_engine.set (new Lwt_engine.libev) *)

let async name callback =
  Lwt.async begin
      fun () -> Lwt.catch
		  callback
		  begin fun exn ->
			Lwt_io.eprintlf "Failure in %s: %s"
					name (Printexc.to_string exn)
		  end
    end

let fail message = Lwt_io.eprintl message >>= fun () -> Lwt.fail_with message

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

let time f =
  let start = Unix.gettimeofday () in
  let () = f () in
  Unix.gettimeofday () -. start

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

and join = function
  | [] -> "/"
  | path ->
     let length = List.fold_left
		    (fun n node -> n + 1 + String.length node)
		    0
		    path
     in
     let result = Bytes.make length ' ' in
     let _ = List.fold_left
	       begin fun n node ->
		     let node_length = String.length node in
		     let start = n - 1 - node_length in
		     Bytes.blit_string node 0 result (start + 1) node_length;
		     Bytes.set result start '/';
		     start
	       end
	       length
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
  assert (split_path "a/b/c/d" = ["d"; "c"; "b"; "a"])

let () =
  assert (join ["bb"; "aa"] = "/aa/bb");
  assert (join [] = "/")
