module Y = Yojson.Basic

type builtin_exn =
  | Unknown of string
  | System of string
  | Unexpected
exception BuiltinException of builtin_exn

let rec exn_tag : exn -> int = fun exn ->
  match exn with
  | Out_of_memory
  | Sys_error _
  | Failure _
  | Invalid_argument _
  | End_of_file
  | Division_by_zero
  | Not_found
  | Match_failure _
  | Sys_blocked_io
  | Stack_overflow
  | Assert_failure _
  | Undefined_recursive_module _ -> exn_tag (BuiltinException Unexpected)
  | _ ->
     let tag =
       let exn = Obj.repr exn in
       if Obj.tag exn = Obj.object_tag then
	 (Obj.field exn 1) |> Obj.magic
       else
	 Obj.magic (Obj.field (Obj.field exn 0) 1)
     in if 0 <= tag && tag < 256
	then tag
	else exn_tag (BuiltinException Unexpected)

let sys_exn_tag = exn_tag (BuiltinException Unexpected)
let () = assert (try failwith "" with e -> exn_tag e = sys_exn_tag)

let default_json_to_exn json =
  let open Y.Util in
  match json |> (member "kind") |> to_string with
  | "Unknown" -> BuiltinException (Unknown (json |> (member "value") |> to_string))
  | "System" -> BuiltinException (System (json |> (member "value") |> to_string))
  | "Unexpected" -> BuiltinException Unexpected
  | kind -> failwith ("Unexpected builtin exception: " ^ kind)

let default_exn_to_json exn =
  match exn with
  | BuiltinException (Unknown e) -> `Assoc [ "kind", `String "Unknown" ;
					     "value", `String e ]
  | BuiltinException (System e) -> `Assoc [ "kind", `String "System" ;
					    "value", `String e ]
  | BuiltinException Unexpected -> `Assoc [ "kind", `String "Unexpected" ]
  | _ -> `Assoc [ "kind", `String (if sys_exn_tag = exn_tag exn then "System" else "Unknown") ;
		  "value", `String (Printexc.to_string exn) ]

let json_to_exn_translators = Array.make 256 default_json_to_exn
let exn_to_json_translators = Array.make 256 default_exn_to_json

let register_translator exn_example ~to_exn ~to_json =
  let exn_tag = exn_tag exn_example in
  json_to_exn_translators.(exn_tag) <- to_exn;
  exn_to_json_translators.(exn_tag) <- to_json

let to_exn json =
  let open Y.Util in
  let exn_tag = json |> (member "exn_tag") |> to_int in
  json |> (member "exn_value") |> json_to_exn_translators.(exn_tag)

let to_json exn =
  let exn_tag = exn_tag exn in
  let exn_value = exn_to_json_translators.(exn_tag) exn in
  `Assoc [ "exn_tag", `Int exn_tag ;
	   "exn_value", exn_value ]

let repr json =
  let open Y.Util in
  json |> (member "exn_value")

let () =
  let open Y.Util in
  register_translator
    (Unix.Unix_error (Unix.ECONNREFUSED, "", ""))
    ~to_exn: (fun json -> Unix.Unix_error(
			      json |> (member "error") |> to_int |> Obj.magic,
			      json |> (member "name") |> to_string,
			      json |> (member "arg") |> to_string))
    ~to_json: (function
		| Unix.Unix_error(error, name, arg) ->
		   `Assoc [
		      "error", `Int (Obj.magic error) ;
		      "error_message", `String (Unix.error_message error) ;
		      "name", `String name ;
		      "arg", `String arg
		    ]
		| exn -> failwith "Unexpected unix error.");

  register_translator
    Protocol.Timeout
    ~to_exn: (fun _ -> Protocol.Timeout)
    ~to_json: (fun _ -> `String "Timeout error");

  register_translator
    Exit
    ~to_exn: (fun _ -> Exit)
    ~to_json: (fun _ -> `String "Exit")

let () =
  assert begin
      let success = ref true in
      let test_exn exn =
	if exn = (exn |> to_json |> to_exn)
	then ()
	else success := false
      in
      List.iter
	test_exn
	[ Unix.Unix_error (Unix.ECONNREFUSED, "a", "b") ;
	  Protocol.Timeout ;
	  BuiltinException Unexpected ;
	  BuiltinException (System "test failure") ;
	  Exit ];
      !success
    end
