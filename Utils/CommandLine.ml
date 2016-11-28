module M = Map.Make(struct type t = string let compare = compare end)
let all_args = Hashtbl.create 10
and all_specs = ref M.empty
and initialized = ref false

let reset () =
  Hashtbl.clear all_args;
  all_specs := M.empty;
  initialized := false

let init_aux argv =
  if !initialized then failwith "Flags have already been initialized.";
  reset ();
  let l = Array.length argv in
  let rec aux i =
    if i < l then
      match
	let arg = argv.(i) in
	let l = String.length arg in
	if l = 0 then
	  None
	else if arg.[0] <> '-' then
	  failwith ("This is not a flag: " ^ arg)
	else
	  (** We support both -arg and --arg. *)
	  Some (if l > 1 && arg.[1] = '-'
		then String.sub arg 2 (l - 2)
		else String.sub arg 1 (l - 1))
      with
      | None -> aux (i+1)
      | Some arg ->
	 let value, next =
	   if i = l - 1 ||
		let next = argv.(i + 1) in
		String.length next != 0 && next.[0] = '-'
	   then "", i + 1
	   else argv.(i + 1), i + 2
	 in
	 if Hashtbl.mem all_args arg
	 then failwith ("arg already defined: " ^ arg);
	 Hashtbl.add all_args arg value;
	 aux next
  in
  aux 1;
  initialized := true

let init () = init_aux Sys.argv

let register_specs specs =
  List.iter
    begin fun spec ->
	  let flag, _, _ = spec in
	  if M.mem flag !all_specs
	  then failwith ("Flag " ^ flag ^ " was already defined.");
	  all_specs := M.add flag spec !all_specs
    end
    specs

let rec eval_spec spec value =
  let open Arg in
  match spec with
  | Unit f -> f ()
  | Bool f -> f (bool_of_string value)
  | Set r -> r := true
  | Clear r -> r := false
  | String f -> f value
  | Set_string r -> r := value
  | Int f -> f (int_of_string value)
  | Set_int r -> r := int_of_string value
  | Float f -> f (float_of_string value)
  | Set_float r -> r := float_of_string value
  | Tuple l -> List.iter (fun spec -> eval_spec spec value) l
  | Symbol (symbols, f) -> if List.mem value symbols
			   then f value
			   else failwith ("Unexpected symbol " ^ value)
  | Rest rest -> failwith "Rest arg specs are not implemented"

let print_usage_aux ?(print_string = Log.log "%s") ?(usage = "Usage:") () =
  M.fold
    (fun _ (flag, spec, doc) accu -> ("--" ^ flag, spec, doc) :: accu)
    !all_specs
    []
  |> List.rev
  |> Arg.align
  |> (fun specs -> Arg.usage_string specs usage)
  |> print_string

let print_usage () = print_usage_aux ()

let parse specs =
  if not !initialized then failwith "Flags have not been initialized.";
  register_specs specs;
  List.iter
    begin fun (flag, spec, _) ->
	  match Hashtbl.find all_args flag with
	  | value -> (try eval_spec spec value
		      with e -> print_usage_aux (); raise e)
	  | exception Not_found -> ()
    end
    specs

let verify () =
  if not !initialized then failwith "Flags have not been initialized.";
  let all_specs = !all_specs in
  if Hashtbl.fold
       begin fun arg _ accu ->
	     if M.mem arg all_specs
	     then accu
	     else (Log.log "Unknown command-line arg: %s\n" arg; true)
       end
       all_args
       false
  then
    let error_message = "There are unknown command-line args" in
    print_usage_aux ~usage: error_message ();
    failwith error_message
  else initialized := false

let () =
  assert (Log.dlog "Testing CommandLine");
  assert
    ([ "should not be initialized",
       (fun () -> not !initialized) ;

       "init",
       begin fun () ->
       let module HS = Container_HashSet in
       init_aux [| "Exe" ; "--flag" ; "ok" ; "-boo" ; "--other" ; "-end" |];
       !initialized
       && HS.eq
            ([ "flag", "ok" ;
               "boo", "" ;
               "other", "" ;
               "end", ""
             ] |> HS.of_list)
            (all_args |> Iterable.of_hashtbl |> HS.of_iterable)
       end ;

       "reset",
       (fun () -> reset (); not !initialized) ;

       "init 2",
       begin fun () ->
       let module HS = Container_HashSet in
       init_aux [| "Exe" ; "--flag" ; "ok" ; "-boo" ; "--other" ; "-end" ; "x" |];
       !initialized
       && HS.eq
            ([ "flag", "ok" ;
               "boo", "" ;
               "other", "" ;
               "end", "x"
             ] |> HS.of_list)
            (all_args |> Iterable.of_hashtbl |> HS.of_iterable)
       end ;
     ] |> Asserts.test);

  assert
    (let ip = ref ""
     and port = ref (-1) in
     let specs =
       [ "ip", Arg.Set_string ip, "<ip> IP address" ;
         "port", Arg.Set_int port, "<port> Port number" ]
     and reset () = reset (); ip := ""; port := -1
     in
     assert (reset (); true);
     [ "parse fails if not initialized",
       begin fun () ->
       try parse specs; false
       with Failure _ -> true
       end ;

       "parse success",
       begin fun () ->
       reset ();
       init_aux [| "Program" ; "--ip" ; "192.168.1.1" ; "--port" ; "80" |];
       parse specs;
       !ip = "192.168.1.1" && !port = 80
       && (verify (); true)
       end ;

       "parse, default values for missing command-line flags",
       begin fun () ->
       reset ();
       ip := "default value";
       init_aux [| "Program" ; "--port" ; "80" |];
       parse specs;
       !ip = "default value" && !port = 80
       && (verify (); true)
       end ;

       "verify fails on unknown arguments",
       begin fun () ->
       reset ();
       init_aux [| "Program" ;
                   "--ip" ; "192.168.1.1" ;
                   "--port" ; "80" ;
                   "--unknown" ; "???" |];
       parse specs;
       !ip = "192.168.1.1" && !port = 80
       && (try verify (); true with Failure _ -> true)
       end ;

       "parse arguments separately",
       begin fun () ->
       reset ();
       init_aux [| "Program" ; "--ip" ; "192.168.1.1" ; "--port" ; "80" |];
       parse [ List.hd specs ];
       parse (List.tl specs);
       !ip = "192.168.1.1" && !port = 80
       && (verify (); true)
       end ;

       "print usage",
       begin fun () ->
       reset ();
       init_aux [| |];
       parse specs;
       let usage = ref "" in
       print_usage_aux ~print_string: (fun s -> usage := s)
		       ~usage: "Usage:" ();
       Asserts.equals "%s"
         !usage
         {|Usage:
  --ip <ip>     IP address
  --port <port> Port number
  -help         Display this list of options
  --help        Display this list of options
|}
       end ;
     ] |> Asserts.test);
  assert (reset (); true)

let () = init ()
