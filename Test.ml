open Lwt.Infix
open SyncTypes

let has_run = ref false

let () =
  assert begin
      has_run = ref true;
      let () =
	DynamicHtmlPages.register ();
	HttpFileHandler.register ()
      in
      let module X =
	struct
	  module Y = Yojson.Basic
	  module M = Map.Make(struct
				 type t = string
				 let compare = Pervasives.compare
			       end)
	  module JsonOp = Operations.Json

	  type test_case = {
	      connection : string ;
	      query : Y.json ;
	      response : Y.json }

	  let test_ip = "127.0.0.1"
	  let test_timeout = 10.
	  let settings = ref None
	  let errors = ref 0

	  (** Runs a test server on the given port.
              The server automatically dies after a timeout. *)
	  let run_test_server name port =
	    if Lwt_unix.fork () = 0
	    then Lwt_main.run
		   begin
		     Lwt.async (fun () -> Lwt_unix.sleep test_timeout
					  >>= fun () -> Lwt_io.eprintlf
							  "FAIL: On %s:%i, test timeout exceeded: %0.2fs"
							  name port test_timeout
					  >>= fun () -> Lwt_io.flush Lwt_io.stderr
					  >>= fun () -> exit 0);
		     Lwt_io.printlf "Starting server %s" name
		     >>= (fun () -> let { server } = Server.start ~name ~ip:test_ip ~port () in
				    Utils.async
				      (Printf.sprintf "Killing server %s:%i" name port)
				      (fun () -> server.onshutdown >>= (fun () -> exit 0));
				    (* Wait forever. *)
				    let waiter, _ = Lwt.wait () in waiter)
		   end

	  (** Opens connections to servers running on [test_ip] and on the given ports. *)
	  let open_connections ports =
	    let root = Server.create_root ~ip:"fake" ~port:(-1) in
	    Lwt_list.map_s
	      begin fun (name, port) ->
		    Lwt_io.printlf "Open connection %s on %s:%i" name test_ip port
		    >>= (fun () -> Rpc.open_connection ~ip:test_ip ~port root)
		    >|= fun connection -> name, { connection with peer_name = name }
	      end
	      ports

	  (** Runs tests.
              Scans all test cases to list required test servers,
              Starts all test servers,
              Runs the tests,
              Stops the test servers. *)
	  let run_tests test_cases =
	    (* List required test servers. *)
	    let servers =
	      List.fold_left
		begin fun servers test_case ->
		      if M.mem test_case.connection servers
		      then servers
		      else M.add test_case.connection (8080 + M.cardinal servers) servers
		end
		M.empty
		test_cases
	    in
	    let list_map f l = List.rev (List.rev_map f l) in
	    let rec map_json f = function
	      | `Port _ as p -> f p
	      | `Assoc l -> `Assoc (list_map (fun (k, v) -> k, map_json f v) l)
	      | `List l -> `List (list_map (map_json f) l)
	      | `Bool _ | `Float _ | `Int _ | `Null | `String _ as x -> f x
	    in

	    (* Replace all the places that mention a port number with the actual test server address. *)
	    let resolve_port =
	      map_json begin
		  function
		  | `Port x -> `Int (M.find x servers)
		  | `Bool _ | `Float _ | `Int _ | `Null | `String _ as x -> x
		end
	    in
	    let test_cases =
	      list_map begin fun test_case ->
			     { test_case with query = resolve_port test_case.query }
		       end
		       test_cases
	    in

	    let call connection test =
	      Lwt.catch
		begin fun () ->
		      let call_aux =
			match !settings with
			| None -> JsonProtocol.Sync.call connection
			| Some settings -> JsonProtocol.Sync.call ~settings connection
		      in call_aux test.query
		end
		begin fun exn ->
		      let open Y.Util in
		      ExnTranslator.to_json exn |> (member "exn_value") |> Lwt.return
		end
	    in

	    (* Run the test cases. *)
	    let servers = M.bindings servers in
	    Lwt_io.flush_all ()
	    >>= (fun () -> List.iter (fun (name, port) -> run_test_server name port) servers;
			   Lwt_unix.sleep 0.3)
	    >>= (fun () -> open_connections servers)
	    >>= (fun connections_list ->
		 Lwt.finalize
		   begin fun () ->
			 (* Map from test server name to connection. *)
			 let connections =
			   List.fold_left
			     (fun connections (name, connection) -> M.add name connection connections)
			     M.empty
			     connections_list
			 in
			 Lwt_list.iter_s
			   (* For each test case. *)
			   begin fun t ->
				 Lwt_io.printlf "\ttest case: %s" (Y.to_string t.query)
				 >>= begin fun () ->
					   (* Send the test query to the test server. *)
					   let connection = M.find t.connection connections in
					   call connection t
				     end
				 >>= begin fun response ->
					   if response = t.response
					   then Lwt.return_unit
					   else (incr errors;
						 Lwt_io.eprintlf
						   "FAIL    expected %s, but got %s"
						   (Y.to_string t.response) (Y.to_string response))
				     end
			   end
			   test_cases
		   end
		   begin fun () ->
			 Lwt_list.iter_p
			   begin fun (name, connection) ->
				 Lwt_io.printlf "Killing %s" name
				 >>= fun () -> JsonProtocol.Sync.write_request
						 connection.output
						 JsonOp.kill
			   end
			   connections_list
		   end)

	  module TestJsonOp =
	    struct
	      (** Creates a Json message to add a peer.
                  The peer port is [`Port of string] value that will be replaced with a
                  [`Int of int] value by the test case framework. *)
	      let add_peer ip peer_name =
		`Assoc [ "operation", `String "add_peer" ;
			 "ip", `String ip ;
			 "port", (`Port peer_name |> Obj.magic) ]

	      let () =
		JsonProtocol.register_operation
		  "peer_name"
		  begin fun message connection ->
			let open Y.Util in
			let peer_name = message |> (member "peer_name") |> to_string in
			let peer = Hashtbl.find connection.root.connections peer_name in
			Lwt.return (`Assoc [ "result", `String peer.peer_name ])
		  end
	      let peer_name peer_name =
		`Assoc [ "operation", `String "peer_name" ;
			 "peer_name", `String peer_name ]

	      let () =
		JsonProtocol.register_operation
		  "my_name"
		  begin fun message connection ->
			let open Y.Util in
			let peer_name = message |> (member "peer_name") |> to_string in
			let root_to_peer = Hashtbl.find connection.root.connections peer_name in
			NativeProtocol.Async.call root_to_peer
						  (fun peer_to_root -> Lwt.return peer_to_root.peer_name)
			>|= fun root_name -> `Assoc [ "result", `String root_name ]
		  end
	      let my_name peer_name =
		`Assoc [ "operation", `String "my_name" ;
			 "peer_name", `String peer_name ]

	      let () =
		JsonProtocol.register_operation
		  "sleep"
		  begin fun message connection ->
			let open Y.Util in
			let timeout = message |> (member "timeout") |> to_float in
			Lwt_unix.sleep timeout
			>|= fun root_name -> `Assoc [ "result", `Null ]
		  end
	      let sleep timeout =
		`Assoc [ "operation", `String "sleep" ;
			 "timeout", `Float timeout ]

	      let () =
		JsonProtocol.register_operation
		  "forward_native_timeout"
		  begin fun message connection ->
			let open Y.Util in
			let peer_name = message |> (member "peer_name") |> to_string
			and timeout = message |> (member "timeout") |> to_float
			and forward = message |> (member "forward") in
			NativeProtocol.Async.call
			  ~settings: { Protocol.timeout = timeout }
			  (Hashtbl.find connection.root.connections peer_name)
			  (JsonProtocol.Sync.get_operation forward)
		  end
	      let forward_native_timeout peer_name timeout op =
		`Assoc [ "operation", `String "forward_native_timeout" ;
			 "peer_name", `String peer_name ;
			 "timeout", `Float timeout ;
			 "forward", op ]
	    end

	  module Builders =
	    struct
	      let times connection a b =
		{ connection ;
		  query = JsonOp.times a b ;
		  response = `Assoc [ "result", `Int (a * b) ] }

	      let divide connection a b =
		{ connection ;
		  query = JsonOp.divide a b ;
		  response = `Assoc [ "result", `Int (a / b) ] }

	      let root_name connection =
		{ connection ;
		  query = JsonOp.root_name ;
		  response = `Assoc [ "result", `String connection ] }

	      (** Creates a test_case where the peer's port is replaced with a numerical
                  value by the test case framework. *)
	      let add_peer ?(expected = "Both") connection peer =
		{ connection ;
		  query = TestJsonOp.add_peer test_ip peer ;
		  response = `Assoc [ "result", `String expected ] }

	      let peers connection peers =
		{ connection ;
		  query = JsonOp.peers ;
		  response = `Assoc [ "result", `List (List.map (fun peer -> `String peer) peers) ] }

	      let remove_peer ?(expected = true) connection removed_peer =
		{ connection ;
		  query = JsonOp.remove_peer removed_peer ;
		  response = `Assoc [ "result", `Bool expected ] }

	      let kill_peer connection peer =
		{ connection ;
		  query = JsonOp.kill_peer peer ;
		  response = `Assoc [ "result", `Bool true ] }

	      let peer_name connection peer =
		{ connection ;
		  query = TestJsonOp.peer_name peer ;
		  response = `Assoc [ "result", `String peer ] }

	      let my_name connection peer =
		{ connection ;
		  query = TestJsonOp.my_name peer ;
		  response = `Assoc [ "result", `String connection ] }

	      let forward connection test =
		{ connection ;
		  query = JsonOp.forward test.connection test.query ;
		  response = test.response }

	      let forward_native connection test =
		{ connection ;
		  query = JsonOp.forward_native test.connection test.query ;
		  response = test.response }

	      let sleep connection duration =
		{ connection ;
		  query = TestJsonOp.sleep duration ;
		  response = `Assoc [ "result", `Null ] }

	      let timeout connection duration =
		{ connection ;
		  query = TestJsonOp.sleep duration ;
		  response = `String "Timeout error" }

	      let forward_native_timeout connection timeout test =
		{ connection ;
		  query = TestJsonOp.forward_native_timeout
			    test.connection
			    timeout
			    test.query ;
		  response = test.response }

	      let op_not_found connection op =
		{ connection ;
		  query = `Assoc [ "operation", `String op ] ;
		  response = `Assoc [ "kind", `String "System" ;
				      "value", `String ("Failure(\"Operation not found: <" ^ op ^ ">\")") ]
		}
	    end

	  let basic_test_cases =
	    [
	      (* Arithmetic operations *)
	      Builders.divide "A" 7 2 ;
	      Builders.divide "B" 23 7 ;

	      (* root_name *)
	      Builders.root_name "A" ;
	      Builders.root_name "B" ;

	      (* add_peer *)
	      Builders.add_peer "A" "B" ;

	      Builders.add_peer ~expected:"None" "B" "A" ;
	      Builders.add_peer ~expected:"None" "A" "B" ;

	      { connection = "A" ;
		query = JsonOp.add_peer test_ip 9999 ;
		response = `Assoc [
			      "error", `Int 63 ;
			      "error_message", `String "Connection refused" ;
			      "name", `String "connect" ;
			      "arg", `String"" ] } ;

	      (* remove_peer *)
	      Builders.peers "A" ["B"] ;
	      Builders.peers "B" ["A"] ;
	      Builders.remove_peer "A" "B" ;
	      Builders.peers "A" [] ;
	      Builders.peers "B" [] ;
	      Builders.remove_peer ~expected:false "A" "B" ;
	      Builders.remove_peer ~expected:false "B" "A" ;
	      Builders.add_peer "A" "B" ;

	      (* Async connections *)
	      (Builders.times "A" 5 7)
	      |> (Builders.forward "B")
	      |> (Builders.forward_native "A")
	      |> (Builders.forward "B")
	      |> (Builders.forward "A") ;

	      (Builders.times "B" 6 7)
	      |> (Builders.forward "A")
	      |> (Builders.forward_native "B")
	      |> (Builders.forward "A")
	      |> (Builders.forward "B") ;

	      (* Exceptions *)
	      Builders.op_not_found "A" "invalid op!" ;

	      (Builders.op_not_found "B" "invalid op!")
	      |> (Builders.forward "A") ;

	      (Builders.op_not_found "B" "invalid op!")
	      |> (Builders.forward_native "A")
	      |> (Builders.forward_native "B")
	      |> (Builders.forward "A") ;

	      (* Use peers *)
	      Builders.peers "A" ["B"] ;
	      Builders.peers "B" ["A"] ;
	      Builders.peer_name "A" "B" ;
	      Builders.peer_name "B" "A" ;
	      Builders.my_name "A" "B" ;
	      Builders.my_name "B" "A" ;
	    ]

	  let run_timeout_test_case () =
	    let old_settings = !settings in
	    settings := Some { Protocol.timeout = 0.2 };
	    let safe_server = "SafeServer"
	    and safe_server2 = "SafeServer2"
	    and test_server = "TestServer"
	    and timeout_server = "TimeoutServer"
	    and timeout_server2 = "TimeoutServer2" in
	    (run_tests [
		 Builders.add_peer test_server timeout_server ;
		 Builders.add_peer timeout_server2 test_server ;
		 Builders.add_peer safe_server timeout_server ;
		 Builders.add_peer safe_server2 timeout_server2 ;

		 Builders.sleep timeout_server 0.1 ;
		 (Builders.sleep timeout_server 0.09)
		 |> (Builders.forward_native_timeout test_server 0.1) ;
		 (Builders.sleep timeout_server2 0.09)
		 |> (Builders.forward_native_timeout test_server 0.1) ;

		 (* This query will crash and close the connection to "TimeoutServer". *)
		 (Builders.timeout timeout_server 0.3) ;
		 (Builders.timeout timeout_server 0.11)
		 |> (Builders.forward_native_timeout test_server 0.1) ;
		 (Builders.timeout timeout_server2 0.11)
		 |> (Builders.forward_native_timeout test_server 0.1) ;

		 Builders.kill_peer safe_server timeout_server ;
		 Builders.kill_peer safe_server2 timeout_server2 ;
	       ])
	    >|= (fun () -> settings := old_settings)
	    >>= (fun () -> Lwt_unix.sleep 0.3)
	    >>= (fun () -> run_tests [
			       Builders.times safe_server 1 7 ;
			       Builders.times test_server 2 7 ;
			       Builders.times timeout_server 3 7 ;
			       Builders.times timeout_server2 4 7 ;
			     ])

	  let run () =
	    Lwt_io.printl "Runnning tests..."

	    >|= (fun () -> settings := None)

	    >>= (fun () -> run_tests basic_test_cases)
	    >>= run_timeout_test_case
	    >>= (fun () -> run_tests basic_test_cases)

	    >>= (fun () -> Lwt_io.printlf "\n%s\nErrors: %d" (String.make 80 '=') !errors)
	    >|= fun () -> true
	end
      in
      Lwt_main.run (X.run ())
    end

let () = if not !has_run then failwith "Tests must be compiled without -noassert"
