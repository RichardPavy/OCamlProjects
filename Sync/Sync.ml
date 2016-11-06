(** Synchronize folders. *)
open Lwt.Infix

type shared_dir = {
    peer : connection ;
    name : string ;
    local_path : string list ;
    mutable last_sync : float ;
  }

 and kind = File | Dir | Null
 and stat = { path : string list ;
	      kind : kind ;
	      mtime : float ;
	      perm : int }

let shared_dirs = Hashtbl.create 16
let shared_dirs_key = Storage.register shared_dirs

let remote_shared_dirs peer =
  let (module Storage : StorageT) = peer.root.storage in
  Storage.get shared_dirs_key

let stat path =
  Lwt.catch
    begin fun () ->
	  Lwt_unix.lstat (join path)
	  >|= fun stats ->
	      let open Unix in
	      let kind = match stats.st_kind with
		| S_REG (* Regular file *) -> File
		| S_DIR (* Directory *) -> Dir
		| S_CHR (* Character device *)
		| S_BLK (* Block device *)
		| S_LNK (* Symbolic link *)
		| S_FIFO (* Named pipe *)
		| S_SOCK (* Socket *)
		  -> Null
	      in { path ;
		   kind ;
		   mtime = stats.st_mtime ;
		   perm = stats.st_perm }
    end
    begin function
      | Unix.Unix_error (Unix.ENOENT, _, _) ->
	 Lwt.return { path ;
		      kind = Null ;
		      mtime = max_float ;
		      perm = 0 }
      | exn -> Lwt.fail exn
    end

let ls path =
  let path_str = join path in
  Lwt.catch
    begin fun () ->
	  let files = Lwt_unix.files_of_directory path_str in
	  let stats = Lwt_stream.filter_map_s
			begin function
			  | "." | ".." -> Lwt.return_none
			  | filename -> stat (filename :: path) >|= fun stat -> Some stat
			end
			files
	  in Lwt_stream.to_list stats
    end
    (fun _ -> Lwt.return_nil)

let rec add_shared_dir ~peer ~name
		       ~local_path
		       ~peer_path =
  assert (local_path <> []);
  let shared_dirs = remote_shared_dirs peer in
  let key = peer.peer_name, name in
  if Hashtbl.mem shared_dirs key
  then Lwt.return_unit
  else
    let call = NativeProtocol.Async.call peer in
    Hashtbl.add shared_dirs key
		{ peer ; name ; local_path ; last_sync = 0. };
    Utils.async "remove shared dir on close"
		begin fun () ->
		      peer.onclose
		      >|= fun () -> Hashtbl.remove shared_dirs key
		end;
    call (fun peer ->
	  add_shared_dir ~peer ~name
			 ~local_path: peer_path
			 ~peer_path: [])

and list_shared_dirs peer =
  Hashtbl.fold
    begin fun key shared_dir accu ->
	  if shared_dir.peer.peer_name = peer.peer_name
	  then (shared_dir.name,
		shared_dir.local_path,
		shared_dir.last_sync) :: accu
	  else accu
    end
    shared_dirs []

let delete_local job stat =
  let rec aux stat =
    let ignore_failure f = Lwt.catch f (fun _ -> Lwt.return_unit) in
    let path = join stat.path in
    match stat.kind with
    | File | Null -> ignore_failure (fun () -> Lwt_unix.unlink path)
    | Dir -> ls stat.path
	     >>= Lwt_list.iter_s aux
	     >>= fun () -> ignore_failure (fun () -> Lwt_unix.rmdir path)
  in job (fun () -> aux stat)

let copy_file job shared_dir relative_path =
  ()

let update_file job shared_dir relative_path local_stat =
  let aux () =
  in job aux

let rec sync_dir ~job ~peer ~shared_dir ~relative_path =
  let aux () =
    let call = NativeProtocol.Async.call peer in
    let last_sync = shared_dir.last_sync
    and shared_dir_name = shared_dir.name
    and local_path = relative_path @ shared_dir.local_path in
    let local_dir = ls local_path
    and remote_dir =
      call begin
	  fun peer ->
	  let shared_dirs = remote_shared_dirs peer in
	  let shared_dir = Hashtbl.find shared_dirs (peer.peer_name, shared_dir_name) in
	  let local_path = relative_path @ shared_dir.local_path in
	  ls local_path
	end
    in
    let module NameSet = Set.Make(struct type t = string let compare = compare end) in
    let names = ref NameSet.empty in
    let hash_dir dir =
      let h = Hashtbl.create (List.length dir) in
      List.iter begin fun stat ->
		      let name = List.hd stat.path in
		      names := NameSet.add name !names;
		      Hashtbl.add h name stat
		end
		dir;
      Lwt.return h
    in
    local_dir >>= hash_dir >>= fun local_dir ->
    remote_dir >>= hash_dir >>= fun remote_dir ->
    let names = !names in
    NameSet.iter
      begin fun name ->
	    let local_stat = try Some (Hashtbl.find local_dir name)
			     with Not_found -> None
	    and remote_stat = try Some (Hashtbl.find remote_dir name)
			      with Not_found -> None
	    in
	    match local_stat, remote_stat with
	    | Some local_stat, Some remote_stat ->
	       (* if local is Dir and remote is Dir, sync sub-dirs *)
	       if local_stat.kind = Dir && remote_stat.kind = Dir
	       then sync_dir ~job ~peer ~shared_dir ~relative_path: (name :: relative_path)
	       else
	      	 if remote_stat.mtime > last_sync
		    && remote_stat.mtime > local_stat.mtime
		 then (* remote changed since last sync and is newer than local.
		       * add to job to copy from remote to local. *)
		   update_file job shared_dir (name :: relative_path) local_stat
	    | None, Some remote_stat -> 
	       if remote_stat.mtime > last_sync
	       then (* remote is new. *)
		 copy_file job shared_dir (name :: relative_path)
	       else (* delete remote. *)
		 ()
	    | Some local_stat, None ->
	       if local_stat.mtime > last_sync
	       then (* local is new. *)
		 ()
	       else (* delete local. *)
		 delete_local job local_stat
	    | None, None -> failwith "Impossible"
      end
      names;
    Lwt.return_unit
  in job aux


module Test =
  struct
    module Y = Yojson.Basic
    module M = Map.Make(struct
			   type t = string
			   let compare = Pervasives.compare
			 end)
    module JsonOp = Operations.Json

    type 'a test_case = {
	connection : string;
	query : 'a;
	response : 'a;
      }

    let test_ip = "127.0.0.1"
    let settings = ref None
    let errors = ref 0

    exception TestFailure of Y.json * Y.json
    let expected_exception = `String "exception"

    let run_test_server name port =
      if Lwt_unix.fork () = 0
      then Lwt_main.run
	     begin
	       let timeout = 10. in
	       Lwt.async (fun () -> Lwt_unix.sleep timeout
				    >>= fun () -> Lwt_io.eprintf "Test timeout exceeded: %0.2fs" timeout
				    >>= exit 0);
	       Lwt_io.printlf "Starting server %s" name
	       >>= (fun () -> let { server } = Server.start ~name ~ip:test_ip ~port () in
			      server.onshutdown)
	       >>= (fun () -> Lwt_io.printlf "Server %s is down" name)
	       >>= (fun () -> exit 0)
	     end

    let open_connections ips_ports =
      let root = Server.create_root ~ip:"fake" ~port:(-1) in
      Lwt_list.map_s
	begin fun (name, port) ->
	      Lwt_io.printlf "Open connection %s on %s:%i" name test_ip port
	      >>= (fun () -> RPC.open_connection ~ip:test_ip ~port root)
	      >|= fun connection -> name, connection
	end
	ips_ports

    let run_tests test_cases =
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
	| `Assoc l -> `Assoc (list_map (fun (k, v) -> k, f v) l)
	| `List l -> `List (list_map f l)
	| `Bool _ | `Float _ | `Int _ | `Null | `String _ as x -> f x
      in
      let resolve_port =
	map_json begin
	    function
	    | `Port x -> `Int (M.find x servers)
	    | `Assoc _ | `Bool _ | `Float _ | `Int _ | `List _ | `Null | `String _ as x -> x
	  end
      in
      let test_cases =
	list_map (fun test_case ->
		  { test_case with query = resolve_port test_case.query;
				   response = resolve_port test_case.response })
		 test_cases
      in
      let servers = M.bindings servers in
      Lwt_io.flush_all ()
      >>= (fun () -> List.iter (fun (name, port) -> run_test_server name port) servers;
		     Lwt_unix.sleep 0.3)
      >>= (fun () -> open_connections servers)
      >>= begin fun connections_list ->
		Lwt.finalize
		  begin fun () ->
			let connections =
			  List.fold_left
			    (fun connections (name, connection) -> M.add name connection connections)
			    M.empty
			    connections_list
			in
			Lwt_list.iter_s
			  begin fun t ->
				(Lwt_io.printlf "        test case: %s" (Y.to_string t.query))
				>>= begin fun () ->
					  let connection = M.find t.connection connections in
					  Lwt.catch
					    (fun () -> match !settings with
						       | None -> JsonProtocol.Sync.call connection t.query
						       | Some settings -> JsonProtocol.Sync.call ~settings connection t.query)
					    (fun exn -> Lwt.return expected_exception)
				    end
				>>= fun response ->
				if response = t.response
				then Lwt.return_unit
				else begin
				    incr errors;
				    Lwt_io.printlf "FAIL    expected %s, but got %s"
						   (Y.to_string t.response) (Y.to_string response)
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
		  end
	  end

    module TestJsonOp =
      struct
	let add_peer ip peer_name =
	  `Assoc [ "operation", `String "add_peer";
		   "ip", `String ip;
		   "port", `Port peer_name ]

	let () =
	  JsonProtocol.register_operation
	    "peer_name"
	    begin fun message connection ->
		  let open Yojson.Basic.Util in
		  let peer_name = message |> (member "peer_name") |> to_string in
		  let peer = Hashtbl.find connection.root.connections peer_name in
		  Lwt.return (`Assoc [ "result", `String peer.peer_name ])
	    end
	let peer_name peer_name =
	  `Assoc [ "operation", `String "peer_name";
		   "peer_name", `String peer_name ]

	let () =
	  JsonProtocol.register_operation
	    "my_name"
	    begin fun message connection ->
		  let open Yojson.Basic.Util in
		  let peer_name = message |> (member "peer_name") |> to_string in
		  let root_to_peer = Hashtbl.find connection.root.connections peer_name in
		  NativeProtocol.Async.call root_to_peer
					    (fun peer_to_root -> Lwt.return peer_to_root.peer_name)
		  >|= fun root_name -> `Assoc [ "result", `String root_name ]
	    end
	let my_name peer_name =
	  `Assoc [ "operation", `String "my_name";
		   "peer_name", `String peer_name ]

	let () =
	  JsonProtocol.register_operation
	    "sleep"
	    begin fun message connection ->
		  let open Yojson.Basic.Util in
		  let timeout = message |> (member "timeout") |> to_float in
		  Lwt_unix.sleep timeout
		  >|= fun root_name -> `Assoc [ "result", `Null ]
	    end
	let sleep timeout =
	  `Assoc [ "operation", `String "sleep";
		   "timeout", `Float timeout ]

	let () =
	  JsonProtocol.register_operation
	    "forward_native_timeout"
	    begin fun message connection ->
		  let open Yojson.Basic.Util in
		  let peer_name = message |> (member "peer_name") |> to_string
		  and timeout = message |> (member "timeout") |> to_float
		  and forward = message |> (member "forward") in
		  NativeProtocol.Async.call
		    ~settings: { Protocol.timeout = timeout }
		    (Hashtbl.find connection.root.connections peer_name)
		    (JsonProtocol.Sync.get_operation forward)
	    end
	let forward_native_timeout peer_name timeout op =
	  `Assoc [ "operation", `String "forward_native_timeout";
		   "peer_name", `String peer_name;
		   "timeout", `Float timeout;
		   "forward", op ]
      end

    let basic_test_cases =
      [
	(* Arithmetic operations *)
	{ connection = "A";
	  query = JsonOp.divide 7 2;
	  response = `Assoc [ "result", `Int 3 ] };

	{ connection = "B";
	  query = JsonOp.divide 23 7;
	  response = `Assoc [ "result", `Int (-1) ] };

	(* root_name *)
	{ connection = "A";
	  query = JsonOp.root_name;
	  response = `Assoc [ "result", `String "A" ] };

	{ connection = "B";
	  query = JsonOp.root_name;
	  response = `Assoc [ "result", `String "B" ] };

	(* add_peer *)
	{ connection = "A";
	  query = TestJsonOp.add_peer test_ip "B";
	  response = `Assoc [ "result", `String "Both" ] };

	{ connection = "A";
	  query = TestJsonOp.add_peer test_ip "B";
	  response = `Assoc [ "result", `String "None" ] };

	{ connection = "B";
	  query = TestJsonOp.add_peer test_ip "A";
	  response = `Assoc [ "result", `String "None" ] };

	{ connection = "A";
	  query = JsonOp.add_peer test_ip 9999;
	  response = expected_exception };

	(* remove_peer *)
	{ connection = "A";
	  query = JsonOp.peers;
	  response = `Assoc [ "result", `List [ `String "B" ] ] };

	{ connection = "A";
	  query = JsonOp.remove_peer "B";
	  response = `Assoc [ "result", `Bool true ] };

	{ connection = "A";
	  query = JsonOp.peers;
	  response = `Assoc [ "result", `List [] ] };

	{ connection = "B";
	  query = JsonOp.peers;
	  response = `Assoc [ "result", `List [] ] };


	{ connection = "A";
	  query = JsonOp.remove_peer "B";
	  response = `Assoc [ "result", `Bool false ] };

	{ connection = "B";
	  query = JsonOp.remove_peer "A";
	  response = `Assoc [ "result", `Bool false ] };

	{ connection = "A";
	  query = TestJsonOp.add_peer test_ip "B";
	  response = `Assoc [ "result", `String "Both" ] };

	(* Async connections *)
	{ connection = "A";
	  query = (let fwd = JsonOp.forward
		   and fwd_native = JsonOp.forward_native in
		   fwd "B"
		       (fwd "A"
			    (fwd_native "B"
					(fwd "A"
					     (JsonOp.times 3 7)))));
	  response = `Assoc [ "result", `Int 21 ] };

	(* Exceptions *)
	{ connection = "A";
	  query = `Assoc [ "operation", `String "operation does not exist" ];
	  response = expected_exception };
	{ connection = "A";
	  query = JsonOp.forward
		    "B"
		    (`Assoc [ "operation", `String "operation does not exist" ]);
	  response = expected_exception };
	{ connection = "A";
	  query = JsonOp.forward
		    "B"
		    (JsonOp.forward_native
		       "A"
		       (JsonOp.forward_native
			  "B"
			  (`Assoc [ "operation", `String "operation does not exist" ])));
	  response = expected_exception };

	(* use peers *)
	{ connection = "A";
	  query = JsonOp.peers;
	  response = `Assoc [ "result", `List [`String "B"] ] };

	{ connection = "B";
	  query = JsonOp.peers;
	  response = `Assoc [ "result", `List [`String "A"] ] };

	{ connection = "A";
	  query = TestJsonOp.peer_name "B";
	  response = `Assoc [ "result", `String "B" ] };

	{ connection = "B";
	  query = TestJsonOp.peer_name "A";
	  response = `Assoc [ "result", `String "A" ] };

	{ connection = "B";
	  query = TestJsonOp.my_name "A";
	  response = `Assoc [ "result", `String "B" ] };

	{ connection = "A";
	  query = TestJsonOp.my_name "B";
	  response = `Assoc [ "result", `String "A" ] };
      ]

    let run_timeout_test_case () =
      settings := Some { Protocol.timeout = 0.2 };
      (run_tests [
	   { connection = "TestServer";
	     query = TestJsonOp.add_peer test_ip "TimeoutServer";
	     response = `Assoc [ "result", `String "Both" ] };
	   { connection = "TimeoutServer2";
	     query = TestJsonOp.add_peer test_ip "TestServer";
	     response = `Assoc [ "result", `String "Both" ] };

	   { connection = "TimeoutServer";
	     query = TestJsonOp.sleep 0.1;
	     response = `Assoc [ "result", `Null ] };
	   { connection = "TestServer";
	     query = TestJsonOp.forward_native_timeout
		       "TimeoutServer" 0.1
		       (TestJsonOp.sleep 0.09);
	     response = `Assoc [ "result", `Null ] };
	   { connection = "TestServer";
	     query = TestJsonOp.forward_native_timeout
		       "TimeoutServer2" 0.1
		       (TestJsonOp.sleep 0.09);
	     response = `Assoc [ "result", `Null ] };

	   { connection = "TimeoutServer";
	     query = TestJsonOp.sleep 0.3;
	     response = expected_exception };
	   { connection = "TestServer";
	     query = TestJsonOp.forward_native_timeout
		       "TimeoutServer" 0.1
		       (TestJsonOp.sleep 0.11);
	     response = expected_exception };
	   { connection = "TestServer";
	     query = TestJsonOp.forward_native_timeout
		       "TimeoutServer2" 0.1
		       (TestJsonOp.sleep 0.11);
	     response = expected_exception };
	 ])
      >|= (fun () -> settings := None)
      >>= (fun () -> Lwt_unix.sleep 0.3)
      >>= (fun () -> run_tests [
		       	 { connection = "TimeoutServer";
			   query = JsonOp.times 2 7;
			   response = `Assoc [ "result", `Int 14 ] };
		       	 { connection = "TimeoutServer";
			   query = JsonOp.times 3 7;
			   response = `Assoc [ "result", `Int 21 ] };
		       	 { connection = "TestServer";
			   query = JsonOp.times 4 7;
			   response = `Assoc [ "result", `Int 28 ] };
		       ])

    exception TestQuotaFailure
    let run_quota_tests () =
      let open Quota in
      let q = create 1000
      and count = ref 0
      and active = ref 0
      and max_active = ref (-1) in
      let wait () =
	incr count;
	if !count mod 3 = 0
	then Lwt.fail TestQuotaFailure
	else begin
	    incr active;
	    max_active := max !active !max_active;
	    Lwt_unix.sleep 0.1 >|= fun () -> decr active
	  end
      in
      let rec wait_p n =
	if n == 0
	then Lwt.return_unit
	else
	  let wait () = with_quota q 50 wait
	  and next = wait_p (n - 1) in
	  Lwt.finalize wait (fun () -> next)
      in Lwt.catch
	   (fun () -> wait_p 75)
	   (function TestQuotaFailure -> Lwt.return_unit | exn -> Lwt.fail exn)
	 >>= fun () -> if !count <> 75 || !active <> 0 || !max_active <> 20
		       then
			 Lwt_io.eprintlf "count=%d, active=%d, max_active=%d"
					 !count !active !max_active
			 >>= fun () -> Lwt.fail_with "Unexpected results in run_quota_tests"
		       else Lwt.return_unit

    let run () =
      Lwt_io.printl "Runnning tests..."

      >>= run_quota_tests

      >|= (fun () -> settings := None)

      >>= (fun () -> run_tests basic_test_cases)
      >>= run_timeout_test_case

      >>= (fun () -> run_tests basic_test_cases)

      >>= (fun () -> Lwt_io.printlf "\n%s\nErrors: %d" (String.make 80 '=') !errors)
      >>= (fun () -> exit 0)
  end

let () = Utils.async "tests" Test.run

let () =
  let waiter, _ = Lwt.wait ()
  in Lwt_main.run waiter

(* let server = Server.start () *)
