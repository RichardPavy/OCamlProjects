open Lwt.Infix
open SyncTypes

module AsyncUtils = Sync_Utils_AsyncUtils

type json = Yojson.Basic.json

let add_peer =
  let add_connection peer_name connection =
    let root = connection.root in
    if Hashtbl.mem root.connections peer_name
    then false (* connection is already registered *)
    else begin
	connection.peer_name <- peer_name;
	Hashtbl.add root.connections peer_name connection;
	AsyncUtils.async
	  "connection_closed"
	  (fun () -> connection.onclose
		     >|= fun () -> Hashtbl.remove root.connections peer_name);
	true
      end
  in
  fun ~ip ~port root ->
  let root_name = root.root_name in
  Rpc.open_connection ~ip ~port root
  >>= begin fun root_to_peer ->
      NativeProtocol.Sync.call
        root_to_peer
        (fun peer_to_root ->
          let peer = peer_to_root.root in
          let peer_name = peer.root_name in
          Lwt.return (peer_name, add_connection root_name peer_to_root))
      >|= begin fun (peer_name, peer_to_root_added) ->
          let root_to_peer_added = add_connection peer_name root_to_peer in
          if root_to_peer_added then Server.serve root_to_peer;
          match root_to_peer_added, peer_to_root_added with
          | true, true -> `Both
          | true, false -> `SelfOnly
          | false, true -> `PeerOnly
          | false, false -> `None
          end
      end

module Json = struct
  let () =
    let register_arithmetic_operation name f =
      JsonProtocol.register_operation
	name
	begin fun message connection ->
	let open Yojson.Basic.Util in
	let a = message |> (member "a") |> to_int
	and b = message |> (member "b") |> to_int
	in Lwt.return (`Assoc [ "result", `Int (f a b) ])
	end
    in
    register_arithmetic_operation "plus" ( + );
    register_arithmetic_operation "minus" ( - );
    register_arithmetic_operation "times" ( * );
    register_arithmetic_operation "divide" ( / );
    register_arithmetic_operation "modulo" ( mod )

  let plus, minus, times, divide, modulo =
    let op name (a:int) (b:int) =
      `Assoc [ "operation", `String name;
	       "a", `Int a;
	       "b", `Int b ]
    in (op "plus"),
       (op "minus"),
       (op "times"),
       (op "divide"),
       (op "modulo")

  let () =
    JsonProtocol.register_operation
      "kill"
      begin fun message connection ->
      connection.root.server.shutdown ();
      connection.root.server.onshutdown
      >>= fun () -> AsyncUtils.fail "Server down."
      end
  let kill = `Assoc [ "operation", `String "kill" ]

  let () =
    JsonProtocol.register_operation
      "kill_peer"
      begin fun message connection ->
      let open Yojson.Basic.Util in
      let peer_name = message |> (member "peer_name") |> to_string
      and root = connection.root in
      begin
	if Hashtbl.mem root.connections peer_name
	then let peer = Hashtbl.find root.connections peer_name in
	     Lwt_io.atomic
	       (fun output -> JsonProtocol.Sync.write_request
				output
				kill
			      >|= fun () -> true)
	       peer.output
	else Lwt.return_false
      end >|= fun result -> `Assoc [ "result", `Bool result ]
      end
  let kill_peer peer_name =
    `Assoc [ "operation", `String "kill_peer";
	     "peer_name", `String peer_name ]

  let () =
    JsonProtocol.register_operation
      "add_peer"
      begin fun message connection ->
      let open Yojson.Basic.Util in
      let ip = message |> (member "ip") |> to_string
      and port = message |> (member "port") |> to_int
      in
      add_peer ~ip ~port connection.root
      >|= fun result ->
      let str = match result with
	| `Both -> "Both"
	| `SelfOnly -> "SelfOnly"
	| `PeerOnly -> "PeerOnly"
	| `None -> "None"
      in `Assoc [ "result", `String str ]
      end
  let add_peer ip port =
    `Assoc [ "operation", `String "add_peer";
	     "ip", `String ip;
	     "port", `Int port ]

  let () =
    JsonProtocol.register_operation
      "remove_peer"
      begin fun message connection ->
      let open Yojson.Basic.Util in
      let peer_name = message |> (member "peer_name") |> to_string
      and root = connection.root in
      begin
	if Hashtbl.mem root.connections peer_name
	then let peer = Hashtbl.find root.connections peer_name in
	     peer.close() >|= fun () -> true
	else Lwt.return_false
      end >|= fun result -> `Assoc [ "result", `Bool result ]
      end
  let remove_peer peer_name =
    `Assoc [ "operation", `String "remove_peer";
	     "peer_name", `String peer_name ]

  let () =
    JsonProtocol.register_operation
      "root_name"
      begin fun message connection ->
      let root = connection.root in
      Lwt.return (`Assoc [ "result", `String root.root_name ])
      end
  let root_name = `Assoc [ "operation", `String "root_name" ]

  let () =
    JsonProtocol.register_operation
      "peers"
      begin fun message connection ->
      let root = connection.root in
      let connections = Hashtbl.fold
			  (fun _ peer l -> `String peer.peer_name :: l)
			  root.connections []
      in
      Lwt.return (`Assoc [ "result", `List connections ])
      end
  let peers = `Assoc [ "operation", `String "peers" ]

  let () =
    JsonProtocol.register_operation
      "forward"
      begin fun message connection ->
      let open Yojson.Basic.Util in
      let peer_name = message |> (member "peer_name") |> to_string
      and forward = message |> (member "forward") in
      JsonProtocol.Async.call
	(Hashtbl.find connection.root.connections peer_name)
	forward
      end
  let forward peer_name op =
    `Assoc [ "operation", `String "forward";
	     "peer_name", `String peer_name;
	     "forward", op ]

  let () =
    JsonProtocol.register_operation
      "forward_native"
      begin fun message connection ->
      let open Yojson.Basic.Util in
      let peer_name = message |> (member "peer_name") |> to_string
      and forward = message |> (member "forward") in
      NativeProtocol.Async.call
	(Hashtbl.find connection.root.connections peer_name)
	(JsonProtocol.Sync.get_operation forward)
      end
  let forward_native peer_name op =
    `Assoc [ "operation", `String "forward_native";
	     "peer_name", `String peer_name;
	     "forward", op ]
end
