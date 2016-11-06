(** Module that stores key-value pairs.
    Storage is implemented by an array, and keys are just integers. *)
module Storage : sig
    (** ['a keys] store values of type ['a] *)
    type 'a key

    (** Stores a value and returns its allocated key *)
    val register : 'a -> 'a key

    (** Returns the value stored under the given key *)
    val get : 'a key -> 'a
  end

(** Type of modules that stores key-value pairs *)
module type StorageT =
  sig
    type 'a key = 'a Storage.key
    val register : 'a -> 'a key
    val get : 'a key -> 'a
  end

(** Data structure storing information about the local machine. *)
type root = {
    root_name : string; (** Name of the local machine. *)
    server : server;    (** Server instance running on the local machine. *)
    connections : (string, connection) Hashtbl.t; (** Connections established to other machines. *)
    storage : (module StorageT); (** Local storage. *)
  }
 and server = {
     ip : string;
     port : int;
     shutdown : unit -> unit;
     onshutdown : unit Lwt.t;
   }
 and connection = {
     root : root; (** The local machine. *)
     mutable peer_name : string; (** Name of the peer. For any connection:
                                     root.connections[peer_name] == connection *)
     sockaddr : Lwt_unix.sockaddr;
     lock : Lwt_mutex.t; (** Not used, since we use Lwt_io.atomic instead. *)
     input : Lwt_io.input_channel;
     output : Lwt_io.output_channel;
     close : unit -> unit Lwt.t; (* Closes the connection. *)
     onclose : unit Lwt.t; (* Thead that wakes up when the connection is closed. *)
   }
