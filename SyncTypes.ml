module Storage :
sig
  type 'a key
  val register : 'a -> 'a key
  val get : 'a key -> 'a
end =
  struct
    type 'a key = int
    let data = ref ([||])
    let register value =
      let old_data = !data in
      let length = Array.length old_data in
      data := Array.init
		(length + 1)
		begin fun i ->
		      if i == length
		      then Obj.magic value
		      else old_data.(i)
		end;
      length
    let get key = Obj.magic !data.(key)
  end

module type StorageT = (module type of Storage with type 'a key = 'a Storage.key)

type root = {
    root_name : string;
    server : server;
    connections : (string, connection) Hashtbl.t;
    storage : (module StorageT)
  }

 and server = {
     ip : string;
     port : int;
     shutdown : unit -> unit;
     onshutdown : unit Lwt.t;
   }

 and connection = {
     root : root;
     mutable peer_name : string;
     sockaddr : Lwt_unix.sockaddr;
     lock : Lwt_mutex.t;
     input : Lwt_io.input_channel;
     output : Lwt_io.output_channel;
     close : unit -> unit Lwt.t;
     onclose : unit Lwt.t;
   }
