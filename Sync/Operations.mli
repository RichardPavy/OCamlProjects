val add_peer :
  ip:string ->
  port:int ->
  SyncTypes.root ->
  [> `Both | `None | `PeerOnly | `SelfOnly ] Lwt.t

type json = Yojson.Basic.json

module Json :
sig
  val plus : int -> int -> json
  val minus : int -> int -> json
  val times : int -> int -> json
  val divide : int -> int -> json
  val modulo : int -> int -> json
  val kill : json
  val kill_peer : string -> json
  val add_peer : string -> int -> json
  val remove_peer : string -> json
  val root_name : json
  val peers : json
  val forward : string -> json -> json
  val forward_native : string -> json -> json
end
