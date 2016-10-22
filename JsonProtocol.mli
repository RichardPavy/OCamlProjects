(** Module for JSON protocol. *)

open SyncTypes

module Y = Yojson.Basic
type operation = Y.json -> connection -> Y.json Lwt.t

val register_operation : string -> operation -> unit

module Sync : (Protocol.Sig with
		 type 'a response = Y.json and
		 type 'a request = Y.json)

module Async : (Protocol.Sig with
		  type 'a response = Y.json and
		  type 'a request = Y.json)
