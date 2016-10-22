(** Module for Native protocol. *)

open SyncTypes

module Sync : (Protocol.Sig with
		 type 'a response = 'a and
		 type 'a request = connection -> 'a Lwt.t)
module Async : (Protocol.Sig with
		  type 'a response = 'a and
		  type 'a request = connection -> 'a Lwt.t)
