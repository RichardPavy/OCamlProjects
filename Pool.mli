module Bits :
sig
  (** Returns the low bits of an int. *)
  val low : int -> int

  (** Returns the high bits of an int. *)
  val high : int -> int

  (** Creates an int with the given high an low bits. *)
  val int : high:int -> low:int -> int
end

module Key :
sig
  type 'a t
  val index : 'a t -> int
  val key : int -> 'a t
end

(** Keys of buckets in a pool. *)
type 'a key = 'a Key.t

(** Type for buckets allocated in a pool. *)
type 'a bucket = { key : 'a key; resource : 'a; }

(** Exception thrown when a bucket is not found under a given key. *)
exception UnavailableResource

(** Exception thrown when using a key that has been freed and stores a different bucket. *)
exception SeedMismatch

(** Type of modules that create resources. *)
module type ResourceType =
  sig
    type 'a t
    val create : unit -> 'a t
  end

(** Functor that creates pools. *)
module Make :
  functor (Resource : ResourceType) ->
    sig
      type 'a resource = 'a Resource.t

      (** Creates a new resource *)
      val alloc : unit -> 'a resource bucket

      (** Removes the resource from the pool. The associated key can no longer be used. *)
      val free : 'a resource key -> unit

      (** Returns a previously allocated resource given its key. *)
      val get : 'a resource key -> 'a resource
    end
