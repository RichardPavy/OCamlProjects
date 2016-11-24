type t
val parse : string -> t
val parsef : ('a, unit, string, t) format4 -> 'a
val to_string : t -> string

(** /folder/file.ext -> file *)
val basename : t -> string

(** /folder/file.ext -> ext *)
val extension : t -> string

(** /folder/file.ext -> file.ext *)
val filename : t -> string

(** /folder/file.ext -> /folder/file *)
val full_base : t -> string

(** /folder/file.ext -> /folder *)
val parent : t -> t
val child : t -> string -> t

(** /folder/file.ext -> /folder/file *)
val strip_ext : t -> t

(** /folder/file.ext other -> /folder/file.other *)
val with_ext : string -> t -> t

(** / *)
val root : t

(** Whether the file starts at / or is relative. *)
val is_absolute : t -> bool

(** True for . and / *)
val is_root : t -> bool

(** Whether the file is in the root folder. *)
val is_toplevel : t -> bool

val chroot : int -> t -> t

val map : (string -> string) -> t -> t
