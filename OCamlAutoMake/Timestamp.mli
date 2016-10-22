type file_kind =
  | Special
  | Folder
  | File
  | Null

val kind : File.t -> file_kind
val get : File.t -> float
val clear : File.t -> unit
