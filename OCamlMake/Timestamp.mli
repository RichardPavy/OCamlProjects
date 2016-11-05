type file_kind =
  | Special
  | Folder
  | File
  | Null

val kind : Utils_File.t -> file_kind
val get : Utils_File.t -> float
val clear : Utils_File.t -> unit
