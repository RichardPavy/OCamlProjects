type file_kind =
  | Special
  | Folder
  | File
  | Null

val kind : Utils.File.t -> file_kind
val get : Utils.File.t -> float
val clear : Utils.File.t -> unit
