type flags
type t =
  | RDONLY
  | WRONLY
  | RDWR
  | NONBLOCK
  | APPEND
  | CREAT
  | TRUNC
  | EXCL
  | SHLOCK
  | EXLOCK
  | NOFOLLOW
  | SYMLINK
  | EVTONLY
  | CLOEXEC

val rdonly : flags
val wronly : flags
val rdwr : flags
val nonblock : flags
val append : flags
val creat : flags
val trunc : flags
val excl : flags
val shlock : flags
val exlock : flags
val nofollow : flags
val symlink : flags
val evtonly : flags
val cloexec : flags

val to_code : t -> flags
val has : flags -> t -> bool
val of_flags : flags -> t list
val to_flags : t list -> flags
