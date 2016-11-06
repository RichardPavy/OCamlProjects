type errcode
val e2big : errcode
val eacces : errcode
val eagain : errcode
val ebadf : errcode
val ebusy : errcode
val echild : errcode
val edeadlk : errcode
val edom : errcode
val eexist : errcode
val efault : errcode
val efbig : errcode
val eintr : errcode
val einval : errcode
val eio : errcode
val eisdir : errcode
val emfile : errcode
val emlink : errcode
val enametoolong : errcode
val enfile : errcode
val enodev : errcode
val enoent : errcode
val enoexec : errcode
val enolck : errcode
val enomem : errcode
val enospc : errcode
val enosys : errcode
val enotdir : errcode
val enotempty : errcode
val enotty : errcode
val enxio : errcode
val eperm : errcode
val epipe : errcode
val erange : errcode
val erofs : errcode
val espipe : errcode
val esrch : errcode
val exdev : errcode
val ewouldblock : errcode
val einprogress : errcode
val ealready : errcode
val enotsock : errcode
val edestaddrreq : errcode
val emsgsize : errcode
val eprototype : errcode
val enoprotoopt : errcode
val eprotonosupport : errcode
val esocktnosupport : errcode
val eopnotsupp : errcode
val epfnosupport : errcode
val eafnosupport : errcode
val eaddrinuse : errcode
val eaddrnotavail : errcode
val enetdown : errcode
val enetunreach : errcode
val enetreset : errcode
val econnaborted : errcode
val econnreset : errcode
val enobufs : errcode
val eisconn : errcode
val enotconn : errcode
val eshutdown : errcode
val etoomanyrefs : errcode
val etimedout : errcode
val econnrefused : errcode
val ehostdown : errcode
val ehostunreach : errcode
val eloop : errcode
val eoverflow : errcode
val ok : errcode
val unknown : errcode

type t =
  | E2BIG
  | EACCES
  | EAGAIN
  | EBADF
  | EBUSY
  | ECHILD
  | EDEADLK
  | EDOM
  | EEXIST
  | EFAULT
  | EFBIG
  | EINTR
  | EINVAL
  | EIO
  | EISDIR
  | EMFILE
  | EMLINK
  | ENAMETOOLONG
  | ENFILE
  | ENODEV
  | ENOENT
  | ENOEXEC
  | ENOLCK
  | ENOMEM
  | ENOSPC
  | ENOSYS
  | ENOTDIR
  | ENOTEMPTY
  | ENOTTY
  | ENXIO
  | EPERM
  | EPIPE
  | ERANGE
  | EROFS
  | ESPIPE
  | ESRCH
  | EXDEV
  | EWOULDBLOCK
  | EINPROGRESS
  | EALREADY
  | ENOTSOCK
  | EDESTADDRREQ
  | EMSGSIZE
  | EPROTOTYPE
  | ENOPROTOOPT
  | EPROTONOSUPPORT
  | ESOCKTNOSUPPORT
  | EOPNOTSUPP
  | EPFNOSUPPORT
  | EAFNOSUPPORT
  | EADDRINUSE
  | EADDRNOTAVAIL
  | ENETDOWN
  | ENETUNREACH
  | ENETRESET
  | ECONNABORTED
  | ECONNRESET
  | ENOBUFS
  | EISCONN
  | ENOTCONN
  | ESHUTDOWN
  | ETOOMANYREFS
  | ETIMEDOUT
  | ECONNREFUSED
  | EHOSTDOWN
  | EHOSTUNREACH
  | ELOOP
  | EOVERFLOW
  | OK
  | Unknown
val to_errcode : t -> errcode
val from_errcode : errcode -> t
val to_string : t -> string

exception Error of t
val throw : t -> 'a
