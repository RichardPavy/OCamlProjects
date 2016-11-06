type errcode = int

external ocamlfuse_errcode_impl : unit -> errcode array = "ocamlfuse_errcode_impl"

let errcodes = ocamlfuse_errcode_impl ()

let i = ref 0
let e2big = errcodes.(!i) let _ = incr i
let eacces = errcodes.(!i) let _ = incr i
let eagain = errcodes.(!i) let _ = incr i
let ebadf = errcodes.(!i) let _ = incr i
let ebusy = errcodes.(!i) let _ = incr i
let echild = errcodes.(!i) let _ = incr i
let edeadlk = errcodes.(!i) let _ = incr i
let edom = errcodes.(!i) let _ = incr i
let eexist = errcodes.(!i) let _ = incr i
let efault = errcodes.(!i) let _ = incr i
let efbig = errcodes.(!i) let _ = incr i
let eintr = errcodes.(!i) let _ = incr i
let einval = errcodes.(!i) let _ = incr i
let eio = errcodes.(!i) let _ = incr i
let eisdir = errcodes.(!i) let _ = incr i
let emfile = errcodes.(!i) let _ = incr i
let emlink = errcodes.(!i) let _ = incr i
let enametoolong = errcodes.(!i) let _ = incr i
let enfile = errcodes.(!i) let _ = incr i
let enodev = errcodes.(!i) let _ = incr i
let enoent = errcodes.(!i) let _ = incr i
let enoexec = errcodes.(!i) let _ = incr i
let enolck = errcodes.(!i) let _ = incr i
let enomem = errcodes.(!i) let _ = incr i
let enospc = errcodes.(!i) let _ = incr i
let enosys = errcodes.(!i) let _ = incr i
let enotdir = errcodes.(!i) let _ = incr i
let enotempty = errcodes.(!i) let _ = incr i
let enotty = errcodes.(!i) let _ = incr i
let enxio = errcodes.(!i) let _ = incr i
let eperm = errcodes.(!i) let _ = incr i
let epipe = errcodes.(!i) let _ = incr i
let erange = errcodes.(!i) let _ = incr i
let erofs = errcodes.(!i) let _ = incr i
let espipe = errcodes.(!i) let _ = incr i
let esrch = errcodes.(!i) let _ = incr i
let exdev = errcodes.(!i) let _ = incr i
let ewouldblock = errcodes.(!i) let _ = incr i
let einprogress = errcodes.(!i) let _ = incr i
let ealready = errcodes.(!i) let _ = incr i
let enotsock = errcodes.(!i) let _ = incr i
let edestaddrreq = errcodes.(!i) let _ = incr i
let emsgsize = errcodes.(!i) let _ = incr i
let eprototype = errcodes.(!i) let _ = incr i
let enoprotoopt = errcodes.(!i) let _ = incr i
let eprotonosupport = errcodes.(!i) let _ = incr i
let esocktnosupport = errcodes.(!i) let _ = incr i
let eopnotsupp = errcodes.(!i) let _ = incr i
let epfnosupport = errcodes.(!i) let _ = incr i
let eafnosupport = errcodes.(!i) let _ = incr i
let eaddrinuse = errcodes.(!i) let _ = incr i
let eaddrnotavail = errcodes.(!i) let _ = incr i
let enetdown = errcodes.(!i) let _ = incr i
let enetunreach = errcodes.(!i) let _ = incr i
let enetreset = errcodes.(!i) let _ = incr i
let econnaborted = errcodes.(!i) let _ = incr i
let econnreset = errcodes.(!i) let _ = incr i
let enobufs = errcodes.(!i) let _ = incr i
let eisconn = errcodes.(!i) let _ = incr i
let enotconn = errcodes.(!i) let _ = incr i
let eshutdown = errcodes.(!i) let _ = incr i
let etoomanyrefs = errcodes.(!i) let _ = incr i
let etimedout = errcodes.(!i) let _ = incr i
let econnrefused = errcodes.(!i) let _ = incr i
let ehostdown = errcodes.(!i) let _ = incr i
let ehostunreach = errcodes.(!i) let _ = incr i
let eloop = errcodes.(!i) let _ = incr i
let eoverflow = errcodes.(!i) let _ = incr i
let ok = errcodes.(!i) let _ = incr i
let unknown = errcodes.(!i) let _ = incr i

type t =
  | E2BIG (* Argument list too long *)
  | EACCES (* Permission denied *)
  | EAGAIN (* Resource temporarily unavailable; try again *)
  | EBADF (* Bad file descriptor *)
  | EBUSY (* Resource unavailable *)
  | ECHILD (* No child process *)
  | EDEADLK (* Resource deadlock would occur *)
  | EDOM (* Domain error for math functions, etc. *)
  | EEXIST (* File exists *)
  | EFAULT (* Bad address *)
  | EFBIG (* File too large *)
  | EINTR (* Function interrupted by signal *)
  | EINVAL (* Invalid argument *)
  | EIO (* Hardware I/O error *)
  | EISDIR (* Is a directory *)
  | EMFILE (* Too many open files by the process *)
  | EMLINK (* Too many links *)
  | ENAMETOOLONG (* Filename too long *)
  | ENFILE (* Too many open files in the system *)
  | ENODEV (* No such device *)
  | ENOENT (* No such file or directory *)
  | ENOEXEC (* Not an executable file *)
  | ENOLCK (* No locks available *)
  | ENOMEM (* Not enough memory *)
  | ENOSPC (* No space left on device *)
  | ENOSYS (* Function not supported *)
  | ENOTDIR (* Not a directory *)
  | ENOTEMPTY (* Directory not empty *)
  | ENOTTY (* Inappropriate I/O control operation *)
  | ENXIO (* No such device or address *)
  | EPERM (* Operation not permitted *)
  | EPIPE (* Broken pipe *)
  | ERANGE (* Result too large *)
  | EROFS (* Read-only file system *)
  | ESPIPE (* Invalid seek e.g. on a pipe *)
  | ESRCH (* No such process *)
  | EXDEV (* Invalid link *)
  | EWOULDBLOCK (* Operation would block *)
  | EINPROGRESS (* Operation now in progress *)
  | EALREADY (* Operation already in progress *)
  | ENOTSOCK (* Socket operation on non-socket *)
  | EDESTADDRREQ (* Destination address required *)
  | EMSGSIZE (* Message too long *)
  | EPROTOTYPE (* Protocol wrong type for socket *)
  | ENOPROTOOPT (* Protocol not available *)
  | EPROTONOSUPPORT (* Protocol not supported *)
  | ESOCKTNOSUPPORT (* Socket type not supported *)
  | EOPNOTSUPP (* Operation not supported on socket *)
  | EPFNOSUPPORT (* Protocol family not supported *)
  | EAFNOSUPPORT
  | EADDRINUSE (* Address already in use *)
  | EADDRNOTAVAIL (* Can't assign requested address *)
  | ENETDOWN (* Network is down *)
  | ENETUNREACH (* Network is unreachable *)
  | ENETRESET (* Network dropped connection on reset *)
  | ECONNABORTED (* Software caused connection abort *)
  | ECONNRESET (* Connection reset by peer *)
  | ENOBUFS (* No buffer space available *)
  | EISCONN (* Socket is already connected *)
  | ENOTCONN (* Socket is not connected *)
  | ESHUTDOWN (* Can't send after socket shutdown *)
  | ETOOMANYREFS (* Too many references: can't splice *)
  | ETIMEDOUT (* Connection timed out *)
  | ECONNREFUSED (* Connection refused *)
  | EHOSTDOWN (* Host is down *)
  | EHOSTUNREACH (* No route to host *)
  | ELOOP (* Too many levels of symbolic links *)
  | EOVERFLOW (* File size or position not representable *)
  | OK
  | Unknown

let to_errcode (error : t) = errcodes.(Obj.magic error)

let errcode_to_error_map = Hashtbl.create (Array.length errcodes)

let _ = for i = 0 to Array.length errcodes - 1 do
	  Hashtbl.add errcode_to_error_map errcodes.(i) (Obj.magic i)
	done

let from_errcode error = try Hashtbl.find errcode_to_error_map error
			 with Not_found -> Unknown

let strings = [|
    "E2BIG" ;
    "EACCES" ;
    "EAGAIN" ;
    "EBADF" ;
    "EBUSY" ;
    "ECHILD" ;
    "EDEADLK" ;
    "EDOM" ;
    "EEXIST" ;
    "EFAULT" ;
    "EFBIG" ;
    "EINTR" ;
    "EINVAL" ;
    "EIO" ;
    "EISDIR" ;
    "EMFILE" ;
    "EMLINK" ;
    "ENAMETOOLONG" ;
    "ENFILE" ;
    "ENODEV" ;
    "ENOENT" ;
    "ENOEXEC" ;
    "ENOLCK" ;
    "ENOMEM" ;
    "ENOSPC" ;
    "ENOSYS" ;
    "ENOTDIR" ;
    "ENOTEMPTY" ;
    "ENOTTY" ;
    "ENXIO" ;
    "EPERM" ;
    "EPIPE" ;
    "ERANGE" ;
    "EROFS" ;
    "ESPIPE" ;
    "ESRCH" ;
    "EXDEV" ;
    "EWOULDBLOCK" ;
    "EINPROGRESS" ;
    "EALREADY" ;
    "ENOTSOCK" ;
    "EDESTADDRREQ" ;
    "EMSGSIZE" ;
    "EPROTOTYPE" ;
    "ENOPROTOOPT" ;
    "EPROTONOSUPPORT" ;
    "ESOCKTNOSUPPORT" ;
    "EOPNOTSUPP" ;
    "EPFNOSUPPORT" ;
    "EAFNOSUPPORT" ;
    "EADDRINUSE" ;
    "EADDRNOTAVAIL" ;
    "ENETDOWN" ;
    "ENETUNREACH" ;
    "ENETRESET" ;
    "ECONNABORTED" ;
    "ECONNRESET" ;
    "ENOBUFS" ;
    "EISCONN" ;
    "ENOTCONN" ;
    "ESHUTDOWN" ;
    "ETOOMANYREFS" ;
    "ETIMEDOUT" ;
    "ECONNREFUSED" ;
    "EHOSTDOWN" ;
    "EHOSTUNREACH" ;
    "ELOOP" ;
    "EOVERFLOW" ;
    "OK" ;
    "Unknown" ;
   |]

let to_string (error : t) = strings.(Obj.magic error)

exception Error of t

let throw error = raise (Error error)

(* Test: ocamlopt -o ErrCode ErrCodeImpl.c ErrCode.mli ErrCode.ml ; ./ErrCode

let _ =
  for i = 0 to Array.length errcodes - 1 do
    let error : t = Obj.magic i in
    print_int (Obj.magic (from_errcode (to_errcode error)));
    print_string " : ";
    print_string (to_string error);
    print_string " = ";
    print_int (to_errcode error);
    print_newline ()
  done
*)
