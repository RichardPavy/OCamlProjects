#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/threads.h>

#include <errno.h>

static int unix_error_table[] = {
  E2BIG,
  EACCES,
  EAGAIN,
  EBADF,
  EBUSY,
  ECHILD,
  EDEADLK,
  EDOM,
  EEXIST,
  EFAULT,
  EFBIG,
  EINTR,
  EINVAL,
  EIO,
  EISDIR,
  EMFILE,
  EMLINK,
  ENAMETOOLONG,
  ENFILE,
  ENODEV,
  ENOENT,
  ENOEXEC,
  ENOLCK,
  ENOMEM,
  ENOSPC,
  ENOSYS,
  ENOTDIR,
  ENOTEMPTY,
  ENOTTY,
  ENXIO,
  EPERM,
  EPIPE,
  ERANGE,
  EROFS,
  ESPIPE,
  ESRCH,
  EXDEV,
  EWOULDBLOCK,
  EINPROGRESS,
  EALREADY,
  ENOTSOCK,
  EDESTADDRREQ,
  EMSGSIZE,
  EPROTOTYPE,
  ENOPROTOOPT,
  EPROTONOSUPPORT,
  ESOCKTNOSUPPORT,
  EOPNOTSUPP,
  EPFNOSUPPORT,
  EAFNOSUPPORT,
  EADDRINUSE,
  EADDRNOTAVAIL,
  ENETDOWN,
  ENETUNREACH,
  ENETRESET,
  ECONNABORTED,
  ECONNRESET,
  ENOBUFS,
  EISCONN,
  ENOTCONN,
  ESHUTDOWN,
  ETOOMANYREFS,
  ETIMEDOUT,
  ECONNREFUSED,
  EHOSTDOWN,
  EHOSTUNREACH,
  ELOOP,
  EOVERFLOW,
  0, // OK
  ELAST + 1 // Unknown
};

CAMLprim value ocamlfuse_errcode_impl(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(errcodes);
  int size = sizeof(unix_error_table) / sizeof(unix_error_table[0]);
  errcodes = caml_alloc(size, 0);
  for (int i = 0; i < size; i++) {
    Field(errcodes, i) = Val_int(unix_error_table[i]);
  }
  CAMLreturn (errcodes);
}
