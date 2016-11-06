#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/threads.h>

#include <sys/fcntl.h>

static int openflags_table[] = {
  O_RDONLY,
  O_WRONLY,
  O_RDWR,
  O_NONBLOCK,
  O_APPEND,
  O_CREAT,
  O_TRUNC,
  O_EXCL,
  O_SHLOCK,
  O_EXLOCK,
  O_NOFOLLOW,
  O_SYMLINK,
  O_EVTONLY,
  O_CLOEXEC
};

CAMLprim value ocamlfuse_openflags_impl(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(openflags);
  int size = sizeof(openflags_table) / sizeof(openflags_table[0]);
  openflags = caml_alloc(size, 0);
  for (int i = 0; i < size; i++) {
    Field(openflags, i) = Val_int(openflags_table[i]);
  }
  CAMLreturn (openflags);
}
