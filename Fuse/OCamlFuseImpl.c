#define FUSE_USE_VERSION 26

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/threads.h>

#include <fuse.h>
#include <memory.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>

static value* ocamlfuse_access_callback;
static value* ocamlfuse_create_callback;
static value* ocamlfuse_mknod_callback;
static value* ocamlfuse_destroy_callback;
static value* ocamlfuse_flush_callback;
static value* ocamlfuse_getattr_callback;
static value* ocamlfuse_fgetattr_callback;
static value* ocamlfuse_getxattr_callback;
static value* ocamlfuse_setxattr_callback;
static value* ocamlfuse_init_callback;
static value* ocamlfuse_mkdir_callback;
static value* ocamlfuse_open_callback;
static value* ocamlfuse_opendir_callback;
static value* ocamlfuse_read_callback;
static value* ocamlfuse_readdir_callback;
static value* ocamlfuse_rename_callback;
static value* ocamlfuse_release_callback;
static value* ocamlfuse_releasedir_callback;
static value* ocamlfuse_rmdir_callback;
static value* ocamlfuse_statfs_callback;
static value* ocamlfuse_sync_callback;
static value* ocamlfuse_syncdir_callback;
static value* ocamlfuse_truncate_callback;
static value* ocamlfuse_ftruncate_callback;
static value* ocamlfuse_unlink_callback;
static value* ocamlfuse_write_callback;

#define OCAMLFUSE_DEBUG(args...) fprintf (stderr, args)
// #define OCAMLFUSE_DEBUG(args...) /* args */

static int ocamlfuse_access(const char* path, int mode)
{
  OCAMLFUSE_DEBUG("C: ACCESS path=%s mode=%d\n", path, mode);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback2(*ocamlfuse_access_callback,
		   caml_copy_string(path),
		   Val_int(mode));
  int result_code = Int_val(result);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: ACCESS SUCCESS path=%s mode=%d result=%d\n", path, mode, result_code);
  return -result_code;
}

static int ocamlfuse_chmod(const char* path, mode_t mode)
{
  return 0;
}

static int ocamlfuse_chown(const char* path, uid_t uid, gid_t gid)
{
  return 0;
}

static int ocamlfuse_create(const char* path,
			    mode_t mode,
			    struct fuse_file_info* fi)
{
  OCAMLFUSE_DEBUG("C: CREATE path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback2(*ocamlfuse_create_callback,
		   caml_copy_string(path),
		   Val_int(mode));
  int result_code = Int_val(Field(result, 0));
  if (result_code) {
    caml_release_runtime_system();
    OCAMLFUSE_DEBUG("C: CREATE FAIL path=%s, result=%d\n", path, result_code);
    return -result_code;
  }
  long handle = Long_val(Field(result, 1));
  caml_release_runtime_system();
  fi->fh = handle;
  OCAMLFUSE_DEBUG("C: CREATE SUCCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_mknod(const char* path,
			   mode_t mode, dev_t dev)
{
  OCAMLFUSE_DEBUG("C: MKNOD path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback(*ocamlfuse_mknod_callback,
		  caml_copy_string(path));
  int result_code = Int_val(result);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: MKNOD SUCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static void ocamlfuse_destroy(void* data)
{
  OCAMLFUSE_DEBUG("C: DESTROY\n");
  caml_c_thread_register();
  caml_acquire_runtime_system();
  caml_callback(*ocamlfuse_destroy_callback, Val_unit);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: DESTROY SUCESS\n");
}

static int ocamlfuse_flush(const char* path, struct fuse_file_info* fi)
{
  OCAMLFUSE_DEBUG("C: FLUSH path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback2(*ocamlfuse_flush_callback,
		   caml_copy_string(path),
		   Val_long(fi->fh));
  int result_code = Int_val(result);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: FLUSH SUCCESS path=%s, result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_getattr(const char* path, struct stat* stbuf)
{
  OCAMLFUSE_DEBUG("C: GETATTR path=%s\n", path);
  memset(stbuf, 0, sizeof(struct stat));
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback(*ocamlfuse_getattr_callback,
		  caml_copy_string(path));
  int result_code = Int_val(Field(result, 0));
  if (result_code) {
    caml_release_runtime_system();
    OCAMLFUSE_DEBUG("C: GETATTR FAIL path=%s, result=%d\n", path, result_code);
    return -result_code;
  }
  value result_stat = Field(result, 1);
  int mode = Int_val(Field(result_stat, 0));
  long size = Long_val(Field(result_stat, 1));
  long time = Long_val(Field(result_stat, 2));
  caml_release_runtime_system();

  stbuf->st_mode = mode;
  stbuf->st_size = size;
  stbuf->st_mtime = time;
  stbuf->st_atime = time;
  stbuf->st_ctime = time;
  stbuf->st_nlink = 1;
  OCAMLFUSE_DEBUG("C: GETATTR SUCCESS path=%s, result=%d, mode=%d, size=%ld\n", path, result_code, mode, size);
  return -result_code;
}

static int ocamlfuse_fgetattr(const char* path, struct stat* stbuf,
			      struct fuse_file_info* fi)
{
  OCAMLFUSE_DEBUG("C: FGETATTR path=%s\n", path);
  memset(stbuf, 0, sizeof(struct stat));
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback2(*ocamlfuse_fgetattr_callback,
		   caml_copy_string(path),
		   Val_long(fi->fh));
  int result_code = Int_val(Field(result, 0));
  if (result_code) {
    caml_release_runtime_system();
    OCAMLFUSE_DEBUG("C: FGETATTR FAIL path=%s, result=%d\n", path, result_code);
    return -result_code;
  }
  value result_stat = Field(result, 1);
  int mode = Int_val(Field(result_stat, 0));
  int size = Int_val(Field(result_stat, 1));
  int time = Int_val(Field(result_stat, 2));
  caml_release_runtime_system();

  stbuf->st_mode = mode;
  stbuf->st_size = size;
  stbuf->st_mtime = time;
  stbuf->st_atime = time;
  stbuf->st_ctime = time;
  stbuf->st_nlink = 1;
  OCAMLFUSE_DEBUG("C: FGETATTR SUCCESS path=%s, result=%d, mode=%d, size=%d\n", path, result_code, mode, size);
  return -result_code;
}

static int ocamlfuse_getxattr(const char* path, const char* key, char* buf, size_t size, uint32_t position)
{
  OCAMLFUSE_DEBUG("C: GETXATTR path=%s key=%s\n", path, key);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback2(*ocamlfuse_getxattr_callback,
		   caml_copy_string(path),
		   caml_copy_string(key));
  int result_code = Int_val(Field(result, 0));
  if (result_code) {
    caml_release_runtime_system();
    OCAMLFUSE_DEBUG("C: GETXATTR FAIL path=%s, key=%s, result=%d\n", path, key, result_code);
    return -result_code;
  }
  value result_data = Field(result, 1);
  long result_data_size = caml_string_length(result_data);
  const char* result_data_c = String_val(result_data);
  memcpy(buf,
	 result_data_c,
	 result_data_size < size
	   ? result_data_size
	   : size);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: GETXATTR SUCCESS path=%s key=%s result=%d data=%s\n", path, key, result_code, result_data_c);
  return result_data_size;
}

static int ocamlfuse_setxattr(const char* path, const char* key, const char* data, size_t size, int flags, uint32_t position)
{
  OCAMLFUSE_DEBUG("C: SETXATTR path=%s key=%s data=%s\n", path, key, data);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value data_string = caml_alloc_string(size);
  char* data_string_p = String_val(data_string);
  memcpy(data_string_p, data, size);
  value result =
    caml_callback3(*ocamlfuse_setxattr_callback,
		   caml_copy_string(path),
		   caml_copy_string(key),
		   data_string);
  int result_code = Int_val(result);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: SETXATTR SUCCESS path=%s key=%s data=%s\n", path, key, data);
  return result_code;
}

static void* ocamlfuse_init(struct fuse_conn_info* conn)
{
  OCAMLFUSE_DEBUG("C: INIT\n");
  caml_c_thread_register();
  caml_acquire_runtime_system();
  caml_callback(*ocamlfuse_init_callback, Val_unit);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: INIT SUCESS\n");
  return NULL;
}

static int ocamlfuse_mkdir(const char* path, mode_t mode)
{
  OCAMLFUSE_DEBUG("C: MKDIR path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback2(*ocamlfuse_mkdir_callback,
		   caml_copy_string(path),
		   Val_int(mode));
  int result_code = Int_val(result);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: MKDIR SUCCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_open(const char* path, struct fuse_file_info* fi)
{
  OCAMLFUSE_DEBUG("C: OPEN path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback2(*ocamlfuse_open_callback,
		   caml_copy_string(path),
		   Val_int(fi->flags));
  int result_code = Int_val(Field(result, 0));
  if (result_code) {
    caml_release_runtime_system();
    OCAMLFUSE_DEBUG("C: OPEN FAIL path=%s result=%d\n", path, result_code);
    return -result_code;
  }
  fi->fh = Long_val(Field(result, 1));
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: OPEN SUCCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_opendir(const char* path, struct fuse_file_info* fi)
{
  OCAMLFUSE_DEBUG("C: OPENDIR path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback(*ocamlfuse_opendir_callback,
		  caml_copy_string(path));
  int result_code = Int_val(Field(result, 0));
  if (result_code) {
    caml_release_runtime_system();
    OCAMLFUSE_DEBUG("C: OPENDIR FAIL path=%s result=%d\n", path, result_code);
    return -result_code;
  }
  fi->fh = Long_val(Field(result, 1));
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: OPENDIR SUCCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_read(const char* path,
			  char* buf, size_t size, off_t offset,
			  struct fuse_file_info* fi)
{
  OCAMLFUSE_DEBUG("C: READ path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value args[5];
  args[0] = caml_copy_string(path);
  args[1] = Val_long(fi->fh);
  args[2] = Val_long(offset);
  args[3] = Val_long(size);
  value result = caml_callbackN(*ocamlfuse_read_callback, 4, args);
  int result_code = Int_val(Field(result, 0));
  if (result_code) {
    caml_release_runtime_system();
    OCAMLFUSE_DEBUG("C: READ FAILED path=%s result=%d\n", path, result_code);
    return -result_code;
  }
  value result_data = Field(result, 1);
  long result_data_size = caml_string_length(result_data);
  const char* result_data_c = String_val(result_data);
  memcpy(buf,
	 result_data_c,
	 result_data_size < size
	   ? result_data_size
	   : size);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: READ SUCCESS path=%s result=%d result_data_size=%ld size=%zu\n", path, result_code, result_data_size, size);
  return result_data_size;
}

static int ocamlfuse_readdir(const char* path,
			     void* buf,
			     fuse_fill_dir_t filler,
			     off_t offset,
			     struct fuse_file_info* fi)
{
  (void) offset;
  (void) fi;
  OCAMLFUSE_DEBUG("C: READDIR path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback2(*ocamlfuse_readdir_callback,
		   caml_copy_string(path),
		   Val_long(fi->fh));
  int result_code = Int_val(Field(result, 0));
  if (result_code) {
    caml_release_runtime_system();
    OCAMLFUSE_DEBUG("C: READDIR FAILED path=%s result=%d\n", path, result_code);
    return -result_code;
  }
  value result_dir = Field(result, 1);
  long size = Wosize_val(result_dir);
  for (long i = 0; i < size; i++) {
    filler(buf, String_val(Field(result_dir, i)), NULL, 0);
  }
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: READDIR SUCCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_rename(const char* from_path,
			    const char* to_path)
{
  OCAMLFUSE_DEBUG("C: RENAME from=%s to=%s\n", from_path, to_path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback2(*ocamlfuse_rename_callback,
		   caml_copy_string(from_path),
		   caml_copy_string(to_path));
  int result_code = Int_val(result);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: RENAME SUCCESS from=%s to=%s result=%d\n", from_path, to_path, result_code);
  return -result_code;
}

static int ocamlfuse_release(const char* path,
			     struct fuse_file_info* fi)
{
  OCAMLFUSE_DEBUG("C: RELEASE path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback2(*ocamlfuse_release_callback,
		   caml_copy_string(path),
		   Val_long(fi->fh));
  int result_code = Int_val(result);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: RELEASE SUCCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_releasedir(const char* path,
				struct fuse_file_info* fi)
{
  OCAMLFUSE_DEBUG("C: RELEASEDIR path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback2(*ocamlfuse_releasedir_callback,
		   caml_copy_string(path),
		   Val_long(fi->fh));
  int result_code = Int_val(result);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: RELEASEDIR SUCCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_rmdir(const char* path)
{
  OCAMLFUSE_DEBUG("C: RMDIR path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback(*ocamlfuse_rmdir_callback,
		  caml_copy_string(path));
  int result_code = Int_val(result);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: RMDIR SUCCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_statfs(const char* path, struct statvfs* statfsbuf)
{
  OCAMLFUSE_DEBUG("C: STATFS path=%s\n", path);
  memset(statfsbuf, 0, sizeof(struct statvfs));
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result = caml_callback(*ocamlfuse_statfs_callback, Val_unit);
  int result_code = Int_val(Field(result, 0));
  if (result_code) {
    caml_release_runtime_system();
    OCAMLFUSE_DEBUG("C: STATFS FAIL path=%s result=%d\n", path, result_code);
    return -result_code;
  }
  value result_statfs = Field(result, 1);
  statfsbuf->f_bsize = Long_val(Field(result_statfs, 0));
  statfsbuf->f_blocks = Long_val(Field(result_statfs, 1));
  statfsbuf->f_bfree = Long_val(Field(result_statfs, 2));
  statfsbuf->f_bavail = Long_val(Field(result_statfs, 3));
  statfsbuf->f_files = Long_val(Field(result_statfs, 4));
  statfsbuf->f_ffree = Long_val(Field(result_statfs, 5));
  statfsbuf->f_namemax = Long_val(Field(result_statfs, 6));
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: STATFS SUCCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_sync(const char* path,
			  int datasync,
			  struct fuse_file_info* fi)
{
  OCAMLFUSE_DEBUG("C: SYNC path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback2(*ocamlfuse_sync_callback,
		   caml_copy_string(path),
		   Val_long(fi->fh));
  int result_code = Int_val(result);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: SYNC SUCCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_syncdir(const char* path,
			     int datasync,
			     struct fuse_file_info* fi)
{
  OCAMLFUSE_DEBUG("C: SYNCDIR path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback2(*ocamlfuse_syncdir_callback,
		   caml_copy_string(path),
		   Val_long(fi->fh));
  int result_code = Int_val(result);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: SYNCDIR SUCCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_truncate(const char* path,
			      off_t offset)
{
  OCAMLFUSE_DEBUG("C: TRUNCATE path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback2(*ocamlfuse_truncate_callback,
		   caml_copy_string(path),
		   Val_long(offset));
  int result_code = Int_val(result);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: TRUNCATE SUCCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_ftruncate(const char* path,
			       off_t offset,
			       struct fuse_file_info* fi)
{
  OCAMLFUSE_DEBUG("C: FTRUNCATE path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback3(*ocamlfuse_ftruncate_callback,
		   caml_copy_string(path),
		   Val_long(fi->fh),
		   Val_long(offset));
  int result_code = Int_val(result);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: FTRUNCATE SUCCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_unlink(const char* path)
{
  OCAMLFUSE_DEBUG("C: UNLINK path=%s\n", path);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value result =
    caml_callback(*ocamlfuse_unlink_callback,
		  caml_copy_string(path));
  int result_code = Int_val(result);
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: UNLINK SUCCESS path=%s result=%d\n", path, result_code);
  return -result_code;
}

static int ocamlfuse_utimens(const char* path, const struct timespec tv[2]) {
  return 0;
}

static int ocamlfuse_write(const char* path,
			   const char* data, size_t size, off_t offset,
			   struct fuse_file_info* fi)
{
  OCAMLFUSE_DEBUG("C: WRITE path=%s size=%zu offset=%lld\n", path, size, offset);
  caml_c_thread_register();
  caml_acquire_runtime_system();
  value args[4];
  args[0] = caml_copy_string(path);
  args[1] = Val_long(fi->fh);
  args[2] = caml_alloc_string(size);
  char* data_string_p = String_val(args[2]);
  memcpy(data_string_p, data, size);
  args[3] = Val_long(offset);
  OCAMLFUSE_DEBUG("C: WRITE path=%s size=%zu offset=%lld CALLING OCAML CODE\n", path, size, offset);
  value result = caml_callbackN(*ocamlfuse_write_callback, 4, args);
  int result_code = Int_val(Field(result, 0));
  if (result_code) {
    caml_release_runtime_system();
    OCAMLFUSE_DEBUG("C: WRITE FAIL path=%s size=%zu offset=%lld result=%d\n", path, size, offset, result_code);
    return -result_code;
  }
  long written = Long_val(Field(result, 1));
  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: WRITE SUCCESS path=%s size=%zu offset=%lld written=%ld\n", path, size, offset, written);
  return written;
}

static struct fuse_operations ocamlfuse_operations = {
  .access       = ocamlfuse_access,
  .chmod        = ocamlfuse_chmod,
  .chown        = ocamlfuse_chown,
  .create       = ocamlfuse_create,
  .mknod        = ocamlfuse_mknod,
  .destroy      = ocamlfuse_destroy,
  .flush        = ocamlfuse_flush,
  .getattr      = ocamlfuse_getattr,
  .fgetattr     = ocamlfuse_fgetattr,
  .setxattr     = ocamlfuse_setxattr,
  .getxattr     = ocamlfuse_getxattr,
  .init         = ocamlfuse_init,
  .mkdir        = ocamlfuse_mkdir,
  .open         = ocamlfuse_open,
  .opendir      = ocamlfuse_opendir,
  .read         = ocamlfuse_read,
  .readdir      = ocamlfuse_readdir,
  .rename       = ocamlfuse_rename,
  .release      = ocamlfuse_release,
  .releasedir   = ocamlfuse_releasedir,
  .rmdir        = ocamlfuse_rmdir,
  .statfs       = ocamlfuse_statfs,
  .fsync        = ocamlfuse_sync,
  .fsyncdir     = ocamlfuse_syncdir,
  .truncate     = ocamlfuse_truncate,
  .ftruncate    = ocamlfuse_ftruncate,
  .unlink       = ocamlfuse_unlink,
  .utimens      = ocamlfuse_utimens,
  .write        = ocamlfuse_write,
};

CAMLprim value ocamlfuse_start_impl(value fuse_argv) {
  CAMLparam1(fuse_argv);
  CAMLlocal1(fuse_arg);

  int argc = Wosize_val(fuse_argv);
  char** argv = malloc(argc * sizeof(char*));

  for (int i = 0; i < argc; i++) {
    fuse_arg = Field(fuse_argv, i);
    char* arg = String_val(fuse_arg);
    long arg_size = caml_string_length(fuse_arg) + 1;
    argv[i] = malloc(arg_size);
    memcpy(argv[i], arg, arg_size);
  }

  ocamlfuse_access_callback = caml_named_value("ocamlfuse_access");
  ocamlfuse_create_callback = caml_named_value("ocamlfuse_create");
  ocamlfuse_mknod_callback = caml_named_value("ocamlfuse_mknod");
  ocamlfuse_destroy_callback = caml_named_value("ocamlfuse_destroy");
  ocamlfuse_flush_callback = caml_named_value("ocamlfuse_flush");
  ocamlfuse_getattr_callback = caml_named_value("ocamlfuse_getattr");
  ocamlfuse_fgetattr_callback = caml_named_value("ocamlfuse_fgetattr");
  ocamlfuse_getxattr_callback = caml_named_value("ocamlfuse_getxattr");
  ocamlfuse_setxattr_callback = caml_named_value("ocamlfuse_setxattr");
  ocamlfuse_init_callback = caml_named_value("ocamlfuse_init");
  ocamlfuse_mkdir_callback = caml_named_value("ocamlfuse_mkdir");
  ocamlfuse_open_callback = caml_named_value("ocamlfuse_open");
  ocamlfuse_opendir_callback = caml_named_value("ocamlfuse_opendir");
  ocamlfuse_read_callback = caml_named_value("ocamlfuse_read");
  ocamlfuse_readdir_callback = caml_named_value("ocamlfuse_readdir");
  ocamlfuse_rename_callback = caml_named_value("ocamlfuse_rename");
  ocamlfuse_release_callback = caml_named_value("ocamlfuse_release");
  ocamlfuse_releasedir_callback = caml_named_value("ocamlfuse_releasedir");
  ocamlfuse_rmdir_callback = caml_named_value("ocamlfuse_rmdir");
  ocamlfuse_statfs_callback = caml_named_value("ocamlfuse_statfs");
  ocamlfuse_sync_callback = caml_named_value("ocamlfuse_sync");
  ocamlfuse_syncdir_callback = caml_named_value("ocamlfuse_syncdir");
  ocamlfuse_truncate_callback = caml_named_value("ocamlfuse_truncate");
  ocamlfuse_ftruncate_callback = caml_named_value("ocamlfuse_ftruncate");
  ocamlfuse_unlink_callback = caml_named_value("ocamlfuse_unlink");
  ocamlfuse_write_callback = caml_named_value("ocamlfuse_write");

  caml_release_runtime_system();
  OCAMLFUSE_DEBUG("C: FUSE system starting.\n");
  fuse_main(argc, argv, &ocamlfuse_operations, NULL);
  for (int i = 0; i < argc; i++) {
    free (argv[i]);
  }
  free (argv);

  caml_acquire_runtime_system();
  CAMLreturn (Val_unit);
}
