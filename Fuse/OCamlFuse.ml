type kind =
  | REG
  | DIR

 and stat = {
     kind : kind ; (* Kind. *)
     size : int ; (* Size in bytes. *)
     time : int ; (* Modification time in seconds. *)
   }

 and stat_internal = {
     mode_internal : int ; (* kind & perm *)
     size_internal : int ; (* Size in bytes. *)
     time_internal : int ; (* Modification time in seconds. *)
   }

 and handle = int
 and errcode = ErrCode.errcode
 and openflags = OpenFlags.flags
 and mode = int
 and statfs = {
     bsize   : int ; (* 0. File system block size *)
  (* frsize  : int ;       Fundamental file system block size *)
     blocks  : int ; (* 1. Blocks on FS in units of f_frsize *)
     bfree   : int ; (* 2. Free blocks *)
     bavail  : int ; (* 3. Blocks available to non-root *)
     files   : int ; (* 4. Total inodes *)
     ffree   : int ; (* 5. Free inodes *)
  (* favail  : int ;       Free inodes for non-root *)
  (* fsid    : int ;       Filesystem ID *)
  (* flag    : int ;       Bit mask of values *)
     namemax : int ; (* 6. Max file name length *)
   }

 and filesystem = {
     access     : path: string -> mode: mode -> unit ;
     create     : path: string -> mode: mode -> handle ;
     mknod      : path: string -> mode: mode -> unit ;
     destroy    : unit -> unit ;
     flush      : path: string -> handle: handle -> unit ;
     getattr    : path: string -> stat ;
     fgetattr   : path: string -> handle: handle -> stat ;
     getxattr   : path: string -> key: string -> string ;
     setxattr   : path: string -> key: string -> value: string -> unit ;
     init       : unit -> unit ;
     mkdir      : path: string -> mode: mode -> unit ;
     fopen      : path: string -> flags: openflags -> handle ;
     opendir    : path: string -> handle ;
     read       : path: string -> handle: handle -> offset: int -> size: int -> string ;
     readdir    : path: string -> handle: handle -> string array ;
     rename     : from_path: string -> to_path: string -> unit ;
     release    : path: string -> handle: handle -> unit ;
     releasedir : path: string -> handle: handle -> unit ;
     rmdir      : path: string -> unit ;
     statfs     : unit -> statfs ;
     sync       : path: string -> handle: handle -> unit ;
     syncdir    : path: string -> handle: handle -> unit ;
     truncate   : path: string -> size: int -> unit ;
     ftruncate  : path: string -> handle: handle -> size: int -> unit ;
     unlink     : path: string -> unit ;
     write      : path: string -> handle: handle -> data: string -> offset: int -> int ;
   }

 and filesystem_internal = {
     internal_access     : string -> mode -> errcode ;
     internal_create     : string -> mode -> errcode * handle ;
     internal_mknod      : string -> mode -> errcode ;
     internal_destroy    : unit -> unit ;
     internal_flush      : string -> handle -> errcode ;
     internal_getattr    : string -> errcode * stat ;
     internal_fgetattr   : string -> handle -> errcode * stat ;
     internal_getxattr   : string -> string -> errcode * string ;
     internal_setxattr   : string -> string -> string -> errcode ;
     internal_init       : unit -> unit ;
     internal_mkdir      : string -> mode -> errcode ;
     internal_fopen      : string -> openflags -> errcode * handle ;
     internal_opendir    : string -> errcode * handle ;
     internal_read       : string -> handle -> int -> int -> errcode * string ;
     internal_readdir    : string -> handle -> errcode * string array ;
     internal_rename     : string -> string -> errcode ;
     internal_release    : string -> handle -> errcode ;
     internal_releasedir : string -> handle -> errcode ;
     internal_rmdir      : string -> errcode ;
     internal_statfs     : unit -> errcode * statfs ;
     internal_sync       : string -> handle -> errcode ;
     internal_syncdir    : string -> handle -> errcode ;
     internal_truncate   : string -> int -> errcode ;
     internal_ftruncate  : string -> handle -> int -> errcode ;
     internal_unlink     : string -> errcode ;
     internal_write      : string -> handle -> string -> int -> errcode * int ;
   }

let internal_of_stat { kind ; size ; time } =
  { mode_internal = 0o777 lor (match kind with
			       | REG -> 0o100000 (* TODO: put in C. *)
			       | DIR -> 0o040000) ;
    size_internal = size ;
    time_internal = time }

let null_handle = 0
and getattr_error = { kind = REG ; size = -1 ; time = -1 }
and statfs_error = {
    bsize = 0 ;
    blocks = 0 ;
    bfree = 0 ;
    bavail = 0 ;
    files = 0 ;
    ffree = 0 ;
    namemax = 0 ;
  }

let internal_of_filesystem ~debug fs =
  let mutex = Mutex.create () in
  let lock f () =
    Mutex.lock mutex;
    try
      let result = f () in
      Mutex.unlock mutex;
      result
    with exn ->
      Mutex.unlock mutex;
      raise exn
  in
  let debug =
    if debug
    then
      let prerr x =
	flush_all ();
	prerr_string x;
	prerr_newline ();
	flush_all ()
      in fun f () ->
	 let t = 1 in (*Thread.self () in*)
	 "let t = string_of_int t in";
	 prerr ("1 :::: " ^ "t" ^ " ::::  before init gc..." );
	 Gc.full_major () ;
	 prerr "2 ::::  after init gc..." ;

	 let result = f () in

	 prerr "3 ::::  before final gc..." ;
	 Gc.full_major () ;
	 prerr "4 :::: after final gc..." ;
	 result

    else fun f () -> f ()
  in
  let run f = debug (lock f) in    
  let wrap0 f = try run f () ; ErrCode.ok with
		| ErrCode.Error error -> ErrCode.to_errcode error
		| exn -> prerr_string (Printexc.to_string exn);
			 prerr_newline ();
			 flush_all ();
			 ErrCode.unknown
  and wrap1 f default_value = try ErrCode.ok, run f () with
			      | ErrCode.Error error ->
				 (ErrCode.to_errcode error),
				 default_value
			      | exn -> prerr_string (Printexc.to_string exn);
				       prerr_newline ();
				       flush_all ();
				       ErrCode.unknown, default_value
  in
  let internal_access path mode = wrap0 (fun () -> fs.access ~path ~mode)
  and internal_create path mode = wrap1 (fun () -> fs.create ~path ~mode) null_handle
  and internal_mknod path mode = wrap0 (fun () -> fs.mknod ~path ~mode)
  and internal_destroy = fs.destroy
  and internal_flush path handle = wrap0 (fun () -> fs.flush ~path ~handle)
  and internal_getattr path = wrap1 (fun () -> fs.getattr ~path) getattr_error
  and internal_fgetattr path handle = wrap1 (fun () -> fs.fgetattr ~path ~handle) getattr_error
  and internal_getxattr path key = wrap1 (fun () -> fs.getxattr ~path ~key) ""
  and internal_setxattr path key value = wrap0 (fun () -> fs.setxattr ~path ~key ~value)
  and internal_init = fs.init
  and internal_mkdir path mode = wrap0 (fun () -> fs.mkdir ~path ~mode)
  and internal_fopen path openflags = wrap1 (fun () -> fs.fopen ~path ~flags: openflags) null_handle
  and internal_opendir path = wrap1 (fun () -> fs.opendir ~path) null_handle
  and internal_read path handle offset size = wrap1 (fun () -> fs.read ~path ~handle ~offset ~size) ""
  and internal_readdir path handle = wrap1 (fun () -> fs.readdir ~path ~handle) [||]
  and internal_rename from_path to_path = wrap0 (fun () -> fs.rename ~from_path ~to_path)
  and internal_release path handle = wrap0 (fun () -> fs.release ~path ~handle)
  and internal_releasedir path handle = wrap0 (fun () -> fs.releasedir ~path ~handle)
  and internal_rmdir path = wrap0 (fun () -> fs.rmdir ~path)
  and internal_statfs () = wrap1 fs.statfs statfs_error
  and internal_sync path handle = wrap0 (fun () -> fs.sync ~path ~handle)
  and internal_syncdir path handle = wrap0 (fun () -> fs.syncdir ~path ~handle)
  and internal_truncate path size = wrap0 (fun () -> fs.truncate ~path ~size)
  and internal_ftruncate path handle size = wrap0 (fun () -> fs.ftruncate ~path ~handle ~size)
  and internal_unlink path = wrap0 (fun () -> fs.unlink ~path)
  and internal_write path handle data offset = wrap1 (fun () -> fs.write ~path ~handle ~data ~offset) 0
  in { internal_access ;
       internal_create ;
       internal_mknod ;
       internal_destroy ;
       internal_flush ;
       internal_getattr ;
       internal_fgetattr ;
       internal_getxattr ;
       internal_setxattr ;
       internal_init ;
       internal_mkdir ;
       internal_fopen ;
       internal_opendir ;
       internal_read ;
       internal_readdir ;
       internal_rename ;
       internal_release ;
       internal_releasedir ;
       internal_rmdir ;
       internal_statfs ;
       internal_sync ;
       internal_syncdir ;
       internal_truncate ;
       internal_ftruncate ;
       internal_unlink ;
       internal_write }

external ocamlfuse_start_impl : string array -> unit = "ocamlfuse_start_impl"

let start dir options filesystem =
  let run =
    if List.mem `Foreground options
    then fun f -> f ()
    else fun f ->
	 if Unix.fork () = 0
	 then f ()
	 else ()
  in
  let aux () =
    let filesystem = internal_of_filesystem
		       ~debug:(List.mem `Debug options)
		       filesystem in
    Callback.register "ocamlfuse_access" filesystem.internal_access;
    Callback.register "ocamlfuse_create" filesystem.internal_create;
    Callback.register "ocamlfuse_mknod" filesystem.internal_mknod;
    Callback.register "ocamlfuse_destroy" filesystem.internal_destroy;
    Callback.register "ocamlfuse_flush" filesystem.internal_flush;
    Callback.register "ocamlfuse_getattr"
		      (let getattr = filesystem.internal_getattr in
		       fun path -> let errcode, stat = getattr path in
				   errcode, internal_of_stat stat);
    Callback.register "ocamlfuse_fgetattr"
		      (let fgetattr = filesystem.internal_fgetattr in
		       fun path handle -> let errcode, stat = fgetattr path handle in
					  errcode, internal_of_stat stat);
    Callback.register "ocamlfuse_getxattr" filesystem.internal_getxattr;
    Callback.register "ocamlfuse_setxattr" filesystem.internal_setxattr;
    Callback.register "ocamlfuse_init" filesystem.internal_init;
    Callback.register "ocamlfuse_mkdir" filesystem.internal_mkdir;
    Callback.register "ocamlfuse_open" filesystem.internal_fopen;
    Callback.register "ocamlfuse_opendir" filesystem.internal_opendir;
    Callback.register "ocamlfuse_read" filesystem.internal_read;
    Callback.register "ocamlfuse_readdir" filesystem.internal_readdir;
    Callback.register "ocamlfuse_rename" filesystem.internal_rename;
    Callback.register "ocamlfuse_release" filesystem.internal_release;
    Callback.register "ocamlfuse_releasedir" filesystem.internal_releasedir;
    Callback.register "ocamlfuse_releasedir" filesystem.internal_releasedir;
    Callback.register "ocamlfuse_rmdir" filesystem.internal_rmdir;
    Callback.register "ocamlfuse_statfs" filesystem.internal_statfs;
    Callback.register "ocamlfuse_sync" filesystem.internal_sync;
    Callback.register "ocamlfuse_syncdir" filesystem.internal_syncdir;
    Callback.register "ocamlfuse_truncate" filesystem.internal_truncate;
    Callback.register "ocamlfuse_ftruncate" filesystem.internal_ftruncate;
    Callback.register "ocamlfuse_unlink" filesystem.internal_unlink;
    Callback.register "ocamlfuse_write" filesystem.internal_write;

    let options = List.rev_map (function `Debug -> "-d"
				       | `Foreground -> "-f"
				       | `SingleThreaded -> "-s")
			       options
    in
    ocamlfuse_start_impl (Array.of_list ([ "OCamlFuse" ; dir ] @ options))
  in run aux
