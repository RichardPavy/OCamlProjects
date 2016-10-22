open OCamlFuse

type file = { mutable contents : Buffer.t ;
	      mutable timestamp : float ;
	      xattr : (string, string) Hashtbl.t }

 and folder = { dir : (string, element) Hashtbl.t ;
		dir_xattr : (string, string) Hashtbl.t }

 and element =
   | File of folder * file
   | Folder of folder * folder

let split_path = Utils.split_path
let throw = ErrCode.throw

let root =
  let folder = { dir = Hashtbl.create 10 ; dir_xattr = Hashtbl.create 10 } in
  Folder (folder, folder)

let rec find_file path =
  let path = List.rev (split_path path) in
  let rec aux file path = match file, path with
    | _, [] -> file
    | Folder (_, { dir = folder }), (leg :: path) ->
       let file = Hashtbl.find folder leg in
       aux file path
    | (File _), _ -> throw ErrCode.ENOTDIR
  in
  try aux root path
  with Not_found -> throw ErrCode.ENOENT

let find_basefolder path =
  match split_path path with
  | [] -> throw ErrCode.EINVAL
  | filename :: path ->
     match find_file (Utils.join path) with
     | File _ -> throw ErrCode.ENOTDIR
     | Folder (_, folder) -> filename, folder

let statfs_base = {
    bsize = 1024 ;
    blocks = 1024 * 1024 ;
    bfree = 512 * 1024 ;
    bavail = 512 * 1024 ;
    files = 1024 * 1024 ;
    ffree = 1024 * 1024 ;
    namemax = 1024 * 1024 ;
  }

let files = ref 0
let file_size = ref 0

let access ~path ~mode = ignore (find_file path)

let create ~path ~mode =
  let filename, folder = find_basefolder path in
  if Hashtbl.mem folder.dir filename
  then throw ErrCode.EEXIST
  else
    let file = { contents = Buffer.create 0 ;
		 timestamp = Unix.gettimeofday() ;
		 xattr = Hashtbl.create 10 } in
    Hashtbl.add folder.dir filename
		(File (folder, file));
    incr files;
    null_handle

let mknod ~path ~mode = ignore (create ~path ~mode)

let destroy () = ()

let flush ~path ~handle = ()

let getattr ~path =
  match find_file path with
  | File (_, file) -> { kind = REG ;
			size = Buffer.length file.contents ;
			time = int_of_float file.timestamp }
  | Folder (_, folder) -> { kind = DIR ;
			    size = Hashtbl.length folder.dir ;
			    time = 0 }

let fgetattr ~path ~handle = getattr ~path

let find_xattr path =
  match find_file path
  with File (_, { xattr }) -> xattr
     | Folder (_, { dir_xattr }) -> dir_xattr
				      
let getxattr ~path ~key =
  let xattr = find_xattr path in
  try Hashtbl.find xattr key
  with Not_found -> ""
		      
let setxattr ~path ~key ~value =
  let xattr = find_xattr path in
  Hashtbl.replace xattr key value

let init () = ()

let mkdir ~path ~mode =
  let file, folder = find_basefolder path in
  if Hashtbl.mem folder.dir file
  then throw ErrCode.EEXIST
  else begin
      Hashtbl.add folder.dir file
		  (Folder (folder, { dir = Hashtbl.create 10 ;
				     dir_xattr = Hashtbl.create 10 }));
      incr files
    end

let fopen ~path ~flags =
  match find_file path
  with File _ -> null_handle
     | Folder _ -> throw ErrCode.EISDIR

let opendir ~path =
  match find_file path
  with File _ -> throw ErrCode.ENOTDIR
     | Folder _ -> null_handle

let read ~path ~handle ~offset ~size =
  match find_file path with
  | File (_, { contents }) ->
     let length = Buffer.length contents in
     let offset = min offset length in
     let size = min size (length - offset) in
     Buffer.sub contents offset size
  | Folder _ -> throw ErrCode.EISDIR

let readdir ~path ~handle =
  match find_file path with
  | File _ -> throw ErrCode.ENOTDIR
  | Folder (_, folder) ->
     Array.of_list (Hashtbl.fold
		      (fun name _ accu -> name :: accu)
		      folder.dir
		      [ "." ; ".." ])

let rename ~from_path ~to_path =
  let from_filename, from_folder = find_basefolder from_path
  and to_filename, to_folder = find_basefolder to_path in
  let from_file = Hashtbl.find from_folder.dir from_filename in
  if Hashtbl.mem to_folder.dir to_filename then decr files;
  Hashtbl.remove from_folder.dir from_filename;
  Hashtbl.replace to_folder.dir to_filename from_file

let release ~path ~handle = ()

let releasedir ~path ~handle = ()

let rmdir ~path =
  match find_file path
  with
  | Folder (parent, folder) ->
     if Hashtbl.length folder.dir = 0 then
       let filename = List.hd (split_path path) in
       decr files;
       Hashtbl.remove parent.dir filename
     else throw ErrCode.ENOTEMPTY
  | _ -> throw ErrCode.ENOTDIR

let statfs () =
  let files = !files
  and file_size = !file_size in
  let bfree = statfs_base.blocks - (file_size / statfs_base.bsize) in
  { statfs_base with
    bfree ;
    bavail = bfree ;
    files ;
    ffree = statfs_base.ffree - files }

let sync ~path ~handle = ()

let syncdir ~path ~handle = ()

let truncate ~path ~size: new_size =
  match find_file path
  with Folder _ -> throw ErrCode.EISDIR
     | File (_, file) ->
	let old_size = Buffer.length file.contents in
	if old_size < new_size then
	  Buffer.add_string file.contents (String.make (new_size-old_size) '\000')
	else if old_size > new_size then
	  let contents = Buffer.contents file.contents in
	  Buffer.reset file.contents;
	  Buffer.add_substring file.contents contents 0 new_size

let ftruncate ~path ~handle ~size: new_size = truncate ~path ~size: new_size

let unlink ~path =
  match find_file path
  with
  | File (parent, file) ->
     let filename = List.hd (split_path path) in
     Hashtbl.remove parent.dir filename
  | _ -> throw ErrCode.ENOTDIR

let write ~path ~handle ~data ~offset =
  match (find_file path) with
  | File (_, file) ->
     file_size := !file_size + String.length data;
     if offset = Buffer.length file.contents then
       Buffer.add_string file.contents data
     else
       (let contents = Buffer.contents file.contents in
	Buffer.reset file.contents;
	Buffer.add_substring file.contents contents 0 offset;
	Buffer.add_string file.contents data;
	Buffer.add_substring file.contents
			     contents
			     offset
			     (String.length contents - offset));
     String.length data
  | Folder _ -> throw ErrCode.EISDIR

let _ =
  start
    "/Users/rpavy/Documents/Sync/MemFs"
    [ `Debug ; `Foreground ] (*[ `SingleThreaded ; `Foreground ]*)
    { access ;
      create ;
      mknod ;
      destroy ;
      flush ;
      getattr ;
      fgetattr ;
      getxattr ;
      setxattr ;
      init ;
      mkdir ;
      fopen ;
      opendir ;
      read ;
      readdir ;
      rename ;
      release ;
      releasedir ;
      rmdir ;
      statfs ;
      sync ;
      syncdir ;
      truncate ;
      ftruncate ;
      unlink ;
      write }
