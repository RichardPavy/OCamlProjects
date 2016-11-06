type flags = int
type t =
  | RDONLY        (* open for reading only *)
  | WRONLY        (* open for writing only *)
  | RDWR          (* open for reading and writing *)
  | NONBLOCK      (* do not block on open or for data to become available *)
  | APPEND        (* append on each write *)
  | CREAT         (* create file if it does not exist *)
  | TRUNC         (* truncate size to 0 *)
  | EXCL          (* error if O_CREAT and the file exists *)
  | SHLOCK        (* atomically obtain a shared lock *)
  | EXLOCK        (* atomically obtain an exclusive lock *)
  | NOFOLLOW      (* do not follow symlinks *)
  | SYMLINK       (* allow open of symlinks *)
  | EVTONLY       (* descriptor requested for event notifications only *)
  | CLOEXEC       (* mark as close-on-exec *)

external ocamlfuse_openflags_impl : unit -> flags array = "ocamlfuse_openflags_impl"
let openflags = ocamlfuse_openflags_impl ()

let to_code (flag : t) = openflags.(Obj.magic flag)
let rdonly   = to_code RDONLY
and wronly   = to_code WRONLY
and rdwr     = to_code RDWR
and nonblock = to_code NONBLOCK
and append   = to_code APPEND
and creat    = to_code CREAT
and trunc    = to_code TRUNC
and excl     = to_code EXCL
and shlock   = to_code SHLOCK
and exlock   = to_code EXLOCK
and nofollow = to_code NOFOLLOW
and symlink  = to_code SYMLINK
and evtonly  = to_code EVTONLY
and cloexec  = to_code CLOEXEC
let access_flags = rdonly lor wronly lor rdwr

let all_flags =
  [ RDONLY ;
    WRONLY ;
    RDWR ;
    NONBLOCK ;
    APPEND ;
    CREAT ;
    TRUNC ;
    EXCL ;
    SHLOCK ;
    EXLOCK ;
    NOFOLLOW ;
    SYMLINK ;
    EVTONLY ;
    CLOEXEC ]

let has flags flag =
  if flag = RDONLY
  then flags land access_flags = rdonly
  else let code = to_code flag in
       flags land code = code

let of_flags flags = List.filter (has flags) all_flags

let to_flags l =
  let rec aux accu = function
    | [] -> accu
    | flag :: q -> aux (accu lor to_code flag) q
  in aux 0 l
    
(* Test: ocamlopt -o OpenFlags OpenFlagsImpl.c OpenFlags.mli OpenFlags.ml ; ./OpenFlags

let _ =
  for i = 0 to Array.length openflags - 1 do
    let flag : t = Obj.magic i in
    let result = of_flags (to_flags [flag]) in
    if ([flag] <> result) && ([RDONLY; flag] <> result)
    then failwith (Printf.sprintf "Fail: flag=%i code=%i" i (to_code flag))
  done;
  let all =
    [  RDONLY ;
       NONBLOCK ;
       APPEND ;
       CREAT ;
       TRUNC ;
       EXCL ;
       SHLOCK ;
       EXLOCK ;
       NOFOLLOW ;
       SYMLINK ;
       EVTONLY ;
       CLOEXEC ;
    ] in
  assert (of_flags (to_flags all) = all)
*)
