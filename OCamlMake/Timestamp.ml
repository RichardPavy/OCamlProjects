module HashSet = Container_HashSet
open Utils

let { Cache.fn = get_files_with_base ;
      Cache.clear = clear_files_with_base } =
  Cache.make (fun _ -> HashSet.create ())

let register_file file =
  let h = file |> File.strip_ext |> get_files_with_base
  in if not (HashSet.mem h file) then HashSet.add h file

type file_kind =
  | Special
  | Folder
  | File
  | Null

let to_kind unix_kind =
  let open Unix in
  match unix_kind with
  | S_REG -> File
  | S_DIR -> Folder
  | _ -> Special

let get_info_nocache file =
  let info =
    let open Unix in
    try
      let filename = File.to_string file in
      assert (Log.dlog "Unix.stat <%s>" filename);
      let { st_ctime ; st_mtime ; st_kind } = Unix.stat filename in
      let kind = to_kind st_kind in
      let time = match kind with
	| Folder -> 1. (* We don't care about the modification time of a folder. *)
	| _ -> max st_ctime st_mtime
      in kind, time
    with Unix_error (ENOENT, _, _) -> Null, -1.
  in
  register_file file; info

let max_cache_size = 1000

let { Cache.fn = get_info ;
      Cache.clear = clear_info } =
  Cache.make ~max_size: max_cache_size
	     ~finalize: begin fun file _ ->
			      file
			      |> File.strip_ext
			      |> clear_files_with_base
			end
	     get_info_nocache

let kind file = fst (get_info file)
let get file = snd (get_info file)

let clear file =
  let base = File.strip_ext file in
  Iterable.iter clear_info
		(base
		 |> get_files_with_base
		 |> HashSet.to_iterable);
  clear_files_with_base base

let () =
  assert (Folder = ("/" |> File.parse |> kind));
  assert (0. < ("/" |> File.parse |> get));
  assert (File = ("/bin/ls" |> File.parse |> kind));
  assert (0. < ("/bin/ls" |> File.parse |> get));
  assert (Null = ("aldkcjas7fja" |> File.parse |> kind));
  assert (0. > ("aldkcjas7fja" |> File.parse |> get));
  assert begin
      let file = File.parse "/bin/ls" in
      file |> get |> ignore;
      [ file ] = (file
		  |> get_files_with_base
		  |> HashSet.to_iterable |> Iterable.to_list)
    end;
  assert begin
      let file = File.parse "/bin/ls" in
      file |> get |> ignore;
      assert begin
	  [ file ] = (file
		      |> get_files_with_base
		      |> HashSet.to_iterable |> Iterable.to_list)
	end;
      clear file;
      [] = (file
	    |> get_files_with_base
	    |> HashSet.to_iterable |> Iterable.to_list)
    end;
  assert begin
      let file = File.parse "/bin/ls" in
      file |> get |> ignore;
      let get_files_with_base () =
	file
	|> get_files_with_base
	|> HashSet.to_iterable |> Iterable.to_list
      in
      assert begin [ file ] = get_files_with_base () end;
      for i = 0 to max_cache_size - 1 do
	let fileX = File.parsef "/bin/ls-truc%i" i in
	assert begin [] <> get_files_with_base () end;
	fileX |> get |> ignore
      done;
      [] = get_files_with_base ()
    end;
  assert (Cache.dclear_all_caches ())
