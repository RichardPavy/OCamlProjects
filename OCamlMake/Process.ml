module File = Utils_File
module Log = Utils_Log
module Utils = Utils_Utils

let run_command_aux format cb =
  Printf.ksprintf
    begin fun command ->
    Log.log "$ %s" (String.escaped command);
    let ch = Unix.open_process_in command in
    let close () =
      if Unix.close_process_in ch <> Unix.WEXITED 0 then
	Utils.fail "Command failed: %s" command |> raise
    in
    let rec read_lines accu =
      begin match input_line ch
      with line -> read_lines (line :: accu)
	 | exception End_of_file ->
	    close ();
	    List.rev accu
      end
    in
    try read_lines [] |> cb command
    with Unix.Unix_error (_, _, _) ->
      close ();
      Utils.fail "Unable to run %s" (String.escaped command) |> raise
    end
    format

let run_command format  = run_command_aux format (fun _ x -> x)

let () =
  assert (["line 1"; "line 2"] = run_command "echo 'line 1\nline 2'")

let write_lines file lines =
  let ch = file |> File.to_string |> open_out in
  try
    List.iter
      begin fun line -> output_string ch line;
			output_char ch '\n'
      end
      lines;
    close_out ch
  with e -> close_out ch; raise e

let read_lines file =
  let file = File.to_string file in
  let ch = try open_in file
	   with Sys_error _ -> Utils.fail "Cannot open file %s" file |> raise
  in
  let rec aux accu =
    begin
      match input_line ch
      with line -> aux (line :: accu)
	 | exception End_of_file -> close_in ch; List.rev accu
    end
  in
  try aux []
  with _ -> close_in ch;
	    Utils.fail "Unable to read %s" file |> raise

(**
 * Same result as run_command.
 *
 * if the timestamp from [cache_file] < [timestamp]
 * then run the command again and store the result in cache_file
 * else read the cached result from cache_file. *)
let run_command_cached ~cache_file ~timestamp ~command: format =
  let cache_file_timestamp = Timestamp.get cache_file in
  if cache_file_timestamp <= timestamp
  then run_command_aux format
		       begin fun command result ->
                       assert (Utils.dcheck (timestamp > 0.)
                                            "Source file does not exist for <%s>."
                                            command);
                       Timestamp.clear cache_file;
		       write_lines cache_file result;
		       result
		       end
  else Printf.ksprintf
         (fun command ->
           assert (Utils.dcheck (timestamp > 0.)
                                "Source file does not exist for <%s>."
                                command);
           read_lines cache_file)
         format
