(** Verifies that a file has the right extension. *)
let extension target extension =
  Utils.dcheck (target |> File.extension = extension)
	       "Unexpected *.%s target file: %s"
	       extension (File.to_string target)

let test test_cases =
  test_cases
  |> List.fold_left
       begin fun success (name, test) ->
       if Log.disable test
       then success
       else (Log.log "Test failed: %s" name; false)
       end
       true

let equals ?(eq = (=)) format a b =
  if eq a b then
    true
  else
    (prerr_string "\tNot true that\n\t\t<";
     Printf.eprintf format a;
     prerr_string ">\n\tis equal to\n\t\t<";
     Printf.eprintf format b;
     prerr_string ">\n";
     false)

let () =
  assert (Log.dlog "Testing Asserts");
  assert
    ([ "assert extension success",
       (fun () -> extension ("File.txt" |> Utils_File.parse) "txt") ;

       "assert extension failure",
       (fun () -> try extension ("File.txt" |> Utils_File.parse) "exe" |> ignore;
                      assert false;
                  with Failure _ -> true) ;
     ] |> test)
