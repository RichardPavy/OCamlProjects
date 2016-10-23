let level = ref (-1)

let incr () = incr level

let decr () = decr level

let log_aux result format =
  let indent = ref "" in
  for i = 0 to !level do
    indent := !indent ^ "|   "
  done;
  print_string !indent;
  Printf.kfprintf (fun _ -> print_newline (); result) stdout format

let log format = log_aux () format
let dlog format = log_aux true format

let block format =
  log_aux
    begin fun f ->
	  incr ();
	  let result = f () in
	  decr ();
	  result
    end
    format
