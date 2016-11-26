let level = ref (-1)

let incr () = incr level

let decr () = decr level

let enable = ref true

let log_aux result format =
  if !enable then
    let indent = ref "" in
    for i = 0 to !level do
      indent := !indent ^ "|   "
    done;
    print_string !indent;
    Printf.kfprintf (fun _ -> print_newline (); result) stdout format
  else
    Printf.ikfprintf (fun _ -> result) stdout format

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

let disable cb =
  let previous = !enable in
  enable := false;
  try let result = cb () in
      enable := previous;
      result
  with e -> enable := previous;
            raise e
