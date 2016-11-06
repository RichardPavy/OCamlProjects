let get_handler_name target =
  let length = String.length target in
  let rec find_aux i =
    if i = length || target.[i] = '/'
    then i
    else find_aux (i + 1)
  in
  if target = ""
  then ""
  else
    let start = if target.[0] = '/' then 1 else 0 in
    let l = find_aux start in
    String.sub target start (l - start)

let handlers = Hashtbl.create 16

let register_handler name handler =
  if Hashtbl.mem handlers name
  then failwith ("Handler already defined: " ^ name);
  Hashtbl.add handlers name handler

let () =
  let open HttpProtocol in
  set_handler
    begin fun request ->
	  let handler_name = get_handler_name request.start_line.request_target in
	  let handler = try Hashtbl.find handlers handler_name
			with Not_found -> failwith ("Undefined handler: " ^ handler_name)
	  in handler request
    end

let () =
  assert ("target" = get_handler_name "/target/other");
  assert ("target" = get_handler_name "target/other");
  assert ("target" = get_handler_name "/target/");
  assert ("target" = get_handler_name "/target");
  assert ("target" = get_handler_name "target/");
  assert ("target" = get_handler_name "target");
  assert ("" = get_handler_name "/");
  assert ("" = get_handler_name "//");
  assert ("" = get_handler_name "")

module ContentTypes =
  struct
    let plain_text = "text/plain; charset=utf-8"
    let html = "text/html; charset=utf-8"
  end
