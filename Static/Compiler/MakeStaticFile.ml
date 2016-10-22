let get_extension file =
  let l = String.length file
  and pos = 1 + String.rindex file '.' in
  String.sub file pos (l-pos)

let source_file, ml_file =
  let source_file = ref ""
  and ml_file = ref "" in
  Arg.parse [ "-source-file" ,
	      Arg.Set_string source_file,
	      "source Name of the file to compile as a static resource." ;
	      "-ml-file" ,
	      Arg.Set_string ml_file,
	      "ml Name of the produced OCaml file."]
	    (fun _ -> failwith "Unrecognized arg")
	    "";
  if get_extension !ml_file <> "ml" then
    "Destination is not an OCaml source file: " ^ !ml_file |> failwith;
  !source_file, !ml_file

let () =
  let open Unix in
  let source =
    let size = (stat source_file).st_size in
    let buffer = Bytes.create size in
    begin (* read *)
      let fd = openfile source_file [ O_RDONLY ] 0 in
      let rec aux pos =
	if pos <> size then
	  let pos' = pos + read fd buffer pos (size - pos)
	  in aux pos'
	else pos
      in
      try if size <> aux 0
	  then failwith "Unable to read all content.";
	  close fd
      with e -> close fd; raise e
    end;
    Printf.sprintf {|
		    open File
		    let file = { name = %S ; content = %S }
		    let () = Hashtbl.add files file.name file
		    |}
		   source_file (Bytes.to_string buffer)
  in
  begin (* write *)
    let fd = openfile ml_file [ O_WRONLY ; O_CREAT ; O_TRUNC ] 0o644 in
    try if String.length source <> write fd (Bytes.of_string source) 0 (String.length source)
	then failwith "Unable to write all content.";
	close fd
    with e -> close fd; raise e
  end

