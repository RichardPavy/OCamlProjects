let args () =
  let target = ref ""
  and debug = ref false in
  Arg.parse
    ([ "-target", Arg.Set_string target, "<file.exe> file to build." ;
       "-debug", Arg.Set debug, "true|false whether to add flag noassert" ]
     |> Arg.align)
    (fun anon_arg -> raise (Arg.Bad anon_arg))
    "Builder.exe -target <target> [-debug]";
  File.parse !target, !debug

let main () =
  let target, debug = args () in
  if File.is_root target |> not
  then
    Log.block
      "Building target <%s>" (File.to_string target)
      (fun () ->
        Flag.add ~kind: OCamlRules.Object
                 (fun _ -> [ "-strict-formats" ;
                             "-strict-sequence" ;
                             "-unsafe" ]
                           |> Iterable.of_list)
        |> ignore;
        if not debug then
          [ OCamlRules.Executable ; OCamlRules.Object ]
          |> List.iter begin fun kind ->
                       Flag.add ~kind
                                (fun _ -> Iterable.singleton "-noassert")
                       |> ignore
                       end;
        OCamlMake.add_root_rule_generator OCamlRules.ocaml_rules_generator;
	OCamlMake.build target)

let () =
  assert (Cache.dclear_all_caches ());
  main ()
