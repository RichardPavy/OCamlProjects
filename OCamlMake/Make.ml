open Utils

module Flag = OCamlMake_Common_Flag
module Property = OCamlMake_Common_Property

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

  OCamlMake.add_root_rule_generator
    OCamlMake_OCaml_OCamlRules.ocaml_rules_generator;

  OCamlMake.add_root_rule_generator
    BootstrapRule.bootstrap_rules_generator;

  OCamlMake.add_root_rule_generator
    CommonRules.public_folder_rules_generator;

  if File.is_root target |> not then
    begin fun () ->
    let strict_flags =
      Flag.add ~kind: OCamlMake_OCaml_Flags.Object
               (fun _ -> [ "-strict-formats" ;
                           "-strict-sequence" ;
                           "-unsafe" ;
                         ] |> Iterable.of_list)
      |> Iterable.singleton
    and debug_flags =
      if debug then
        Iterable.empty ()
      else
        let open OCamlMake_OCaml_Flags in
        [| Executable ; Object |]
        |> Iterable.of_array
        |> Iterable.map begin fun kind ->
                        Flag.add ~kind
                                 (fun _ -> Iterable.singleton "-noassert")
                        end
    in
    Iterable.concat strict_flags debug_flags
    |> Iterable.to_list
    |> Property.process
         begin fun () ->
         OCamlMake.build target
         end
    end |> Log.block "Building target <%s>" (File.to_string target)

let () =
  assert (Cache.dclear_all_caches ());
  main ()
