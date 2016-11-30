open OCamlMake_Common
open Utils

module It = Iterable

(**
 * A rule that does nothing:
 * - Source: empty
 * - Target: [file] *)
let noop_rule file =
  let targets = It.singleton file
  and sources = It.empty ()
  and command () = Utils.fail "Noop rules should never be executed." |> raise
  in
  let open OCamlMake in
  { targets ; sources ; command }

(**
 * A rule that creates a folder:
 * - Source: parent folder if not root, else empty
 * - Target: [folder] *)
let folder_rule folder =
  let targets = It.singleton folder
  and sources = if File.is_toplevel folder
		then It.empty ()
		else It.singleton (File.parent folder)
  and command () =
    let kind = Timestamp.kind folder in
    assert begin
	Utils.dcheck (match kind with
		      | Timestamp.Folder | Timestamp.Null -> true
		      | _ -> false)
		     "Unexpected kind for %s." (File.to_string folder)
      end;
    if kind = Timestamp.Null
    then Process.run_command "mkdir %s"
		     	     (File.to_string folder)
         |> ignore
  in
  let open OCamlMake in
  { targets ; sources ; command }

(**
 * A rule to copy a file to the build/ folder.
 * - Source: some/file.txt
 * - Target: build/some/file.txt *)
let private_file_rule target =
  assert (Utils.dcheck (Private.is_private target)
		       "Target %s is not in the private folder (%s/...)"
		       (File.to_string target) Private.private_folder);
  let source = Private.to_public target in
  let targets = It.singleton target
  and sources = [ File.parent target ; source ] |> It.of_list
  and command () = Process.run_command
		     "cp %s %s"
		     (File.to_string source)
		     (File.to_string target)
		   |> ignore
  in
  let open OCamlMake in
  { targets ; sources ; command }

(**
 * A rule to copy a file from the build/ folder.
 * - Source: build/some/file.txt
 * - Target: some/file.txt *)
let public_file_rule target =
  assert (Utils.dcheck (Private.is_public target)
		       "Target %s is in the private folder (%s/...)"
		       (File.to_string target) Private.private_folder);
  let source = Private.to_private target in
  let targets = It.singleton target
  and sources = if File.is_toplevel target
                then It.singleton source
                else [| File.parent target ; source |] |> It.of_array
  and command () = Process.run_command
		     "cp %s %s"
		     (File.to_string source)
		     (File.to_string target)
		   |> ignore
  in
  let open OCamlMake in
  { targets ; sources ; command }

let private_folder_rules_generator ~folder =
  if Private.is_private folder then
    OCamlMake.rule_generator_result ~rules: (It.empty ()) ()
  else
    begin fun () ->
    assert (Utils.dcheck (Private.is_public folder)
		         "Folder %s is a private folder (%s/...)"
		         (File.to_string folder) Private.private_folder);
    let folder_string = if File.is_root folder
		        then ""
		        else (File.to_string folder) ^ "/" in
    FolderContent.list folder
    |> It.of_array
    |> (if File.is_root folder
        then It.filter (fun file -> file <> Private.private_folder)
        else fun it -> it)
    |> It.map (fun file -> File.parsef "%s%s" folder_string file)
    |> It.filter (fun file -> Timestamp.kind file = Timestamp.Folder)
    |> It.map (fun file -> noop_rule file)
    |> fun rules -> OCamlMake.rule_generator_result ~rules ()
    end |> Log.block "public folder rules generator for <%s>"
                     (File.to_string folder)

let build_folder_rules_generator ~folder =
  let rules =
    if File.is_root folder then
      folder_rule (File.parse Private.private_folder)
      |> It.singleton
    else
      It.empty ()
  in
  OCamlMake.rule_generator_result ~rules ()
