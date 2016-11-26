open Utils

let { Cache.fn = list ;
      Cache.clear = clear_cache } =
  Cache.make
    ~max_size: 10
    begin fun folder ->
    match Timestamp.kind folder with
    | Timestamp.Null when not (File.is_root folder) -> [||]
    | Timestamp.Null | Timestamp.Folder ->
       folder
       |> File.to_string
       |> Process.run_command "ls %s"
       |> Array.of_list
    | _ -> Utils.fail "Not a folder: %s" (File.to_string folder) |> raise
    end

let () =
  assert (Log.dlog "Testing FolderContent");
  assert begin
      [ "test file exists",
        (fun () -> "/" |> File.parse |> list |> Array.mem "var") ;

        "test file does not exist",
        (fun () -> "/" |> File.parse |> list |> Array.mem "ssqdfqsdfqsdf"
                   |> not) ;
      ] |> Asserts.test
    end
