(** Sync *)

let () =
  CommandLine.verify ();
  DynamicHtmlPages.register ();
  HttpFileHandler.register ();
  Server.start () |> ignore (* Server.start would return the server root. *);
  Lwt.wait () |> fst |> Lwt_main.run
