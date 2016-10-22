(** Sync *

TODO:
!Static file handler: cleanup
!Str is not thread-safe

!File explorer starts at a configurable root folder
!Javascript support: nodes can have a special attribute onclick --> trigger an action
   An action contains a url. This URL cannot be changed by the user.
   An action is implemented by an xml HTTP request that returns
   - an operation (add, replace, remove, nothing)
   - a destination
   - an html tag
!File preview in right-hand tab
!File editor in the file explorer
!File explorer
!Support for CSS and Javascript

- handle 100-continue
- move all the http files to their own folder
- test http protocol
- jobs
- web shell
- file sync
- nanny
- command line
- http 1.1 protocol
- forward async A -> B -> C -> A --> jobs?
- bit array/data protocol

- persistent storage
- auto reconnect peers when connection failed
- connection pools?
- event api: brodcast messages
- retry logic?
- quotas for RPC: must request quota to do RPC
- fuse
- ...
*)

let () =
  CommandLine.verify ();
  DynamicHtmlPages.register ();
  HttpFileHandler.register ();
  Server.start () |> ignore (* Server.start would return the server root. *);
  Lwt.wait () |> fst |> Lwt_main.run
