open Lwt.Infix

let async format =
  Printf.ksprintf
    begin fun name callback ->
    Lwt.async
      begin fun () ->
      Lwt.catch
	callback
	begin fun exn ->
	Lwt_io.eprintlf "Failure in %s: %s"
			name (Printexc.to_string exn)
	end
      end
    end
    format

let () =
  assert begin
      async "AsyncUtils.ml test: %s" "string" (fun () -> Lwt.fail Exit);
      true
    end

let fail format =
  Printf.ksprintf
    begin fun message ->
    Lwt_io.eprintl message >>= fun () -> Lwt.fail_with message
    end
    format

let () =
  assert begin
      Lwt_main.run begin
          Lwt.catch
            (fun () -> fail "AsyncUtils.ml test: Hello %s %i" "world" 10
                       >|= fun () -> false)
            (fun _ -> Lwt.return_true)
        end
    end

let time f =
  let start = Unix.gettimeofday () in
  f () >>= fun () -> Lwt.return (Unix.gettimeofday () -. start)
