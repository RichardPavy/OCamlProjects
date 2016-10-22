open Lwt.Infix

type executor = (unit -> unit Lwt.t) -> unit
type job = executor -> unit Lwt.t

let run job =
  let count = ref 0
  and waiter, wakener = Lwt.wait () in
  let decr () =
    decr count;
    if !count = 0
    then Lwt.wakeup wakener ();
    Lwt.return_unit
  in
  let executor thread =
    if !count >= 0
    then begin  
	incr count;
	Lwt.async begin fun () ->
			Lwt.catch
			  (fun () -> thread () >>= decr)
			  begin fun exn ->
				count := -1;
				Lwt.wakeup_exn wakener exn;
				Lwt.return_unit
			  end

		  end
      end
  in
  executor (fun () -> job executor);
  waiter
