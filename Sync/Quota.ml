open Lwt.Infix

type quota = {
    init_quantity : int ;
    mutable quantity : int ;
    blocked : (int * (unit -> unit Lwt.t)) Queue.t ;
  }

let create init_quantity = { init_quantity ;
			     quantity = init_quantity ;
			     blocked = Queue.create () }

let rec run_unchecked quota quantity f =
  quota.quantity <- quota.quantity - quantity;
  f () >>= fun () -> release_quota quota quantity

and run_queue quota =
  if Queue.is_empty quota.blocked
  then Lwt.return_unit
  else
    let quantity, f = Queue.peek quota.blocked in
    if quota.quantity < quantity
    then Lwt.return_unit
    else begin
	ignore (Queue.pop quota.blocked);
	let t = run_unchecked quota quantity f
	and q = run_queue quota in
	t >>= fun () -> q
      end

and release_quota quota quantity =
  quota.quantity <- quota.quantity + quantity;
  run_queue quota

let split f =
  let t, u = Lwt.wait () in
  (fun () -> Lwt.catch
	       (fun () -> f () >|= fun r -> Lwt.wakeup u r)
	       (fun exn -> Lwt.wakeup_exn u exn; Lwt.return_unit)),
  t

let with_quota quota quantity f =
  let quantity = min quantity quota.init_quantity in
  let f', t = split f in
  if quota.quantity < quantity
  then (Queue.add (quantity, f') quota.blocked; t)
  else run_unchecked quota quantity f' >>= fun () -> t

(*****************)
(***** Tests *****)
(*****************)

exception TestQuotaFailure
let test_quotas () =
  let q = create 1000
  and count = ref 0
  and active = ref 0
  and max_active = ref (-1) in
  (* wait () simulates some long running process that runs under quota. *)
  let wait () =
    incr count;
    if !count mod 3 = 0
    then raise Exit
    else begin
	incr active;
	max_active := max !active !max_active;
	Lwt_unix.sleep 0.1 >|= fun () -> decr active
      end
  in
  (* [wait_p n] runs n [wait()] threads in parallel. *)
  let rec wait_p n =
    if n == 0
    then Lwt.return_unit
    else
      let wait () = with_quota q 50 wait
      and next = wait_p (n - 1) in
      Lwt.finalize wait (fun () -> next)
  in Lwt.catch
       (fun () -> wait_p 75)
       (function Exit -> Lwt.return_unit | exn -> Lwt.fail exn)
     >>= fun () -> if !count <> 75 || !active <> 0 || !max_active <> 20
		   then
		     Lwt_io.eprintlf "count=%d, active=%d, max_active=%d"
				     !count !active !max_active
		     >>= fun () -> Lwt.fail_with "Unexpected results in run_quota_tests"
		   else Lwt.return_unit

let () = Lwt_main.run (test_quotas ())
