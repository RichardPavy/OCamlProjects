type 'a result = Value of 'a | Exn of exn
type ('a, 'b) cache_fn = { fn: 'a -> 'b; clear: 'a -> unit }

let clear_all_caches_callbacks = ref []

let dclear_all_caches () =
  assert begin
      List.iter (fun callback -> callback ())
		!clear_all_caches_callbacks;
      true
    end;
  true

let make ?max_size ?finalize f =
  let return = function Value e -> e | Exn exn -> raise exn in
  let get, put, remove, clear =
    match max_size with
    | Some max_size ->
       let finalize = match finalize with
	 | Some f -> Some begin fun k v -> match v with
					   | Value v -> f k v
					   | Exn _ -> ()
			  end
	 | None -> None
       in
       let cache = LruMap.create ?finalize max_size in
       (LruMap.get cache),
       (LruMap.put cache),
       (LruMap.remove cache),
       (fun () -> LruMap.clear cache)
    | None ->
       assert (Utils.dcheck (finalize = None)
			    "Can't specify a finalizer without max_size.");
       let cache = Hashtbl.create 10 in
       (Hashtbl.find cache),
       (Hashtbl.add cache),
       (Hashtbl.remove cache),
       (fun () -> Hashtbl.clear cache)
  in
  assert (clear_all_caches_callbacks := clear :: !clear_all_caches_callbacks; true);
  let fn arg =
    begin match get arg with
    | result -> return result
    | exception Not_found ->
       let result = try Value (f arg)
		    with exn -> Exn exn
       in
       put arg result;
       return result
    end
  in { fn ; clear = remove }

let fn ?max_size ?finalize f = (make ?max_size ?finalize f).fn

let () =
  assert begin
      let finalized = ref [] in
      let f = fn ~max_size:3 ~finalize: (fun k v -> finalized := (k,v) :: !finalized)
		 string_of_int in
      f 1 |> ignore;
      f 2 |> ignore;
      f 3 |> ignore;
      assert (!finalized = []);
      f 1 |> ignore;
      assert (!finalized = []);
      f 4 |> ignore;
      !finalized = [2, "2"]
    end;
  assert begin
      try ignore (make ~finalize: (fun k v -> ()) ignore);
	  false
      with Failure _ -> true
    end;
  assert begin
      let c = ref 0 in
      let f () = incr c; if !c > 0 then failwith "Some exception" in
      let f_cached = fn f in
      assert (try f_cached (); false with Failure _ -> true);
      assert (try f_cached (); false with Failure _ -> true);
      assert (try f_cached (); false with Failure _ -> true);
      !c = 1
    end
