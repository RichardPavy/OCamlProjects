type ('k, 'v) t = {
    max_size: int ;
    hash_map: ('k, ('k * 'v) LinkedList.handle ref) Hashtbl.t ;
    linked_list: ('k * 'v) LinkedList.t ;
    finalize: 'k -> 'v -> unit
  }

let create ?(finalize = fun _ _ -> ()) max_size =
  { max_size ;
    hash_map = Hashtbl.create max_size ;
    linked_list = LinkedList.create () ;
    finalize }

let clear { hash_map ; linked_list } =
  Hashtbl.iter
    (fun _ handle_ref -> LinkedList.remove !handle_ref)
    hash_map;
  Hashtbl.clear hash_map;
  assert (0 = LinkedList.length linked_list)

let bump linked_list handle_ref =
  let handle = !handle_ref in
  let kv = LinkedList.get_value handle in
  LinkedList.remove handle;
  handle_ref := LinkedList.add linked_list kv

let get { hash_map ; linked_list } key =
  let handle_ref = Hashtbl.find hash_map key in
  bump linked_list handle_ref;
  snd (LinkedList.get_value !handle_ref)

let remove { hash_map ; linked_list } key =
  let handle_ref = Hashtbl.find hash_map key in
  Hashtbl.remove hash_map key;
  LinkedList.remove !handle_ref

let evict { hash_map ; linked_list ; finalize } =
  let (k, v) = LinkedList.get_first linked_list in
  Hashtbl.remove hash_map k;
  LinkedList.remove_first linked_list;
  finalize k v

let insert_aux lru_map key value =
  if Hashtbl.length lru_map.hash_map = lru_map.max_size
  then evict lru_map;
  assert (Hashtbl.mem lru_map.hash_map key |> not);
  Hashtbl.add
    lru_map.hash_map
    key
    (ref (LinkedList.add lru_map.linked_list (key, value)))

let put ({ hash_map ; linked_list ; max_size } as lru_map) key value =
  begin
    match Hashtbl.find hash_map key with
    | handle_ref ->
       LinkedList.remove !handle_ref;
       handle_ref := LinkedList.add linked_list (key, value)
    | exception Not_found -> insert_aux lru_map key value
  end;
  assert (Hashtbl.length hash_map = LinkedList.length linked_list);
  assert (Hashtbl.length hash_map <= max_size)

let to_iterable { linked_list } = LinkedList.to_iterable linked_list

let () =
  assert begin
	     let expected =
	       let lru_map = create 3 in
	       put lru_map "key1" "first";
	       put lru_map "key2" "second";
	       put lru_map "key3" "third";
	       lru_map
	       |> to_iterable |> Utils_Iterable.to_list
	     and actual = [ ("key1", "first") ;
			    ("key2", "second") ;
			    ("key3", "third") ]
	     in expected = actual
    end;
  assert begin
      let expected =
	let lru_map = create 3 in
	put lru_map "key1" "first";
	put lru_map "key2" "second";
	put lru_map "key3" "third";
	assert (get lru_map "key2" = "second");
	lru_map
	|> to_iterable |> Utils_Iterable.to_list
      and actual = [ ("key1", "first") ;
		     ("key3", "third") ;
		     ("key2", "second") ]
      in expected = actual
    end;
  assert begin
      let expected =
	let lru_map = create 3 in
	put lru_map "key1" "first";
	put lru_map "key2" "second";
	put lru_map "key3" "third";
	assert (get lru_map "key1" = "first");
	put lru_map "key4" "fourth";
	lru_map
	|> to_iterable |> Utils_Iterable.to_list
      and actual = [ ("key3", "third") ;
		     ("key1", "first") ;
		     ("key4", "fourth") ]
      in expected = actual
    end
