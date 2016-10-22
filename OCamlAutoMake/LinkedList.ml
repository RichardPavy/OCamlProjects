type 'a node = { mutable prev: 'a node ;
                 v: 'a ;
                 mutable next: 'a node }

 and 'a t = { mutable length: int ;
              mutable first: 'a node }

 and 'a handle = { list: 'a t ;
                   node: 'a node }

let create () = { length = 0 ; first = Obj.magic () }

let add list v =
  list.length <- 1 + list.length;
  if list.length = 1 then
    let first = { prev = Obj.magic () ;
                  v ;
                  next = Obj.magic () };
    in
    list.first <- first;
    first.prev <- first;
    first.next <- first;
    { list ; node = first }
  else
    (* first.prev <-> first <-> first.next *)
    let first = list.first in
    let prev = first.prev in
    let last = { prev ; v ; next = first } in
    prev.next <- last;
    first.prev <- last;
    { list ; node = last }

let get_value { node = { v } } = v
let length { length } = length

let remove { list ; node } =
  list.length <- list.length - 1;
  if list.length = 0 then
    (assert (node == list.first);
     list.first <- Obj.magic ())
  else
    let { prev ; next} = node in
    prev.next <- next;
    next.prev <- prev;
    if list.first == node then
      list.first <- next

let get_first l =
  assert (l.length > 0);
  l.first.v

let get_last l =
  assert (l.length > 0);
  l.first.prev.v

let remove_first l =
  assert (l.length > 0);
  remove { list = l ; node = l.first }

let remove_last l =
  assert (l.length > 0);
  remove { list = l ; node = l.first.prev }

let of_list l =
  let ll = create () in
  List.iter (fun e -> add ll e |> ignore) l;
  ll

let of_iterable it =
  let ll = create () in
  Iterable.iter (fun e -> add ll e |> ignore) it;
  ll

let rec to_forward_iterable_aux f node n =
  if n > 0
  then (f node.v; to_forward_iterable_aux f node.next (n-1))

let to_forward_iterable { first ; length } =
  if length = 0
  then Iterable.empty ()
  else (fun f -> to_forward_iterable_aux f first length)
       |> Iterable.make

let to_iterable = to_forward_iterable

let rec to_backward_iterable_aux f node n =
  if n > 0
  then (f node.v; to_backward_iterable_aux f node.prev (n-1))

let to_backward_iterable { first ; length } =
  if length = 0
  then Iterable.empty ()
  else (fun f -> to_backward_iterable_aux f first.prev length)
       |> Iterable.make

let rec to_forward_list_aux node n accu =
  if n = 0 then
    accu
  else
    to_forward_list_aux node.prev (n-1) (node.v :: accu)

let rec to_backward_list_aux node n accu =
  if n = 0 then
    accu
  else
    to_backward_list_aux node.next (n-1) (node.v :: accu)

let to_forward_list { first ; length } =
  if length = 0
  then (assert (first == Obj.magic ()); [])
  else to_forward_list_aux first.prev length []

let to_list = to_forward_list

let to_backward_list { first ; length } =
  if length = 0
  then (assert (first == Obj.magic ()); [])
  else to_backward_list_aux first length []

let () =
  assert (let l = of_list [ 1; 2; 3; 4; 5 ] in
          [ 1; 2; 3; 4; 5 ] = to_forward_list l);
  assert (let l = of_list [ 1; 2; 3; 4; 5 ] in
          [ 5; 4; 3; 2; 1 ] = to_backward_list l);

  assert (let l = of_list [ 1; 2; 3; 4; 5 ] in
          [ 1; 2; 3; 4; 5 ] = (l |> to_forward_iterable |> Iterable.to_list));
  assert (let l = of_list [ 1; 2; 3; 4; 5 ] in
          [ 5; 4; 3; 2; 1 ] = (l |> to_backward_iterable |> Iterable.to_list));

  assert (let l = of_list [ 1; 2; 3; 4; 5 ] in
          remove_first l;
          [ 2; 3; 4; 5 ] = to_forward_list l);
  assert (let l = of_list [ 1; 2; 3; 4; 5 ] in
          remove_last l;
          [ 1; 2; 3; 4 ] = to_forward_list l);

  assert (let l = create () in
          let h1 = add l 1 in
          let h2 = add l 2 in
          let h3 = add l 3 in
          let h4 = add l 4 in
          let h5 = add l 5 in
          assert ([ 1; 2; 3; 4; 5 ] = to_forward_list l);
          assert ([ 5; 4; 3; 2; 1 ] = to_backward_list l);
          remove h1;
          assert ([ 2; 3; 4; 5 ] = to_forward_list l);
          assert ([ 5; 4; 3; 2 ] = to_backward_list l);
          remove h3;
          assert ([ 2; 4; 5 ] = to_forward_list l);
          assert ([ 5; 4; 2 ] = to_backward_list l);
          remove h5;
          assert ([ 2; 4 ] = to_forward_list l);
          assert ([ 4; 2 ] = to_backward_list l);
          remove h4;
          assert ([ 2 ] = to_forward_list l);
          assert ([ 2 ] = to_backward_list l);
          remove h2;
          assert ([] = to_forward_list l);
          assert ([] = to_backward_list l);
          true)
