type ('a, 'b) cache_fn = private { fn : 'a -> 'b; clear : 'a -> unit }

val dclear_all_caches : unit -> bool
val make :
  ?max_size: int ->
  ?finalize: ('a -> 'b -> unit) ->
  ('a -> 'b) ->
  ('a, 'b) cache_fn
val fn :
  ?max_size: int ->
  ?finalize: ('a -> 'b -> unit) ->
  ('a -> 'b) ->
  'a -> 'b
