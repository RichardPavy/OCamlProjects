val extension : File.t -> string -> bool
val test : (string * (unit -> bool)) list -> bool
val equals :
  ?eq: ('a -> 'a -> bool) ->
  ('a -> unit, out_channel, unit) format ->
  'a -> 'a -> bool
