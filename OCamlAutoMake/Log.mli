val incr : unit -> unit
val decr : unit -> unit
val log : ('a, out_channel, unit, unit) format4 -> 'a
val dlog : ('a, out_channel, unit, bool) format4 -> 'a
val block : ('a, out_channel, unit, (unit -> 'b) -> 'b) format4 -> 'a
