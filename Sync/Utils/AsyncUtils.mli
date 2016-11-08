val async : ('a, unit, string, (unit -> unit Lwt.t) -> unit) format4 -> 'a
val fail : ('a, unit, string, 'b Lwt.t) format4 -> 'a
val time : (unit -> unit Lwt.t) -> float Lwt.t
