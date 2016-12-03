val command_log : string Utils_Iterable.t
val enable_command_log : bool ref
val run_command : ('a, unit, string, string list) format4 -> 'a
val run_command_cached :
  cache_file: Utils_File.t ->
  timestamp: float ->
  command: ('a, unit, string, string list) format4 ->
  'a
