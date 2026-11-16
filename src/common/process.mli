type failure = {
  argv : string array;
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

val run : string array -> (string * string, failure) result
(* [run_to_file ?max_bytes argv path] runs [argv] with stdout in [path] and
   returns its stderr. Past [max_bytes] of stdout the program is killed, the
   file emptied, and the failure is [output_limit_exceeded] *)
val run_to_file : ?max_bytes:int -> string array -> string -> (string, failure) result

val output_limit_exceeded : failure -> bool
val run_program : string -> string array -> (int, string) result
val signal_name : int -> string

val run_inherited :
  env:string array -> string -> string array -> (Unix.process_status, string) result
