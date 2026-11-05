type failure = {
  argv : string array;
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

val run : string array -> (string * string, failure) result
val run_to_file : string array -> string -> (string, failure) result
val run_program : string -> string array -> (int, string) result
val signal_name : int -> string

val run_inherited :
  env:string array -> string -> string array -> (Unix.process_status, string) result
