type t = {
  file : string;
  start_offset : int;
  end_offset : int;
  line : int;
  column : int;
}

val make :
  file:string -> start_offset:int -> end_offset:int -> line:int -> column:int -> t

val synthetic : t
val is_synthetic : t -> bool
val compare : t -> t -> int
val to_string : t -> string
