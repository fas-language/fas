type t

val index : string -> t
val position : t -> int -> int * int
val offset : t -> int * int -> int
val position_of_offset : string -> int -> int * int
val offset_of_position : string -> int * int -> int
