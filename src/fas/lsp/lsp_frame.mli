type step = Message of string * string | Incomplete | Malformed of string * string

val take_message : string -> step
val resynchronise : string -> bool * string
val write_message : out_channel -> string -> unit
