type t

val create : file:string -> text:string -> t
val file : t -> string
val text : t -> string
val length : t -> int
val span : t -> start_offset:int -> end_offset:int -> Span.t
val line_text : t -> int -> string option
val excerpt : t -> Span.t -> string
