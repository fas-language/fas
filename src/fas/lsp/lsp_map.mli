type t

val create : analyzed:string -> current:string -> t
val to_current : t -> int -> int option
val to_analyzed : t -> int -> int option
