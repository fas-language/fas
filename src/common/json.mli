type t =
  | Null
  | Bool of bool
  | Num of string
  | Str of string
  | Arr of t list
  | Obj of (string * t) list

val parse : string -> (t, string) result
val field : string -> t -> t
val quote : string -> string
val to_string : t -> string
