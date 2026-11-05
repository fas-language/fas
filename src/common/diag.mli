type severity = Error | Warning
type issue = General | Not_constant

type t = {
  severity : severity;
  issue : issue;
  primary : Span.t;
  message : string;
  notes : string list;
  help : string option;
}

val error : ?issue:issue -> ?notes:string list -> ?help:string -> Span.t -> string -> t
val warning : ?notes:string list -> ?help:string -> Span.t -> string -> t
val render : source:Source.t option -> t -> string
val render_all : source:Source.t option -> t list -> string
val render_json : t list -> string
val json_string : string -> string
val local_declaration_message : ?variable:string -> string -> string option
val is_local_declaration_message : string -> bool

val declaration_type_message :
  declaration:string -> variable:string -> ?length:string -> string -> string option

val local_type_error :
  ?span:Span.t ->
  ?ty_span:Span.t ->
  string ->
  ('a, t list) result ->
  ('a, t list) result
