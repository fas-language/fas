val parse : ?limits:Limits.t -> Source.t -> (Ast.program, Diag.t list) result

val parse_blanking :
  ?limits:Limits.t -> Source.t -> (Ast.program * (int * int) list, Diag.t list) result
