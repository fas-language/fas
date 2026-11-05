val lex : ?limits:Limits.t -> Source.t -> (Token.t list, Diag.t list) result
val is_keyword : string -> bool
