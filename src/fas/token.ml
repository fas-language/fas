type kind =
  | Ident of string
  | Int of string
  | Float of string
  | String of string
  | CString of string
  | Kw_fn
  | Kw_return
  | Kw_if
  | Kw_else
  | Kw_while
  | Kw_break
  | Kw_continue
  | Kw_const
  | Kw_var
  | Kw_struct
  | Kw_opaque
  | Kw_extern
  | Kw_defer
  | Kw_use
  | Kw_for
  | Kw_switch
  | Kw_case
  | Kw_default
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Lbracket
  | Rbracket
  | Comma
  | Dot
  | Ellipsis
  | Dot_dot
  | Semi
  | At
  | Colon
  | Assign
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Plus_eq
  | Minus_eq
  | Star_eq
  | Slash_eq
  | Percent_eq
  | Amp_eq
  | Pipe_eq
  | Caret_eq
  | Ltlt_eq
  | Gtgt_eq
  | Amp
  | Pipe
  | Caret
  | Tilde
  | Eqeq
  | Neq
  | Lt
  | Le
  | Ltlt
  | Gt
  | Ge
  | Gtgt
  | Andand
  | Oror
  | Not
  | Newline
  | Eof

type t = { kind : kind; span : Span.t }

let show = function
  | Ident s -> "`" ^ s ^ "`"
  | Int s | Float s -> "`" ^ s ^ "`"
  | String s -> "\"" ^ s ^ "\""
  | CString s -> "c\"" ^ s ^ "\""
  | Kw_fn -> "`fn`"
  | Kw_return -> "`return`"
  | Kw_if -> "`if`"
  | Kw_else -> "`else`"
  | Kw_while -> "`while`"
  | Kw_break -> "`break`"
  | Kw_continue -> "`continue`"
  | Kw_const -> "`const`"
  | Kw_var -> "`var`"
  | Kw_struct -> "`struct`"
  | Kw_opaque -> "`opaque`"
  | Kw_extern -> "`extern`"
  | Kw_defer -> "`defer`"
  | Kw_use -> "`use`"
  | Kw_for -> "`for`"
  | Kw_switch -> "`switch`"
  | Kw_case -> "`case`"
  | Kw_default -> "`default`"
  | Lparen -> "`(`"
  | Rparen -> "`)`"
  | Lbrace -> "`{`"
  | Rbrace -> "`}`"
  | Lbracket -> "`[`"
  | Rbracket -> "`]`"
  | Comma -> "`,`"
  | Dot -> "`.`"
  | Ellipsis -> "`...`"
  | Dot_dot -> "`..`"
  | Semi -> "`;`"
  | At -> "`@`"
  | Colon -> "`:`"
  | Assign -> "`=`"
  | Plus -> "`+`"
  | Minus -> "`-`"
  | Star -> "`*`"
  | Slash -> "`/`"
  | Percent -> "`%`"
  | Plus_eq -> "`+=`"
  | Minus_eq -> "`-=`"
  | Star_eq -> "`*=`"
  | Slash_eq -> "`/=`"
  | Percent_eq -> "`%=`"
  | Amp_eq -> "`&=`"
  | Pipe_eq -> "`|=`"
  | Caret_eq -> "`^=`"
  | Ltlt_eq -> "`<<=`"
  | Gtgt_eq -> "`>>=`"
  | Amp -> "`&`"
  | Pipe -> "`|`"
  | Caret -> "`^`"
  | Tilde -> "`~`"
  | Eqeq -> "`==`"
  | Neq -> "`!=`"
  | Lt -> "`<`"
  | Le -> "`<=`"
  | Ltlt -> "`<<`"
  | Gt -> "`>`"
  | Ge -> "`>=`"
  | Gtgt -> "`>>`"
  | Andand -> "`&&`"
  | Oror -> "`||`"
  | Not -> "`!`"
  | Newline -> "newline"
  | Eof -> "end of file"

let c_header_component = function
  | Ident text | Int text | Float text -> Some text
  | Kw_fn -> Some "fn"
  | Kw_return -> Some "return"
  | Kw_if -> Some "if"
  | Kw_else -> Some "else"
  | Kw_while -> Some "while"
  | Kw_break -> Some "break"
  | Kw_continue -> Some "continue"
  | Kw_const -> Some "const"
  | Kw_var -> Some "var"
  | Kw_struct -> Some "struct"
  | Kw_opaque -> Some "opaque"
  | Kw_extern -> Some "extern"
  | Kw_defer -> Some "defer"
  | Kw_use -> Some "use"
  | Kw_for -> Some "for"
  | Kw_switch -> Some "switch"
  | Kw_case -> Some "case"
  | Kw_default -> Some "default"
  | _ -> None
