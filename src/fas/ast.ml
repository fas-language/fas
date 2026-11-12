type aggregate_length = { expression : expr; text : string; span : Span.t }

and ty =
  | Bool
  | Int of int_kind
  | Float of float_kind
  | Addr
  | Handle of ty
  | Array of aggregate_length * ty
  | Vec of aggregate_length * ty
  | Named_type of string * Span.t
  | Applied_type of string * Span.t * generic_arg list * Span.t
  | Void

and int_kind = U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 | Usize | Isize
and float_kind = F32 | F64

and generic_arg =
  | Type_arg of ty
  | Type_or_index of ty
  | Const_arg of expr
  | Name_arg of string * Span.t

and expr =
  | Int_lit of string * Span.t
  | Float_lit of string * Span.t
  | Bool_lit of bool * Span.t
  | Null of Span.t
  | String_lit of bool * string * Span.t
  | Ident of string * Span.t
  | Unary of unop * expr * Span.t
  | C_dereference of expr * string option * Span.t
  | C_dot_star of expr * Span.t
  | Parenthesized of expr * Span.t
  | Binary of binop * expr * expr * Span.t
  | Call of expr * expr list * Span.t
  | Generic_args of expr * generic_arg list * Span.t
  | Cast of cast_kind * ty * expr * Span.t
  | Select of expr * generic_arg list * Span.t
  | Field of expr * string * Span.t
  | Arrow_field of expr * string * Span.t * Span.t
  | Addr_of of expr * Span.t
  | Handle_from_addr of ty * expr * Span.t
  | Sizeof of ty * Span.t
  | Sizeof_value of expr * Span.t
  | Alignof of ty * Span.t
  | Offsetof of ty * string * Span.t * Span.t
  | Splat of expr * Span.t
  | Ternary of expr * expr * expr * Span.t
  | Array_lit of expr list * Span.t
  | Designated_field of string * Span.t * expr * Span.t
  | Designated_index of expr * expr * Span.t

and unop = Neg | Not | Bit_not

and binop =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Bit_and
  | Bit_or
  | Bit_xor
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  | Shl
  | Shr

and cast_kind = Zext | Sext | Trunc | Bitcast | Float_widen

and stmt =
  | Let of {
      name : string;
      ty : ty;
      ty_span : Span.t;
      init : expr option;
      span : Span.t;
    }
  | View of { name : string; name_span : Span.t; place : expr; span : Span.t }
  | Assign of assign_target * expr * Span.t
  | Compound_assign of assign_target * binop * expr * Span.t * Span.t
  | Return of expr option * Span.t
  | If of expr * stmt list * stmt list option * Span.t
  | While of loop_label option * expr * stmt list * Span.t
  | Break of (string * Span.t) option * Span.t
  | Continue of (string * Span.t) option * Span.t
  | Defer of stmt list * Span.t
  | Expr_stmt of expr * Span.t * Span.t
  | Block of stmt list * Span.t
  | For of
      loop_label option * stmt option * expr option * stmt option * stmt list * Span.t
  | Switch of expr * (expr list * stmt list) list * stmt list option * Span.t
  | Const_for of {
      name : string;
      name_span : Span.t;
      ty : ty;
      ty_span : Span.t;
      low : expr;
      high : expr;
      body : stmt list;
      span : Span.t;
    }
  | Const_copy of {
      name : string;
      ty : ty;
      value : int64;
      note : string;
      body : stmt list;
      span : Span.t;
    }

and loop_label = { name : string; span : Span.t }

and assign_target =
  | Target_ident of string * Span.t
  | Target_select of expr * generic_arg list
  | Target_field of expr * string * Span.t

and field = {
  name : string;
  ty : ty;
  ty_span : Span.t;
  span : Span.t;
  offset : int option;
  unsupported_reason : string option;
}

and param = { name : string; ty : ty; ty_span : Span.t; span : Span.t }
and const_param = { name : string; ty : ty; ty_span : Span.t; span : Span.t }

and generic_param =
  | Type_param of { name : string; span : Span.t }
  | Const_param of const_param

and body = Declaration | Statements of stmt list
and linkage = Internal | External_c
and global_linkage = Internal_global | Export_c | Import_c | Import_const_c
and c_fragment = { tag : string; text : string }
and c_header = C_quoted of string | C_system of string | C_fragment of c_fragment
and pkg = { modules : string; modules_span : Span.t }

and item =
  (* [os]: `use linux "f.fas"` — only part of the program when building for
     that OS (--os). None for an unconditional use. *)
  | Use of {
      path : string;
      c_header : c_header option;
      os : Target_layout.os option;
      span : Span.t;
    }
  | Pkg of { pkg : pkg; span : Span.t }
  | Const of {
      name : string;
      name_span : Span.t;
      ty_span : Span.t;
      ty : ty;
      value : expr;
      span : Span.t;
      extent : Span.t;
    }
  | Global of {
      name : string;
      name_span : Span.t;
      ty_span : Span.t;
      ty : ty;
      init : expr option;
      linkage : global_linkage;
      span : Span.t;
      extent : Span.t;
    }
  | Struct of {
      name : string;
      name_span : Span.t;
      generic_params : generic_param list;
      fields : field list;
      align : int option;
      size : int option;
      is_union : bool;
      span : Span.t;
      extent : Span.t;
    }
  | Opaque of { name : string; name_span : Span.t; span : Span.t; extent : Span.t }
  | Func of {
      name : string;
      name_span : Span.t;
      ret_span : Span.t;
      params : param list;
      ret : ty;
      body : body;
      linkage : linkage;
      variadic : bool;
      generic_params : generic_param list;
      span : Span.t;
      extent : Span.t;
      body_span : Span.t;
    }

type program = { items : item list }

let const_item name name_span ty value span =
  Const { name; name_span; ty_span = name_span; ty; value; span; extent = span }

let with_extent extent = function
  | Const c -> Const { c with extent }
  | Global g -> Global { g with extent }
  | Struct s -> Struct { s with extent }
  | Opaque o -> Opaque { o with extent }
  | Func f -> Func { f with extent }
  | (Use _ | Pkg _) as item -> item

let stored_expr_span = function
  | Int_lit (_, s)
  | Float_lit (_, s)
  | Bool_lit (_, s)
  | Null s
  | String_lit (_, _, s)
  | Ident (_, s)
  | Call (_, _, s)
  | Generic_args (_, _, s)
  | Cast (_, _, _, s)
  | Select (_, _, s)
  | Addr_of (_, s)
  | Handle_from_addr (_, _, s)
  | Sizeof (_, s)
  | Sizeof_value (_, s)
  | Alignof (_, s)
  | Offsetof (_, _, _, s)
  | Splat (_, s)
  | Ternary (_, _, _, s)
  | Array_lit (_, s)
  | Designated_field (_, _, _, s)
  | Designated_index (_, _, s) ->
      s
  | C_dereference (_, _, s) | C_dot_star (_, s) -> s
  | Unary _ | Binary _ | Parenthesized _ | Arrow_field _ | Field _ -> assert false

let rec expr_span = function
  | Binary (_, left, right, _) ->
      let left_span = expr_start_span left and right_span = expr_end_span right in
      Span.make ~file:left_span.Span.file ~start_offset:left_span.Span.start_offset
        ~end_offset:right_span.Span.end_offset ~line:left_span.Span.line
        ~column:left_span.Span.column
  | Unary (_, operand, operator_span) ->
      let end_span = expr_end_span operand in
      Span.make ~file:operator_span.Span.file
        ~start_offset:operator_span.Span.start_offset
        ~end_offset:end_span.Span.end_offset ~line:operator_span.Span.line
        ~column:operator_span.Span.column
  | Parenthesized (_, span) -> span
  | Arrow_field (base, _, _, field_span) | Field (base, _, field_span) ->
      let base_span = expr_start_span base in
      Span.make ~file:base_span.Span.file ~start_offset:base_span.Span.start_offset
        ~end_offset:field_span.Span.end_offset ~line:base_span.Span.line
        ~column:base_span.Span.column
  | expression -> stored_expr_span expression

and expr_start_span = function
  | Binary (_, left, _, _) -> expr_start_span left
  | Parenthesized (_, span) -> span
  | Arrow_field (base, _, _, _) | Field (base, _, _) -> expr_start_span base
  | expression -> expr_span expression

and expr_end_span = function
  | Binary (_, _, right, _) -> expr_end_span right
  | Parenthesized (_, span) -> span
  | Arrow_field (_, _, _, field_span) | Field (_, _, field_span) -> field_span
  | expression -> expr_span expression

let rec expr_leading_operator = function
  | Binary (_, left, _, _) | Ternary (left, _, _, _) -> expr_leading_operator left
  | Unary (Neg, _, _) -> Some "-"
  | Addr_of _ -> Some "&"
  | _ -> None

let rec index_expression = function
  | Named_type (name, span) -> Ident (name, span)
  | Applied_type (name, _, arguments, span) ->
      Select (Ident (name, span), List.map index_argument arguments, span)
  | _ -> failwith "internal error: ambiguous index requires a named type application"

and index_argument = function
  | Type_or_index ty -> Const_arg (index_expression ty)
  | argument -> argument

let stmt_span = function
  | Let { span; _ }
  | View { span; _ }
  | Assign (_, _, span)
  | Compound_assign (_, _, _, span, _)
  | Return (_, span)
  | If (_, _, _, span)
  | While (_, _, _, span)
  | Break (_, span)
  | Continue (_, span)
  | Defer (_, span)
  | Expr_stmt (_, span, _)
  | Block (_, span)
  | For (_, _, _, _, _, span)
  | Switch (_, _, _, span)
  | Const_for { span; _ }
  | Const_copy { span; _ } ->
      span

let string_literal_name c value =
  let buffer = Buffer.create (String.length value) in
  let escape = function
    | '\000' -> "\\0"
    | '\n' -> "\\n"
    | '\r' -> "\\r"
    | '\t' -> "\\t"
    | '\\' -> "\\\\"
    | '"' -> "\\\""
    | character when Char.code character < 32 || Char.code character > 126 ->
        Printf.sprintf "\\x%02x" (Char.code character)
    | character -> String.make 1 character
  in
  String.iter (fun character -> Buffer.add_string buffer (escape character)) value;
  (if c then "c" else "") ^ "\"" ^ Buffer.contents buffer ^ "\""

let rec type_name = function
  | Bool -> "bool"
  | Int U8 -> "u8"
  | Int U16 -> "u16"
  | Int U32 -> "u32"
  | Int U64 -> "u64"
  | Int I8 -> "i8"
  | Int I16 -> "i16"
  | Int I32 -> "i32"
  | Int I64 -> "i64"
  | Int Usize -> "usize"
  | Int Isize -> "isize"
  | Float F32 -> "f32"
  | Float F64 -> "f64"
  | Addr -> "addr"
  | Handle t -> "handle[" ^ type_name t ^ "]"
  | Array (n, t) -> "arr[" ^ n.text ^ ", " ^ type_name t ^ "]"
  | Vec (n, t) -> "vec[" ^ n.text ^ ", " ^ type_name t ^ "]"
  | Named_type (s, _) -> s
  | Applied_type (name, _, args, _) ->
      name ^ "[" ^ String.concat ", " (List.map generic_arg_name args) ^ "]"
  | Void -> "void"

and generic_arg_name = function
  | Type_arg t | Type_or_index t -> type_name t
  | Const_arg e -> expr_name e
  | Name_arg (name, _) -> name

and binop_precedence = function
  | Or -> 1
  | And -> 2
  | Bit_or -> 3
  | Bit_xor -> 4
  | Bit_and -> 5
  | Eq | Ne -> 6
  | Lt | Le | Gt | Ge -> 7
  | Shl | Shr -> 8
  | Add | Sub -> 9
  | Mul | Div | Rem -> 10

and expr_precedence = function
  | Ternary _ -> 0
  | Binary (op, _, _, _) -> binop_precedence op
  | Unary _ | C_dereference _ | Addr_of _ | Sizeof_value _ -> 11
  | Call _ | Generic_args _ | Cast _ | Select _ | C_dot_star _ | Field _ | Arrow_field _
    ->
      12
  | _ -> 13

and expr_name_child parent_precedence right_child expression =
  let text = expr_name expression and precedence = expr_precedence expression in
  if precedence < parent_precedence || (precedence = parent_precedence && right_child)
  then "(" ^ text ^ ")"
  else text

and expr_name = function
  | Int_lit (s, _) -> s
  | Float_lit (s, _) -> s
  | Bool_lit (true, _) -> "true"
  | Bool_lit (false, _) -> "false"
  | Null _ -> "null"
  | String_lit (c, s, _) -> string_literal_name c s
  | Ident (s, _) -> s
  | Unary (op, e, _) ->
      let operator = match op with Neg -> "-" | Not -> "!" | Bit_not -> "~" in
      let operand =
        match (op, e) with
        | Neg, Unary (Neg, _, _)
        | Not, Unary (Not, _, _)
        | Bit_not, Unary (Bit_not, _, _) ->
            " "
        | _ -> ""
      in
      operator ^ operand ^ expr_name_child 11 false e
  | C_dereference (e, _, _) -> "*" ^ expr_name_child 11 false e
  | C_dot_star (e, _) -> expr_name_child 12 false e ^ ".*"
  | Parenthesized (e, _) -> "(" ^ expr_name e ^ ")"
  | Binary (op, l, r, _) ->
      expr_name_child (binop_precedence op) false l
      ^ " "
      ^ (match op with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Rem -> "%"
        | Bit_and -> "&"
        | Bit_or -> "|"
        | Bit_xor -> "^"
        | Eq -> "=="
        | Ne -> "!="
        | Lt -> "<"
        | Le -> "<="
        | Gt -> ">"
        | Ge -> ">="
        | And -> "&&"
        | Or -> "||"
        | Shl -> "<<"
        | Shr -> ">>")
      ^ " "
      ^ expr_name_child (binop_precedence op) true r
  | Call (f, xs, _) ->
      expr_name_child 12 false f ^ "("
      ^ String.concat ", " (List.map expr_name xs)
      ^ ")"
  | Generic_args (f, xs, _) ->
      expr_name_child 12 false f ^ "["
      ^ String.concat ", " (List.map generic_arg_name xs)
      ^ "]"
  | Cast (k, t, e, _) ->
      (match k with
        | Zext -> "zext"
        | Sext -> "sext"
        | Trunc -> "trunc"
        | Bitcast -> "bitcast"
        | Float_widen -> "fpext")
      ^ "[" ^ type_name t ^ "](" ^ expr_name e ^ ")"
  | Select (a, args, _) ->
      expr_name_child 12 false a ^ "["
      ^ String.concat ", " (List.map generic_arg_name args)
      ^ "]"
  | Field (a, n, _) -> expr_name_child 12 false a ^ "." ^ n
  | Arrow_field (a, n, _, _) -> expr_name_child 12 false a ^ "->" ^ n
  | Addr_of ((Addr_of _ as e), _) -> "&(" ^ expr_name e ^ ")"
  | Addr_of (e, _) -> "&" ^ expr_name_child 11 false e
  | Handle_from_addr (t, e, _) ->
      "handle_from_addr[" ^ type_name t ^ "](" ^ expr_name e ^ ")"
  | Sizeof (t, _) -> "sizeof[" ^ type_name t ^ "]"
  | Sizeof_value (e, _) -> "sizeof " ^ expr_name_child 11 false e
  | Alignof (t, _) -> "alignof[" ^ type_name t ^ "]"
  | Offsetof (t, f, _, _) -> "offsetof[" ^ type_name t ^ ", " ^ f ^ "]"
  | Splat (e, _) -> "splat(" ^ expr_name e ^ ")"
  | Ternary (c, a, b, _) ->
      "if " ^ expr_name c ^ " { " ^ expr_name a ^ " } else { " ^ expr_name b ^ " }"
  | Array_lit (xs, _) -> "{" ^ String.concat ", " (List.map expr_name xs) ^ "}"
  | Designated_field (name, _, value, _) -> "." ^ name ^ " = " ^ expr_name value
  | Designated_index (index, value, _) ->
      "[" ^ expr_name index ^ "] = " ^ expr_name value

let aggregate_length expression span = { expression; text = expr_name expression; span }

let int_aggregate_length length span =
  aggregate_length (Int_lit (string_of_int length, span)) span

let item_span = function
  | Use { span; _ }
  | Pkg { span; _ }
  | Const { span; _ }
  | Global { span; _ }
  | Struct { span; _ }
  | Opaque { span; _ }
  | Func { span; _ } ->
      span

let render_program program =
  let buffer = Buffer.create 4096 in
  let text = Buffer.add_string buffer in
  let add_name = text in
  let add_escaped s =
    let length = String.length s in
    let rec go i =
      if i < length then (
        let stop = min length (i + 256) in
        text (String.escaped (String.sub s i (stop - i)));
        go stop)
    in
    go 0
  in
  let emit_comma_list : 'a. ('a -> unit) -> 'a list -> unit =
   fun emit xs ->
    List.iteri
      (fun i x ->
        if i > 0 then text ", ";
        emit x)
      xs
  in
  let rec emit_ty ty =
    match ty with
    | Bool -> text "bool"
    | Int U8 -> text "u8"
    | Int U16 -> text "u16"
    | Int U32 -> text "u32"
    | Int U64 -> text "u64"
    | Int I8 -> text "i8"
    | Int I16 -> text "i16"
    | Int I32 -> text "i32"
    | Int I64 -> text "i64"
    | Int Usize -> text "usize"
    | Int Isize -> text "isize"
    | Float F32 -> text "f32"
    | Float F64 -> text "f64"
    | Addr -> text "addr"
    | Handle inner ->
        text "handle[";
        emit_ty inner;
        text "]"
    | Array (n, inner) ->
        text "arr[";
        add_name n.text;
        text ", ";
        emit_ty inner;
        text "]"
    | Vec (n, inner) ->
        text "vec[";
        add_name n.text;
        text ", ";
        emit_ty inner;
        text "]"
    | Named_type (name, _) -> add_name name
    | Applied_type (name, _, args, _) ->
        add_name name;
        text "[";
        emit_comma_list emit_generic_arg args;
        text "]"
    | Void -> text "void"
  and emit_generic_arg = function
    | Type_arg ty | Type_or_index ty -> emit_ty ty
    | Const_arg e -> emit_expr e
    | Name_arg (name, _) -> add_name name
  and emit_generic_param = function
    | Type_param { name; _ } -> add_name name
    | Const_param { name; ty; _ } ->
        add_name name;
        text " const ";
        emit_ty ty
  and emit_generic_params = function
    | [] -> ()
    | params ->
        text "[";
        emit_comma_list emit_generic_param params;
        text "]"
  and emit_expr e =
    match e with
    | Int_lit (s, _) -> add_name s
    | Float_lit (s, _) -> add_name s
    | Bool_lit (true, _) -> text "true"
    | Bool_lit (false, _) -> text "false"
    | Null _ -> text "null"
    | String_lit (c, s, _) ->
        text (if c then "c\"" else "\"");
        add_escaped s;
        text "\""
    | Ident (name, _) -> add_name name
    | Unary (op, x, _) ->
        text (match op with Neg -> "-" | Not -> "!" | Bit_not -> "~");
        emit_expr x
    | C_dereference (x, _, _) ->
        text "*";
        emit_expr x
    | C_dot_star (x, _) ->
        emit_expr x;
        text ".*"
    | Parenthesized (x, _) ->
        text "(";
        emit_expr x;
        text ")"
    | Binary (op, l, r, _) ->
        emit_expr l;
        text " ";
        text
          (match op with
          | Add -> "+"
          | Sub -> "-"
          | Mul -> "*"
          | Div -> "/"
          | Rem -> "%"
          | Bit_and -> "&"
          | Bit_or -> "|"
          | Bit_xor -> "^"
          | Eq -> "=="
          | Ne -> "!="
          | Lt -> "<"
          | Le -> "<="
          | Gt -> ">"
          | Ge -> ">="
          | And -> "&&"
          | Or -> "||"
          | Shl -> "<<"
          | Shr -> ">>");
        text " ";
        emit_expr r
    | Call (f, xs, _) ->
        emit_expr f;
        text "(";
        emit_comma_list emit_expr xs;
        text ")"
    | Generic_args (f, xs, _) ->
        emit_expr f;
        text "[";
        emit_comma_list emit_generic_arg xs;
        text "]"
    | Cast (k, ty, x, _) ->
        text
          (match k with
          | Zext -> "zext["
          | Sext -> "sext["
          | Trunc -> "trunc["
          | Bitcast -> "bitcast["
          | Float_widen -> "fpext[");
        emit_ty ty;
        text "](";
        emit_expr x;
        text ")"
    | Select (a, args, _) ->
        emit_expr a;
        text "[";
        List.iteri
          (fun i arg ->
            if i > 0 then text ", ";
            match arg with
            | Type_arg ty | Type_or_index ty -> emit_ty ty
            | Const_arg e -> emit_expr e
            | Name_arg (name, _) -> add_name name)
          args;
        text "]"
    | Field (a, name, _) ->
        emit_expr a;
        text ".";
        add_name name
    | Arrow_field (a, name, _, _) ->
        emit_expr a;
        text "->";
        add_name name
    | Addr_of (x, _) ->
        text "&";
        emit_expr x
    | Handle_from_addr (t, x, _) ->
        text "handle_from_addr[";
        emit_ty t;
        text "]( ";
        emit_expr x;
        text ")"
    | Sizeof (ty, _) ->
        text "sizeof[";
        emit_ty ty;
        text "]"
    | Sizeof_value (e, _) ->
        text "sizeof ";
        emit_expr e
    | Alignof (ty, _) ->
        text "alignof[";
        emit_ty ty;
        text "]"
    | Offsetof (ty, name, _, _) ->
        text "offsetof[";
        emit_ty ty;
        text ", ";
        add_name name;
        text "]"
    | Splat (x, _) ->
        text "splat(";
        emit_expr x;
        text ")"
    | Ternary (c, a, b, _) ->
        emit_expr c;
        text " ? ";
        emit_expr a;
        text " : ";
        emit_expr b
    | Array_lit (xs, _) ->
        text "{";
        emit_comma_list emit_expr xs;
        text "}"
    | Designated_field (name, _, value, _) ->
        text ".";
        add_name name;
        text " = ";
        emit_expr value
    | Designated_index (index, value, _) ->
        text "[";
        emit_expr index;
        text "] = ";
        emit_expr value
  and emit_stmt indent s =
    match s with
    | Let { name; ty; init; _ } -> (
        text indent;
        add_name name;
        text " ";
        emit_ty ty;
        match init with
        | None -> ()
        | Some e ->
            text " = ";
            emit_expr e)
    | View { name; place; _ } ->
        text indent;
        text "view ";
        add_name name;
        text " = ";
        emit_expr place
    | Assign (Target_ident (name, _), e, _) ->
        text indent;
        add_name name;
        text " = ";
        emit_expr e
    | Assign (_, e, _) ->
        text indent;
        text "<target> = ";
        emit_expr e
    | Compound_assign (_, _, e, _, _) ->
        text indent;
        text "<target> compound= ";
        emit_expr e
    | Return (None, _) ->
        text indent;
        text "return"
    | Return (Some e, _) ->
        text indent;
        text "return ";
        emit_expr e
    | Expr_stmt (e, _, _) ->
        text indent;
        emit_expr e
    | Break (None, _) ->
        text indent;
        text "break"
    | Break (Some (name, _), _) ->
        text indent;
        text "break ";
        add_name name
    | Continue (None, _) ->
        text indent;
        text "continue"
    | Continue (Some (name, _), _) ->
        text indent;
        text "continue ";
        add_name name
    | Defer (xs, _) ->
        text indent;
        text "defer {";
        emit_lines (indent ^ "  ") xs;
        text "\n";
        text indent;
        text "}"
    | Block (xs, _) | Const_copy { body = xs; _ } ->
        text indent;
        text "{";
        emit_lines (indent ^ "  ") xs;
        text "\n";
        text indent;
        text "}"
    | Const_for { name; ty; low; high; body; _ } ->
        text indent;
        text "for const ";
        add_name name;
        text " ";
        emit_ty ty;
        text " in ";
        emit_expr low;
        text "..";
        emit_expr high;
        text " {";
        emit_lines (indent ^ "  ") body;
        text "\n";
        text indent;
        text "}"
    | If (c, yes, no, _) -> (
        text indent;
        text "if ";
        emit_expr c;
        text " {";
        emit_lines (indent ^ "  ") yes;
        text "\n";
        text indent;
        text "}";
        match no with
        | None -> ()
        | Some xs ->
            text "\n";
            text indent;
            text "else {";
            emit_lines (indent ^ "  ") xs;
            text "\n";
            text indent;
            text "}")
    | While (label, c, xs, _) ->
        text indent;
        Option.iter
          (fun label ->
            add_name label.name;
            text ": ")
          label;
        text "while ";
        emit_expr c;
        text " {";
        emit_lines (indent ^ "  ") xs;
        text "\n";
        text indent;
        text "}"
    | For (label, _, _, _, xs, _) ->
        text indent;
        Option.iter
          (fun label ->
            add_name label.name;
            text ": ")
          label;
        text "for ... {";
        emit_lines (indent ^ "  ") xs;
        text "\n";
        text indent;
        text "}"
    | Switch (e, _, _, _) ->
        text indent;
        text "switch ";
        emit_expr e;
        text " { ... }"
  and emit_lines indent xs =
    List.iter
      (fun s ->
        text "\n";
        emit_stmt indent s)
      xs
  and emit_body body =
    match body with
    | Declaration -> ()
    | Statements xs ->
        text " {\n";
        List.iteri
          (fun i s ->
            if i > 0 then text "\n";
            emit_stmt "  " s)
          xs;
        text "\n}"
  in
  let emit_item item =
    match item with
    | Pkg { pkg = { modules; _ }; _ } ->
        text "use pkg \"";
        add_escaped modules;
        text "\""
    | Use { path; c_header; os; _ } -> (
        match c_header with
        | None ->
            text "use ";
            (match os with
            | Some os -> text (Target_layout.os_name os ^ " ")
            | None -> ());
            text "\"";
            add_escaped path;
            text "\""
        | Some (C_quoted header) ->
            text "use \"C\"";
            text " \"";
            add_escaped header;
            text "\""
        | Some (C_system header) ->
            text "use \"C\"";
            text " <";
            text header;
            text ">"
        | Some (C_fragment fragment) ->
            text "use \"C\"";
            text
              (Printf.sprintf " <<%s\n%s\n%s" fragment.tag fragment.text fragment.tag))
    | Const { name; ty; value; _ } ->
        text "const ";
        add_name name;
        text " ";
        emit_ty ty;
        text " = ";
        emit_expr value
    | Global { name; ty; init; linkage; _ } ->
        (match linkage with
        | Internal_global -> text "var "
        | Export_c | Import_c | Import_const_c -> text "extern var ");
        add_name name;
        text " ";
        emit_ty ty;
        Option.iter
          (fun value ->
            text " = ";
            emit_expr value)
          init
    | Struct { name; generic_params; fields; align; _ } ->
        text "struct ";
        add_name name;
        emit_generic_params generic_params;
        (match align with
        | None -> ()
        | Some n ->
            text " @align(";
            text (string_of_int n);
            text ")");
        text " {\n";
        List.iteri
          (fun i (f : field) ->
            if i > 0 then text "\n";
            text "  ";
            add_name f.name;
            text " ";
            emit_ty f.ty)
          fields;
        text "\n}"
    | Opaque { name; _ } ->
        text "opaque ";
        add_name name
    | Func { name; generic_params; params; ret; body; linkage; _ } ->
        text (match linkage with Internal -> "fn " | External_c -> "extern fn ");
        add_name name;
        emit_generic_params generic_params;
        text "(";
        emit_comma_list
          (fun (p : param) ->
            add_name p.name;
            text " ";
            emit_ty p.ty)
          params;
        text ") ";
        emit_ty ret;
        emit_body body
  in
  List.iteri
    (fun i item ->
      if i > 0 then (
        text "\n";
        text "\n");
      emit_item item)
    program.items;
  text "\n";
  Buffer.contents buffer

let expanded_walker ~identifiers ~limit =
  let total = ref 0 in
  let failed = ref None in
  let count span =
    if !failed = None then if !total >= limit then failed := Some span else incr total
  in
  let rec go_ty at ty =
    if !failed = None then
      match ty with
      | Bool | Int _ | Float _ | Void | Addr -> count at
      | Named_type (name, _) ->
          identifiers := name :: !identifiers;
          count at
      | Handle inner ->
          count at;
          go_ty at inner
      | Array (length, inner) | Vec (length, inner) ->
          count at;
          go_expr length.expression;
          go_ty at inner
      | Applied_type (_, _, args, span) ->
          count span;
          List.iter (go_generic_arg span) args
  and go_generic_arg at = function
    | Type_arg inner | Type_or_index inner ->
        count at;
        go_ty at inner
    | Const_arg e ->
        count at;
        go_expr e
    | Name_arg (name, span) ->
        identifiers := name :: !identifiers;
        count span
  and go_expr e =
    if !failed = None then (
      let at = expr_span e in
      count at;
      match e with
      | Ident (name, _) -> identifiers := name :: !identifiers
      | Int_lit _ | Float_lit _ | Bool_lit _ | Null _ | String_lit _ -> ()
      | Unary (_, x, _) | C_dereference (x, _, _) | C_dot_star (x, _) -> go_expr x
      | Parenthesized (x, _) -> go_expr x
      | Binary (_, l, r, _) ->
          go_expr l;
          go_expr r
      | Call (f, xs, _) ->
          go_expr f;
          List.iter go_expr xs
      | Generic_args (f, args, _) ->
          go_expr f;
          List.iter (go_generic_arg at) args
      | Cast (_, ty, x, _) ->
          go_ty at ty;
          go_expr x
      | Select (a, args, _) ->
          go_expr a;
          List.iter (go_generic_arg at) args
      | Field (a, _, _) | Arrow_field (a, _, _, _) -> go_expr a
      | Addr_of (x, _) -> go_expr x
      | Handle_from_addr (t, x, _) ->
          go_ty at t;
          go_expr x
      | Sizeof (ty, _) | Alignof (ty, _) -> go_ty at ty
      | Sizeof_value (expression, _) -> go_expr expression
      | Offsetof (ty, _, _, _) -> go_ty at ty
      | Splat (x, _) -> go_expr x
      | Ternary (c, a, b, _) ->
          go_expr c;
          go_expr a;
          go_expr b
      | Array_lit (xs, _) -> List.iter go_expr xs
      | Designated_field (_, _, value, _) -> go_expr value
      | Designated_index (index, value, _) ->
          go_expr index;
          go_expr value)
  and go_target = function
    | Target_ident (name, span) ->
        identifiers := name :: !identifiers;
        count span
    | Target_select (a, args) ->
        let at = expr_span a in
        count at;
        go_expr a;
        List.iter (go_generic_arg at) args
    | Target_field (a, _, span) ->
        count span;
        go_expr a
  and go_stmts xs = List.iter go_stmt xs
  and go_stmt s =
    if !failed = None then (
      count (stmt_span s);
      match s with
      | Let { ty; init; span; _ } ->
          go_ty span ty;
          Option.iter go_expr init
      | View { place; _ } -> go_expr place
      | Assign (t, e, _) | Compound_assign (t, _, e, _, _) ->
          go_target t;
          go_expr e
      | Return (e, _) -> Option.iter go_expr e
      | If (c, yes, no, _) ->
          go_expr c;
          go_stmts yes;
          Option.iter go_stmts no
      | While (_, c, xs, _) ->
          go_expr c;
          go_stmts xs
      | Break _ | Continue _ -> ()
      | Defer (xs, _) | Block (xs, _) | Const_copy { body = xs; _ } -> go_stmts xs
      | Const_for { ty; ty_span; low; high; body; _ } ->
          go_ty ty_span ty;
          go_expr low;
          go_expr high;
          go_stmts body
      | Expr_stmt (e, _, _) -> go_expr e
      | For (_, i, c, st, body, _) ->
          Option.iter go_stmt i;
          Option.iter go_expr c;
          Option.iter go_stmt st;
          go_stmts body
      | Switch (scr, arms, default, _) ->
          go_expr scr;
          List.iter
            (fun (es, xs) ->
              List.iter go_expr es;
              go_stmts xs)
            arms;
          Option.iter go_stmts default)
  and go_field (f : field) =
    if !failed = None then (
      count f.span;
      go_ty f.span f.ty)
  and go_param (p : param) =
    if !failed = None then (
      count p.span;
      go_ty p.span p.ty)
  and go_generic_param = function
    | Type_param { span; _ } -> count span
    | Const_param { ty; span; _ } ->
        count span;
        go_ty span ty
  and go_item item =
    if !failed = None then (
      let at = item_span item in
      count at;
      match item with
      | Use _ | Pkg _ -> ()
      | Const { ty; value; span; _ } ->
          go_ty span ty;
          go_expr value
      | Global { name; linkage; ty; init; span; _ } ->
          if linkage <> Internal_global then identifiers := name :: !identifiers;
          go_ty span ty;
          Option.iter go_expr init
      | Struct { generic_params; fields; _ } ->
          List.iter go_generic_param generic_params;
          List.iter go_field fields
      | Opaque _ -> ()
      | Func { name; linkage; params; ret; body; generic_params; span; _ } -> (
          if linkage <> Internal then identifiers := name :: !identifiers;
          List.iter go_generic_param generic_params;
          List.iter go_param params;
          go_ty span ret;
          match body with Statements xs -> go_stmts xs | Declaration -> ()))
  in
  (go_item, go_stmt, total, failed)

let fold_expanded_nodes ?(identifiers = ref []) ~limit program =
  let go_item, _, total, failed = expanded_walker ~identifiers ~limit in
  List.iter go_item program.items;
  (!total, !failed)

let count_stmt_nodes statements =
  let _, go_stmt, total, _ = expanded_walker ~identifiers:(ref []) ~limit:max_int in
  List.iter go_stmt statements;
  !total

let stmt_names statement =
  let identifiers = ref [] in
  let _, go_stmt, _, _ = expanded_walker ~identifiers ~limit:max_int in
  go_stmt statement;
  List.sort_uniq String.compare !identifiers

let count_expanded_nodes program = fst (fold_expanded_nodes ~limit:max_int program)

let used_names program =
  let identifiers = ref [] in
  ignore (fold_expanded_nodes ~identifiers ~limit:max_int program);
  List.sort_uniq String.compare !identifiers

let unresolved_names program =
  let declaration name =
    List.exists
      (function
        | Const { name = declared; _ }
        | Struct { name = declared; _ }
        | Opaque { name = declared; _ }
        | Global { name = declared; linkage = Internal_global; _ }
        | Func { name = declared; linkage = Internal; _ } ->
            declared = name
        | _ -> false)
      program.items
  in
  used_names program |> List.filter (fun name -> not (declaration name))

let check_expanded_nodes ~limits program =
  if limits.Limits.max_ast_nodes < 0 then
    Error
      (Diag.error Span.synthetic
         (Printf.sprintf "budget max_ast_nodes must not be negative (profile %s)"
            (Limits.budget_profile_name limits)))
  else
    match fold_expanded_nodes ~limit:limits.Limits.max_ast_nodes program with
    | _, None -> Ok ()
    | _, Some span ->
        Error
          (Diag.error span
             (Printf.sprintf
                "cumulative expanded AST nodes exceed budget max_ast_nodes of %d \
                 (profile %s)"
                limits.Limits.max_ast_nodes
                (Limits.budget_profile_name limits)))

let specialization_name_delimiter = "$spec$"

let specialization_template_name name =
  let delimiter_length = String.length specialization_name_delimiter in
  let limit = String.length name - delimiter_length in
  let rec find index =
    if index > limit then None
    else if String.sub name index delimiter_length = specialization_name_delimiter then
      Some (String.sub name 0 index)
    else find (index + 1)
  in
  find 0

let item_span_by_name program name =
  let lookup name =
    List.find_map
      (fun item ->
        match item with
        | (Func { name = item_name; span; _ } | Const { name = item_name; span; _ })
          when item_name = name ->
            Some span
        | _ -> None)
      program.items
  in
  match lookup name with
  | Some _ as span -> span
  | None ->
      Option.fold ~none:None
        ~some:(fun template -> lookup template)
        (specialization_template_name name)

let rec stmt_terminates = function
  | Return _ | Break _ | Continue _ -> true
  | Block (body, _) -> block_terminates body
  | If (_, then_body, Some else_body, _) ->
      block_terminates then_body && block_terminates else_body
  | Switch (_, arms, Some default, _) ->
      block_terminates default
      && List.for_all (fun (_, body) -> block_terminates body) arms
  | _ -> false

and block_terminates body =
  match body with
  | [] -> false
  | statement :: rest ->
      if stmt_terminates statement then true else block_terminates rest

let rec copy_jumps ~select statements = List.concat_map (copy_jump ~select) statements

and copy_jump ~select = function
  | (Break (None, _) | Continue (None, _)) as statement -> select statement
  | If (_, yes, no, _) ->
      copy_jumps ~select yes @ Option.fold ~none:[] ~some:(copy_jumps ~select) no
  | Block (body, _) | Defer (body, _) -> copy_jumps ~select body
  | Switch (_, arms, default, _) ->
      List.concat_map (fun (_, body) -> copy_jumps ~select body) arms
      @ Option.fold ~none:[] ~some:(copy_jumps ~select) default
  | Let _ | View _ | Assign _ | Compound_assign _ | Return _ | Expr_stmt _ | Break _
  | Continue _ | While _ | For _ | Const_for _ | Const_copy _ ->
      []

let copy_breaks body =
  copy_jumps ~select:(function Break (None, span) -> [ span ] | _ -> []) body

let copy_continues body =
  copy_jumps ~select:(function Continue (None, span) -> [ span ] | _ -> []) body <> []

let rec continues_to_breaks label statements =
  List.map (continue_to_break label) statements

and continue_to_break label = function
  | Continue (None, span) -> Break (Some (label, span), span)
  | If (condition, yes, no, span) ->
      If
        ( condition,
          continues_to_breaks label yes,
          Option.map (continues_to_breaks label) no,
          span )
  | Block (body, span) -> Block (continues_to_breaks label body, span)
  | Defer (body, span) -> Defer (continues_to_breaks label body, span)
  | Switch (value, arms, default, span) ->
      Switch
        ( value,
          List.map (fun (cases, body) -> (cases, continues_to_breaks label body)) arms,
          Option.map (continues_to_breaks label) default,
          span )
  | ( Let _ | View _ | Assign _ | Compound_assign _ | Return _ | Expr_stmt _ | Break _
    | Continue _ | While _ | For _ | Const_for _ | Const_copy _ ) as statement ->
      statement
