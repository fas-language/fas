(* Enums ("Fas v2 decisions.md", Enums).

   [desugar] runs before any type is resolved:
   - it checks each `enum Name K { ... }` and records it in [Hir.enums]
     (underlying integer type, members and values in declaration order);
   - each member becomes a constant `Name.Member` of type `Name`, valued
     `enum_from[Name](v)`, so constants, globals, generics and switch labels
     see members exactly as other constants;
   - every `Name.Member` in the program is rewritten to that constant's name.
     A local can never be named like an enum (Sema_flow refuses it), so
     `Name.x` with `Name` an enum always means a member. *)

let error span message = Error [ Diag.error span message ]
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let member_name enum member = enum ^ "." ^ member

let int_bits = function
  | Hir.U8 | Hir.I8 -> 8
  | Hir.U16 | Hir.I16 -> 16
  | Hir.U32 | Hir.I32 -> 32
  | Hir.U64 | Hir.I64 -> 64

let signed = function
  | Hir.I8 | Hir.I16 | Hir.I32 | Hir.I64 -> true
  | Hir.U8 | Hir.U16 | Hir.U32 | Hir.U64 -> false

(* the largest value of [kind], as a 64-bit pattern *)
let max_value kind =
  let bits = int_bits kind in
  if signed kind then Int64.sub (Int64.shift_left 1L (bits - 1)) 1L
  else if bits = 64 then -1L
  else Int64.sub (Int64.shift_left 1L bits) 1L

let min_value kind =
  if signed kind then Int64.neg (Int64.shift_left 1L (int_bits kind - 1)) else 0L

let fits kind v =
  if signed kind then v >= min_value kind && v <= max_value kind
  else Int64.unsigned_compare v (max_value kind) <= 0

let value_text kind v = if signed kind then Int64.to_string v else Printf.sprintf "%Lu" v

(* a member's value: an integer literal, or a negated one for a signed type *)
let literal_value kind = function
  | Ast.Int_lit (raw, span) -> (
      match Sema_numeric.parse_integer raw with
      | Ok v when signed kind && v < 0L -> error span "enum value does not fit its type"
      | Ok v -> Ok (v, span)
      | Error message -> error span message)
  | Ast.Unary (Ast.Neg, Ast.Int_lit (raw, span), whole) when signed kind -> (
      match Sema_numeric.parse_integer raw with
      | Ok v when v >= 0L || v = Int64.min_int -> Ok (Int64.neg v, whole)
      | Ok _ -> error whole "enum value does not fit its type"
      | Error message -> error span message)
  | value ->
      error (Ast.expr_span value)
        "an enum member's value must be an integer literal (`Plus = 10`)"

let check_enum name repr repr_span name_span members =
  let* kind =
    match repr with
    | Ast.Int k ->
        Ok
          (match k with
          | Ast.U8 -> Hir.U8
          | Ast.U16 -> Hir.U16
          | Ast.U32 -> Hir.U32
          | Ast.U64 -> Hir.U64
          | Ast.I8 -> Hir.I8
          | Ast.I16 -> Hir.I16
          | Ast.I32 -> Hir.I32
          | Ast.I64 -> Hir.I64)
    | _ ->
        Error
          [
            Diag.error ~help:(Printf.sprintf "write `enum %s u8 {`" name) repr_span
              "an enum's underlying type must be an integer type";
          ]
  in
  let* () =
    if members = [] then error name_span (Printf.sprintf "enum `%s` has no members" name)
    else Ok ()
  in
  let names = Hashtbl.create 16 and values = Hashtbl.create 16 in
  let rec go next acc = function
    | [] -> Ok (List.rev acc)
    | (member, span, value) :: rest ->
        let* () =
          match Hashtbl.find_opt names member with
          | Some first ->
              Error
                [
                  Diag.error
                    ~notes:[ "first declared at " ^ Span.to_string first ]
                    span
                    (Printf.sprintf "duplicate member `%s` in enum `%s`" member name);
                ]
          | None -> Ok ()
        in
        let* v, value_span =
          match (value, next) with
          | Some value, _ -> literal_value kind value
          | None, Some v -> Ok (v, span)
          | None, None ->
              error span
                (Printf.sprintf
                   "`%s.%s` counts past the largest `%s`; give it a value"
                   name member (Hir.ty_name (Hir.Int kind)))
        in
        let* () =
          if fits kind v then Ok ()
          else
            error value_span
              (Printf.sprintf "`%s.%s` = %s does not fit `%s`" name member
                 (value_text kind v) (Hir.ty_name (Hir.Int kind)))
        in
        let* () =
          match Hashtbl.find_opt values v with
          | Some other ->
              error span
                (Printf.sprintf "`%s.%s` and `%s.%s` have the same value %s" name other
                   name member (value_text kind v))
          | None -> Ok ()
        in
        Hashtbl.replace names member span;
        Hashtbl.replace values v member;
        let next = if v = max_value kind then None else Some (Int64.add v 1L) in
        go next ((member, v) :: acc) rest
  in
  let* members = go (Some 0L) [] members in
  Ok { Hir.enum_kind = kind; members }

(* ---- rewriting `Name.Member` ---- *)

let cover (a : Span.t) (b : Span.t) =
  if Span.is_synthetic a || Span.is_synthetic b || a.file <> b.file then b
  else
    Span.make ~file:a.file ~start_offset:a.start_offset ~end_offset:b.end_offset
      ~line:a.line ~column:a.column

exception Bad of Diag.t list

let rec expr e =
  match e with
  | Ast.Field (Ast.Ident (enum, enum_span), member, member_span)
    when Stdlib.Hashtbl.mem Hir.enums enum ->
      let d = Option.get (Hir.enum_def enum) in
      if List.mem_assoc member d.Hir.members then
        Ast.Ident (member_name enum member, cover enum_span member_span)
      else
        raise
          (Bad
             [
               Diag.error member_span
                 (Printf.sprintf "enum `%s` has no member `%s`" enum member);
             ])
  | Ast.Int_lit _ | Ast.Float_lit _ | Ast.Bool_lit _ | Ast.Null _ | Ast.String_lit _
  | Ast.Ident _ ->
      e
  | Ast.Unary (op, a, s) -> Ast.Unary (op, expr a, s)
  | Ast.C_dereference (a, n, s) -> Ast.C_dereference (expr a, n, s)
  | Ast.C_dot_star (a, s) -> Ast.C_dot_star (expr a, s)
  | Ast.Parenthesized (a, s) -> Ast.Parenthesized (expr a, s)
  | Ast.Binary (op, a, b, s) -> Ast.Binary (op, expr a, expr b, s)
  | Ast.Call (f, args, s) -> Ast.Call (expr f, List.map expr args, s)
  | Ast.Generic_args (f, args, s) -> Ast.Generic_args (expr f, List.map generic_arg args, s)
  | Ast.Cast (k, t, a, s) -> Ast.Cast (k, ty t, expr a, s)
  | Ast.Select (a, args, s) -> Ast.Select (expr a, List.map generic_arg args, s)
  | Ast.Field (a, n, s) -> Ast.Field (expr a, n, s)
  | Ast.Arrow_field (a, n, s1, s2) -> Ast.Arrow_field (expr a, n, s1, s2)
  | Ast.Addr_of (a, s) -> Ast.Addr_of (expr a, s)
  | Ast.Handle_from_addr (t, a, s) -> Ast.Handle_from_addr (ty t, expr a, s)
  | Ast.Sizeof (t, s) -> Ast.Sizeof (ty t, s)
  | Ast.Sizeof_value (a, s) -> Ast.Sizeof_value (expr a, s)
  | Ast.Alignof (t, s) -> Ast.Alignof (ty t, s)
  | Ast.Offsetof (t, n, s1, s2) -> Ast.Offsetof (ty t, n, s1, s2)
  | Ast.Splat (a, s) -> Ast.Splat (expr a, s)
  | Ast.Ternary (c, a, b, s) -> Ast.Ternary (expr c, expr a, expr b, s)
  | Ast.Array_lit (xs, s) -> Ast.Array_lit (List.map expr xs, s)
  | Ast.Designated_field (n, s1, a, s2) -> Ast.Designated_field (n, s1, expr a, s2)
  | Ast.Designated_index (i, a, s) -> Ast.Designated_index (expr i, expr a, s)
  | Ast.Try (a, s) -> Ast.Try (expr a, s)
  | Ast.Catch (a, handler, s) ->
      let handler =
        match handler with
        | Ast.Catch_value v -> Ast.Catch_value (expr v)
        | Ast.Catch_block (binding, body) -> Ast.Catch_block (binding, stmts body)
      in
      Ast.Catch (expr a, handler, s)

and generic_arg = function
  | Ast.Type_arg t -> Ast.Type_arg (ty t)
  | Ast.Type_or_index t -> Ast.Type_or_index (ty t)
  | Ast.Const_arg e -> Ast.Const_arg (expr e)
  | Ast.Name_arg _ as a -> a

and length (l : Ast.aggregate_length) = { l with expression = expr l.expression }

and ty = function
  | Ast.Handle t -> Ast.Handle (ty t)
  | Ast.Array (l, t) -> Ast.Array (length l, ty t)
  | Ast.Vec (l, t) -> Ast.Vec (length l, ty t)
  | Ast.Applied_type (n, s1, args, s2) -> Ast.Applied_type (n, s1, List.map generic_arg args, s2)
  | (Ast.Bool | Ast.Int _ | Ast.Float _ | Ast.Addr | Ast.Named_type _ | Ast.Void) as t -> t

and target = function
  | Ast.Target_ident _ as t -> t
  | Ast.Target_select (a, args) -> Ast.Target_select (expr a, List.map generic_arg args)
  | Ast.Target_field (a, n, s) -> Ast.Target_field (expr a, n, s)

and stmt = function
  | Ast.Let l -> Ast.Let { l with ty = ty l.ty; init = Option.map expr l.init }
  | Ast.View v -> Ast.View { v with place = expr v.place }
  | Ast.Assign (t, e, s) -> Ast.Assign (target t, expr e, s)
  | Ast.Compound_assign (t, op, e, s1, s2) -> Ast.Compound_assign (target t, op, expr e, s1, s2)
  | Ast.Return (e, s) -> Ast.Return (Option.map expr e, s)
  | Ast.If (c, a, b, s) -> Ast.If (expr c, stmts a, Option.map stmts b, s)
  | Ast.While (label, c, body, s) -> Ast.While (label, expr c, stmts body, s)
  | (Ast.Break _ | Ast.Continue _) as st -> st
  | Ast.Defer (body, s) -> Ast.Defer (stmts body, s)
  | Ast.Expr_stmt (e, s1, s2) -> Ast.Expr_stmt (expr e, s1, s2)
  | Ast.Block (body, s) -> Ast.Block (stmts body, s)
  | Ast.For (label, init, c, step, body, s) ->
      Ast.For
        (label, Option.map stmt init, Option.map expr c, Option.map stmt step, stmts body, s)
  | Ast.Switch (e, arms, default, s) ->
      Ast.Switch
        ( expr e,
          List.map (fun (labels, body) -> (List.map expr labels, stmts body)) arms,
          Option.map stmts default,
          s )
  | Ast.Const_for c ->
      Ast.Const_for
        { c with ty = ty c.ty; low = expr c.low; high = expr c.high; body = stmts c.body }
  | Ast.Const_copy c -> Ast.Const_copy { c with ty = ty c.ty; body = stmts c.body }
  | Ast.Fail (e, s) -> Ast.Fail (expr e, s)
  | Ast.Errdefer (binding, body, s) -> Ast.Errdefer (binding, stmts body, s)
  | Ast.Catch_stmt c ->
      let target =
        match c.target with
        | Ast.Catch_let l -> Ast.Catch_let { l with ty = ty l.ty }
        | Ast.Catch_assign t -> Ast.Catch_assign (target t)
        | Ast.Catch_discard -> Ast.Catch_discard
      in
      Ast.Catch_stmt { c with target; call = expr c.call; body = stmts c.body }

and stmts body = List.map stmt body

let item = function
  | (Ast.Use _ | Ast.Pkg _ | Ast.Opaque _ | Ast.Enum _) as i -> i
  | Ast.Const c -> Ast.Const { c with ty = ty c.ty; value = expr c.value }
  | Ast.Global g -> Ast.Global { g with ty = ty g.ty; init = Option.map expr g.init }
  | Ast.Struct s ->
      Ast.Struct
        { s with fields = List.map (fun (f : Ast.field) -> { f with ty = ty f.ty }) s.fields }
  | Ast.Error_decl e ->
      Ast.Error_decl
        { e with names = List.map (fun (n, s, v) -> (n, s, Option.map expr v)) e.names }
  | Ast.Func f ->
      Ast.Func
        {
          f with
          params = List.map (fun (p : Ast.param) -> { p with ty = ty p.ty }) f.params;
          ret = ty f.ret;
          generic_params =
            List.map
              (function
                | Ast.Const_param cp -> Ast.Const_param { cp with ty = ty cp.ty }
                | p -> p)
              f.generic_params;
          body =
            (match f.body with
            | Ast.Declaration -> Ast.Declaration
            | Ast.Statements body -> Ast.Statements (stmts body));
        }

(* the constant `Name.Member` *)
let member_const enum kind (member, span) v =
  let enum_ty = Ast.Named_type (enum, span) in
  let literal = Ast.Int_lit (value_text kind v, span) in
  let literal =
    if v < 0L && signed kind then
      Ast.Unary (Ast.Neg, Ast.Int_lit (Printf.sprintf "%Lu" (Int64.neg v), span), span)
    else literal
  in
  Ast.Const
    {
      name = member_name enum member;
      name_span = span;
      ty_span = span;
      ty = enum_ty;
      value =
        Ast.Call
          ( Ast.Generic_args (Ast.Ident ("enum_from", span), [ Ast.Type_arg enum_ty ], span),
            [ literal ],
            span );
      span;
      extent = span;
    }

let desugar (program : Ast.program) =
  Stdlib.Hashtbl.reset Hir.enums;
  let* () =
    Result_list.iter
      (function
        | Ast.Enum { name; name_span; repr; repr_span; members; _ } ->
            let* d = check_enum name repr repr_span name_span members in
            if Stdlib.Hashtbl.mem Hir.enums name then
              error name_span (Printf.sprintf "duplicate enum `%s`" name)
            else (
              Stdlib.Hashtbl.replace Hir.enums name d;
              Ok ())
        | _ -> Ok ())
      program.Ast.items
  in
  if Stdlib.Hashtbl.length Hir.enums = 0 then Ok program
  else
    try
      let items =
        List.concat_map
          (function
            | Ast.Enum { name; members; _ } as enum ->
                let d = Option.get (Hir.enum_def name) in
                enum
                :: List.map2
                     (fun (member, span, _) (_, v) ->
                       member_const name d.Hir.enum_kind (member, span) v)
                     members d.Hir.members
            | i -> [ item i ])
          program.Ast.items
      in
      Ok { Ast.items }
    with Bad diagnostics -> Error diagnostics
