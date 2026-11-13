(* Enum erasure: before lowering, every enum type becomes its underlying
   integer type. An enum is a distinct type only for sema; in memory, in
   registers and in the ABI it is exactly its integer type, so lowering never
   sees [Hir.Enum]. The enum builtins keep the enum's name, which lowering
   uses to look up the members. *)

open Hir

let rec ty = function
  | Enum (_, k) -> Int k
  | Array (n, t) -> Array (n, ty t)
  | Vec (n, t) -> Vec (n, ty t)
  | (Bool | Int _ | Float _ | Addr | Handle _ | Struct _ | Opaque _ | Void | Err) as t
    ->
      t

let local (l : local) = { l with ty = ty l.ty }
let zero_init { zero_ty } = { zero_ty = ty zero_ty }

let builtin = function
  | Atomic (op, order, t) -> Atomic (op, order, ty t)
  | Cvt t -> Cvt (ty t)
  | Volatile_load t -> Volatile_load (ty t)
  | Simd_load (t, k) -> Simd_load (ty t, k)
  | b -> b

let target = function User name -> User name | Builtin b -> Builtin (builtin b)

let rec expr = function
  | EInt (v, t, s) -> EInt (v, ty t, s)
  | EFloat (v, t, s) -> EFloat (v, ty t, s)
  | EBool _ as e -> e
  | EVector (vs, t, s) -> EVector (vs, ty t, s)
  | Vector_lit (es, t, s) -> Vector_lit (List.map expr es, ty t, s)
  | Null (t, s) -> Null (ty t, s)
  | EString _ as e -> e
  | Local (l, s) -> Local (local l, s)
  | Global (name, t, s) -> Global (name, ty t, s)
  | Unary (op, e, t, s) -> Unary (op, expr e, ty t, s)
  | Binary (op, a, b, t, s) -> Binary (op, expr a, expr b, ty t, s)
  (* `enum_value(e)` only retypes its operand *)
  | Call (Builtin Enum_value, [ e ], _, _) -> expr e
  | Call (callee, args, t, s) -> Call (target callee, List.map expr args, ty t, s)
  | Indirect_call (f, args, t, s) -> Indirect_call (expr f, List.map expr args, ty t, s)
  | Cast (k, e, t, s) -> Cast (k, expr e, ty t, s)
  | Index (a, i, t, s) -> Index (expr a, expr i, ty t, s)
  | Field (e, name, t, offset, s) -> Field (expr e, name, ty t, offset, s)
  | Raw_select (p, o, t, s) -> Raw_select (expr p, expr o, ty t, s)
  | Address (e, t, s) -> Address (expr e, ty t, s)
  | Function_address _ as e -> e
  | Sizeof (t, n, s) -> Sizeof (ty t, n, s)
  | Alignof (t, n, s) -> Alignof (ty t, n, s)
  | Offsetof (t, name, n, s) -> Offsetof (ty t, name, n, s)
  | Splat (e, t, s) -> Splat (expr e, ty t, s)
  | Ternary (c, a, b, t, s) -> Ternary (expr c, expr a, expr b, ty t, s)
  | Const_array (name, t, s) -> Const_array (name, ty t, s)
  | Try (e, t, s) -> Try (expr e, ty t, s)
  | Catch_value (e, fallback, t, s) -> Catch_value (expr e, expr fallback, ty t, s)
  | Capture (e, l, t, s) -> Capture (expr e, local l, ty t, s)
  | Construct (l, c, s) -> Construct (local l, construction c, s)

and construction = function
  | Init_value e -> Init_value (expr e)
  | Init_zero (z, b, s) -> Init_zero (zero_init z, b, s)
  | Init_omitted (z, s) -> Init_omitted (zero_init z, s)
  | Init_aggregate (t, b, cs, s) -> Init_aggregate (ty t, b, List.map construction cs, s)

let assign_target = function
  | ALocal l -> ALocal (local l)
  | AGlobal (name, t) -> AGlobal (name, ty t)
  | ARaw (p, o, t) -> ARaw (expr p, expr o, ty t)
  | AIndex (a, i) -> AIndex (expr a, expr i)
  | AField (e, name, offset) -> AField (expr e, name, offset)

let rec stmt = function
  | Let (l, e, s) -> Let (local l, Option.map expr e, s)
  | Let_construct (l, c, s) -> Let_construct (local l, construction c, s)
  | View (l, e, s) -> View (local l, expr e, s)
  | Copy (a, b, t, v, s) -> Copy (expr a, expr b, ty t, v, s)
  | Volatile_store (t, a, b, s) -> Volatile_store (ty t, expr a, expr b, s)
  | Simd_store (t, k, es, s) -> Simd_store (ty t, k, List.map expr es, s)
  | Assign (t, e, s) -> Assign (assign_target t, expr e, s)
  | Compound_assign (t, op, e, vt, s) ->
      Compound_assign (assign_target t, op, expr e, ty vt, s)
  | Return (e, s) -> Return (Option.map expr e, s)
  | If (c, a, b, s) -> If (expr c, stmts a, Option.map stmts b, s)
  | While (label, c, body, s) -> While (label, expr c, stmts body, s)
  | For (label, init, c, step, body, s) ->
      For
        ( label,
          Option.map stmt init,
          Option.map expr c,
          Option.map stmt step,
          stmts body,
          s )
  | Switch (e, arms, default, s) ->
      Switch
        ( expr e,
          List.map (fun (labels, body) -> (List.map expr labels, stmts body)) arms,
          Option.map stmts default,
          s )
  | (Break _ | Continue _) as st -> st
  | Defer (body, s) -> Defer (stmts body, s)
  | Expr (e, s) -> Expr (expr e, s)
  | Block (body, s) -> Block (stmts body, s)
  | Fail (e, s) -> Fail (expr e, s)
  | Errdefer (l, body, s) -> Errdefer (Option.map local l, stmts body, s)

and stmts body = List.map stmt body

let func (f : func) =
  {
    f with
    params = List.map local f.params;
    ret = ty f.ret;
    body =
      (match f.body with
      | Declaration -> Declaration
      | Statements body -> Statements (stmts body));
  }

let program (p : program) =
  {
    p with
    structs =
      List.map
        (fun (d : struct_def) ->
          { d with fields = List.map (fun (f : field) -> { f with ty = ty f.ty }) d.fields })
        p.structs;
    consts = List.map (fun (c : const_def) -> { c with ty = ty c.ty }) p.consts;
    const_arrays = List.map (fun (c : const_arr_def) -> { c with ty = ty c.ty }) p.const_arrays;
    globals = List.map (fun (g : global) -> { g with ty = ty g.ty }) p.globals;
    funcs = List.map func p.funcs;
  }
