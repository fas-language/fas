(* The constant-time rules (LANGUAGE.md "Secrets", "Fas v2 decisions.md"
   "Promise: constant time"), checked on the Hir program after Sema.

   Hir types carry no secrecy; it lives beside them: [Hir.local.secret], the
   access secrecy of a raw select or raw assignment, [Hir.secret_fields] and
   [Hir.secret_results]. This pass computes the secrecy of every value from
   those sources and refuses each way a secret could reach timing or a public
   place:

   - a secret condition (`if`, `while`, `for`, `switch`, an if-expression,
     `&&`, `||`, `assert`);
   - a secret index, offset or address;
   - `/` and `%` on a secret; checked arithmetic on a secret;
   - a secret float (`cvt` and bitcasts to a float);
   - the builtins with no constant-time lowering yet: `popcount clz ctz`,
     lane indexing, `permute shuffle compress expand`, reductions, `any`
     `all`, gathers and masked forms;
   - a secret passed to C (an `extern "C"` declaration with a secret
     parameter or result, a public parameter of C), to a syscall, to an
     atomic or volatile access;
   - a secret stored into a public place (a local, global, field, element,
     raw access) or passed to a public parameter, an indirect call included;
   - taking the address of a function with a secret result (an indirect call
     would give it back as public).

   Secrecy is tracked in values and typed accesses, not in memory: reading
   through an untyped `addr` is public, as the decision says.

   Errors in a generic instantiation name it. Every match below is
   exhaustive, so a new Hir form has to decide its rule. *)

type callee = {
  display : string;
  params : Hir.secrecy list;
  extern_c : bool;
  result : Hir.secrecy;
}

type context = {
  prefix : string;
  result : Hir.secrecy;
  callees : (string, callee) Hashtbl.t;
  structs : (string, Hir.struct_def) Hashtbl.t;
  errors : Diag.t list ref;
  touches : bool ref;  (** the function handles a secret (Hir.secret_funcs) *)
}

let is_secret s = s <> Hir.Public

let rec join (a : Hir.secrecy) (b : Hir.secrecy) : Hir.secrecy =
  match (a, b) with
  | Public, s | s, Public -> s
  | Secret, _ | _, Secret -> Secret
  | Elements a, Elements b -> Elements (join a b)

(* may a value of secrecy [a] be stored where [b] is expected? *)
let rec flows_to (a : Hir.secrecy) (b : Hir.secrecy) =
  match (a, b) with
  | Public, _ | _, Secret -> true
  | Elements a, Elements b -> flows_to a b
  | Elements a, Public -> not (is_secret a)
  | Secret, (Public | Elements _) -> false

let err c span message = c.errors := Diag.error span (c.prefix ^ message) :: !(c.errors)

let builtin_name : Hir.builtin -> string = function
  | Atomic _ -> "an atomic operation"
  | Rotl -> "`rotl`"
  | Rotr -> "`rotr`"
  | Popcount -> "`popcount`"
  | Bswap -> "`bswap`"
  | Ctz -> "`ctz`"
  | Clz -> "`clz`"
  | Add_sat -> "`add_sat`"
  | Sub_sat -> "`sub_sat`"
  | Mul_hi -> "`mul_hi`"
  | Any -> "`any`"
  | All -> "`all`"
  | Select -> "`select`"
  | Shuffle -> "`shuffle`"
  | Permute -> "`permute`"
  | Reduce_sum -> "`reduce_sum`"
  | Reduce_min -> "`reduce_min`"
  | Reduce_max -> "`reduce_max`"
  | Reduce_and -> "`reduce_and`"
  | Reduce_or -> "`reduce_or`"
  | Reduce_xor -> "`reduce_xor`"
  | Compress -> "`compress`"
  | Expand -> "`expand`"
  | Addr_bits -> "`addr_bits`"
  | Syscall -> "`syscall`"
  | Addr_from_bits -> "`addr_from_bits`"
  | Cvt _ -> "`cvt`"
  | Float_math _ -> "a float builtin"
  | Handle_addr -> "`handle_addr`"
  | Handle_from_addr _ -> "`handle_from_addr`"
  | Volatile_load _ -> "`volatile_load`"
  | Simd_load _ -> "a gather or masked load"
  | Errno_err -> "`errno_err`"
  | Err_name -> "`err_name`"
  | Enum_value -> "`enum_value`"
  | Enum_from _ -> "`enum_from`"
  | Enum_is _ -> "`enum_is`"
  | Enum_name _ -> "`enum_name`"
  | Trap -> "`unreachable`"
  | Assert -> "`assert`"
  | Checked _ -> "checked arithmetic"
  | Declassify -> "`declassify`"
  | Wipe -> "`wipe`"

let rec value c (e : Hir.expr) : Hir.secrecy =
  match e with
  | EInt _ | EFloat _ | EBool _ | EVector _ | Null _ | EString _ | Global _ | Sizeof _
  | Alignof _ | Offsetof _ | Const_array _ ->
      Public
  | Function_address (name, span) ->
      (match Hashtbl.find_opt c.callees name with
      | Some { result; display; _ } when is_secret result ->
          err c span
            (Printf.sprintf
               "`%s` returns a secret, so it can't be called through a function value: \
                an indirect call's result is public"
               display)
      | _ -> ());
      Public
  | Local ({ ty = Addr; _ }, _) -> Public
  | Local (local, _) -> local.secret
  | Vector_lit (elements, _, _) ->
      List.fold_left (fun s x -> join s (value c x)) Public elements
  | Unary (_, x, _, _) | Splat (x, _, _) | Try (x, _, _) -> value c x
  | Capture (x, local, _, span) ->
      let s = value c x in
      if is_secret local.secret then err c span "an error value is public";
      s
  | Catch_value (x, fallback, _, _) -> join (value c x) (value c fallback)
  | Binary ((And | Or), x, y, _, _) ->
      condition c "`&&` and `||` branch, so their operands" x;
      condition c "`&&` and `||` branch, so their operands" y;
      Public
  | Binary (((Div | Rem) as op), x, y, _, span) ->
      let s = join (value c x) (value c y) in
      if is_secret s then
        err c span
          (Printf.sprintf "`%s` on a secret: division takes data-dependent time"
             (if op = Div then "/" else "%"));
      s
  | Binary (_, x, y, _, _) -> join (value c x) (value c y)
  | Ternary (cond, x, y, _, _) ->
      condition c "an if-expression branches, so its condition" cond;
      join (value c x) (value c y)
  | Cast (_, x, ty, span) ->
      let s = value c x in
      (match ty with
      | Float _ when is_secret s -> err c span "a secret can't become a float"
      | Addr when is_secret s -> err c span "a secret can't become an address"
      | _ -> ());
      s
  | Index (base, index, _, span) -> index_secrecy c base index span
  | Field (base, field, _, _, _) -> field_secrecy c base field
  | Raw_select (base, offset, _, secrecy, span) ->
      public c span "the base of a raw access is an address and" base;
      public c span "a secret offset would make a secret address;" offset;
      secrecy
  | Address (x, _, _) ->
      ignore (value c x);
      Public
  | Construct (local, construction, _) ->
      construct c local.secret construction;
      local.secret
  | Call (User name, args, _, span) -> call c name args span
  | Call (Builtin b, args, _, span) -> builtin c b args span
  | Indirect_call (callee, args, _, span) ->
      public c span "a function value is an address and" callee;
      List.iter
        (fun arg ->
          if is_secret (value c arg) then
            err c (Hir.expr_span arg)
              "a secret argument to an indirect call: a function type's parameters are \
               public")
        args;
      Public

(* [what] completes "... must be public" *)
and public c _span what e =
  if is_secret (value c e) then
    err c (Hir.expr_span e) (Printf.sprintf "%s must be public" what)

and condition c what e = public c (Hir.expr_span e) what e

and index_secrecy c base index span =
  let s = value c base in
  public c span "an index addresses memory, so it" index;
  match Hir.expr_ty base with
  | Array _ -> Hir.element_secrecy s
  | Vec _ ->
      if is_secret s then
        err c span "lane indexing on a secret vector has no constant-time lowering yet";
      s
  | _ -> Public

and field_secrecy c base field =
  ignore (value c base);
  match Hir.expr_ty base with
  | Struct name | Handle name -> Hir.field_secrecy name field
  | _ -> Public

(* a value of secrecy [s] stored where [target] is expected *)
and store c span ~what s target =
  if is_secret s || is_secret target then c.touches := true;
  if not (flows_to s target) then
    err c span (Printf.sprintf "a secret stored into %s" what)

and construct c target (construction : Hir.construction) =
  match construction with
  | Init_value e -> store c (Hir.expr_span e) ~what:"a public place" (value c e) target
  | Init_zero _ | Init_omitted _ -> ()
  | Init_aggregate (ty, _, entries, _) -> (
      match ty with
      | Struct name ->
          let fields =
            match Hashtbl.find_opt c.structs name with
            | Some def -> def.fields
            | None -> []
          in
          List.iteri
            (fun i entry ->
              match List.nth_opt fields i with
              | Some (f : Hir.field) ->
                  construct c (Hir.field_secrecy name f.name) entry
              | None -> construct c Public entry)
            entries
      | Array _ | Vec _ -> List.iter (construct c (Hir.element_secrecy target)) entries
      | _ -> List.iter (construct c target) entries)

and call c name args span =
  let callee =
    Option.value
      ~default:{ display = name; params = []; extern_c = false; result = Public }
      (Hashtbl.find_opt c.callees name)
  in
  List.iteri
    (fun i arg ->
      let s = value c arg in
      let param = Option.value ~default:Hir.Public (List.nth_opt callee.params i) in
      if not (flows_to s param) then
        if callee.extern_c then
          err c (Hir.expr_span arg)
            (Printf.sprintf "a secret passed to the C function `%s`" callee.display)
        else
          err c (Hir.expr_span arg)
            (Printf.sprintf
               "argument %d of `%s` is secret, but the parameter is public; declare it \
                `secret[T]`%s"
               (i + 1) callee.display
               (if callee.display <> name then
                  " (a generic's type argument is inferred public; write it, as in \
                   `f[secret[u32]](x)`)"
                else "")))
    args;
  ignore span;
  callee.result

and builtin c (b : Hir.builtin) args span =
  let secrets = List.map (value c) args in
  let joined = List.fold_left join Hir.Public secrets in
  let refuse why = if is_secret joined then err c span why in
  let no_lowering () =
    refuse
      (Printf.sprintf "%s on a secret has no constant-time lowering yet"
         (builtin_name b));
    Hir.Public
  in
  match b with
  | Select -> (
      match args with
      | [ _; yes; _ ] when is_secret joined -> (
          match Hir.expr_ty yes with
          | Float _ | Addr | Handle _ ->
              err c span
                "a `select` with a secret operand can't give a float or an address";
              Public
          | _ -> joined)
      | _ -> joined)
  | Rotl | Rotr | Bswap | Add_sat | Sub_sat | Mul_hi -> joined
  | Declassify -> Public
  | Popcount | Ctz | Clz | Permute | Shuffle | Compress | Expand | Any | All
  | Reduce_sum | Reduce_min | Reduce_max | Reduce_and | Reduce_or | Reduce_xor
  | Simd_load _ ->
      no_lowering ()
  | Cvt _ | Float_math _ ->
      refuse "a secret can't become a float";
      Public
  | Addr_from_bits ->
      refuse "a secret can't become an address";
      Public
  | Syscall ->
      refuse "a secret passed to a syscall: the kernel is not constant time";
      Public
  | Atomic _ | Volatile_load _ ->
      refuse
        (Printf.sprintf "a secret in %s: it is a public memory access" (builtin_name b));
      Public
  | Checked _ ->
      refuse "checked arithmetic on a secret branches on its overflow";
      Public
  | Assert | Trap ->
      refuse
        (Printf.sprintf "%s branches, so its condition must be public" (builtin_name b));
      Public
  | Addr_bits | Handle_addr | Handle_from_addr _ | Errno_err | Err_name | Enum_value
  | Enum_from _ | Enum_is _ | Enum_name _ | Wipe ->
      refuse (Printf.sprintf "the arguments of %s must be public" (builtin_name b));
      Public

let target_secrecy c (target : Hir.assign_target) span : string * Hir.secrecy =
  match target with
  | ALocal local ->
      ( Printf.sprintf "the public local `%s`; declare it `secret[T]`" local.name,
        local.secret )
  | AGlobal (name, _) ->
      (Printf.sprintf "the global `%s`; globals are public" name, Public)
  | ARaw (base, offset, _, secrecy) ->
      public c span "the base of a raw access is an address and" base;
      public c span "a secret offset would make a secret address;" offset;
      ( "a public typed access; write `p[secret[T], i]` or declare the field secret",
        secrecy )
  | AIndex (base, index) ->
      ( "a public element; declare the elements `secret[T]`",
        index_secrecy c base index span )
  | AField (base, field, _) ->
      ( Printf.sprintf "the public field `%s`; declare it `secret[T]`" field,
        field_secrecy c base field )

(* does a value of this type hold secret bytes (a secret field, at any
   depth)? *)
let rec type_has_secret c seen (t : Hir.ty) =
  match t with
  | Struct name when not (List.mem name seen) -> (
      match Hashtbl.find_opt c.structs name with
      | Some (d : Hir.struct_def) ->
          List.exists
            (fun (fd : Hir.field) ->
              is_secret (Hir.field_secrecy name fd.name)
              || type_has_secret c (name :: seen) fd.ty)
            d.fields
      | None -> false)
  | Array (_, t) -> type_has_secret c seen t
  | _ -> false

let local_secrets c (local : Hir.local) =
  if is_secret local.secret || type_has_secret c [] local.ty then c.touches := true

let rec stmt c (s : Hir.stmt) =
  match s with
  | Let (local, None, _) -> local_secrets c local
  | Let (local, Some e, _) ->
      local_secrets c local;
      store c (Hir.expr_span e)
        ~what:
          (Printf.sprintf "the public local `%s`; declare it `secret[T]`" local.name)
        (value c e) local.secret
  | Let_construct (local, construction, _) ->
      local_secrets c local;
      construct c local.secret construction
  | View (_, e, _) -> ignore (value c e)
  | Copy (destination, source, _, _, span) ->
      let target = value c destination in
      store c span ~what:"a public place" (value c source) target
  | Volatile_store (_, p, v, span) ->
      public c span "a volatile store's address" p;
      public c span "a volatile store is a public memory access, so its value" v
  | Simd_store (_, _, args, span) ->
      List.iter
        (public c span
           "a scatter or masked store has no constant-time lowering yet; its operands")
        args
  | Assign (target, e, span) ->
      let what, secrecy = target_secrecy c target span in
      store c (Hir.expr_span e) ~what (value c e) secrecy
  | Compound_assign (target, op, e, _, span) ->
      let what, secrecy = target_secrecy c target span in
      let s = join secrecy (value c e) in
      (match op with
      | (Div | Rem) when is_secret s ->
          err c span "`/` and `%` on a secret: division takes data-dependent time"
      | _ -> ());
      store c span ~what s secrecy
  | Return (None, _) -> ()
  | Return (Some e, _) ->
      let s = value c e in
      if is_secret s then c.touches := true;
      if not (flows_to s c.result) then
        err c (Hir.expr_span e)
          "a secret returned as a public result; declare the result `secret[T]`"
  | If (cond, yes, no, _) ->
      condition c "`if` branches, so its condition" cond;
      block c yes;
      Option.iter (block c) no
  | While (_, cond, body, _) ->
      condition c "`while` branches, so its condition" cond;
      block c body
  | For (_, init, cond, step, body, _) ->
      Option.iter (stmt c) init;
      Option.iter (condition c "`for` branches, so its condition") cond;
      Option.iter (stmt c) step;
      block c body
  | Switch (x, cases, default, _) ->
      condition c "`switch` branches, so its value" x;
      List.iter (fun (_, body) -> block c body) cases;
      Option.iter (block c) default
  | Break _ | Continue _ -> ()
  | Defer (body, _) | Block (body, _) | Errdefer (_, body, _) -> block c body
  | Expr (e, _) -> if is_secret (value c e) then c.touches := true
  | Fail (e, span) -> public c span "an error value" e

and block c body = List.iter (stmt c) body

let check (program : Hir.program) =
  let callees = Hashtbl.create 64 in
  List.iter
    (fun (f : Hir.func) ->
      Hashtbl.replace callees f.name
        {
          display = Option.value ~default:f.name f.display;
          params = List.map (fun (p : Hir.local) -> p.secret) f.params;
          extern_c = f.linkage = External_c && f.body = Declaration;
          result =
            Option.value ~default:Hir.Public
              (Stdlib.Hashtbl.find_opt Hir.secret_results f.name);
        })
    program.funcs;
  let structs = Hashtbl.create 64 in
  List.iter
    (fun (d : Hir.struct_def) -> Hashtbl.replace structs d.name d)
    program.structs;
  let errors = ref [] in
  List.iter
    (fun (f : Hir.func) ->
      let callee = Hashtbl.find callees f.name in
      let prefix =
        match f.display with
        | Some display when display <> f.name -> Printf.sprintf "in `%s`: " display
        | _ -> ""
      in
      let c =
        {
          prefix;
          result = callee.result;
          callees;
          structs;
          errors;
          touches = ref false;
        }
      in
      List.iter (local_secrets c) f.params;
      if is_secret callee.result then c.touches := true;
      match f.body with
      | Declaration ->
          if
            callee.extern_c
            && (List.exists is_secret callee.params || is_secret callee.result)
          then
            err c f.span
              (Printf.sprintf
                 "the C function `%s` can't take or return a secret: C code is not \
                  constant time"
                 f.name)
      | Statements body ->
          block c body;
          if !(c.touches) then Hashtbl.replace Hir.secret_funcs f.name ())
    program.funcs;
  match List.rev !errors with [] -> Ok () | errors -> Error errors
