(* Converter from the old [Ir] (an in-memory form of LLVM text) to Fir.

   This is the migration bridge (SPEC.md, "Migration"): until lower.ml
   emits Fir directly, every Fir consumer starts here.

   The main translations:
   - Phis become block parameters. Each predecessor passes its incoming
     values as branch arguments.
   - Typed GEPs become [ptradd] chains: constant indices fold into the
     displacement, and each variable index becomes one scaled [ptradd].
   - Aggregate SSA values (structs and arrays loaded, passed or returned by
     value) become pointers to stack copies. An aggregate load copies into a
     fresh entry-block slot. An aggregate store copies out of it, or clears
     the destination for zero/undef.
   - [llvm.*] intrinsic calls become Fir opcodes. Lifetime markers are
     dropped.
   - [Location (line, col)] markers become the location ids of the
     instructions that follow them.
   - [undef] becomes [poison]. *)

exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

open Fir

(* ---------- layouts of old types ---------- *)

type layout_ctx = { structs : (string, Ir.struct_def) Hashtbl.t }

let rec layout lc (t : Ir.ty) =
  match t with
  | Ir.I1 | Ir.I8 -> (1, 1)
  | Ir.I16 -> (2, 2)
  | Ir.I32 | Ir.F32 -> (4, 4)
  | Ir.I64 | Ir.F64 | Ir.Pointer _ -> (8, 8)
  | Ir.I128 -> (16, 16)
  | Ir.Void -> (0, 1)
  | Ir.Vector (n, e) -> (
      let bits =
        match e with
        | Ir.I1 -> 1 | Ir.I8 -> 8 | Ir.I16 -> 16 | Ir.I32 | Ir.F32 -> 32
        | Ir.I64 | Ir.F64 | Ir.Pointer _ -> 64 | Ir.I128 -> 128
        | _ -> unsupported "vector of aggregates"
      in
      match Target_layout.vector (Target_layout.current ()) n bits with
      | Ok (size, align) -> (size, align)
      | Error e -> unsupported "vector layout: %s" e)
  | Ir.Array (n, e) ->
      let s, a = layout lc e in
      (n * s, a)
  | Ir.Struct name -> (
      match Hashtbl.find_opt lc.structs name with
      | None -> unsupported "unknown struct %s" name
      | Some d ->
          (* the front end lays fields out back to back, with explicit
             padding fields, then tail padding *)
          let size, align =
            List.fold_left
              (fun (s, a) f ->
                let fs, fa = layout lc f in
                (s + fs, max a fa))
              (0, 1) d.fields
          in
          (size + d.tail_padding, align))

let size_of_old lc t = fst (layout lc t)

let field_offset lc name index =
  match Hashtbl.find_opt lc.structs name with
  | None -> unsupported "unknown struct %s" name
  | Some d ->
      let rec go n acc = function
        | [] -> unsupported "struct field index out of range"
        | f :: rest -> if n = index then (acc, f) else go (n + 1) (acc + size_of_old lc f) rest
      in
      go 0 0 d.fields

let is_aggregate = function Ir.Struct _ | Ir.Array _ -> true | _ -> false

let rec ty_of_old (t : Ir.ty) =
  match t with
  | Ir.I1 -> i1
  | Ir.I8 -> i8
  | Ir.I16 -> i16
  | Ir.I32 -> i32
  | Ir.I64 -> i64
  | Ir.I128 -> i128
  | Ir.F32 -> F32
  | Ir.F64 -> F64
  | Ir.Pointer _ -> ptr
  | Ir.Vector (n, e) -> Vec (n, ty_of_old e)
  (* aggregates live in memory: their SSA stand-in is a pointer *)
  | Ir.Struct _ | Ir.Array _ -> ptr
  | Ir.Void -> unsupported "void value"

let abi_of_old lc (t : Ir.ty) (e : Ir.extension) =
  if is_aggregate t then
    let size, align = layout lc t in
    Agg { size; align; fields = [] }
  else
    match e with
    | Ir.No_extension -> Scalar (ty_of_old t)
    | Ir.Zero_extension -> Ext (ty_of_old t, Zx)
    | Ir.Sign_extension -> Ext (ty_of_old t, Sx)

let results_of_old lc (t : Ir.ty) e = if t = Ir.Void then [] else [ abi_of_old lc t e ]

(* ---------- functions ---------- *)

type fctx = {
  lc : layout_ctx;
  m : modul;
  f : func;
  old : Ir.func;
  locals : (int, value) Hashtbl.t;  (** old %v ids, or placeholders *)
  defined : (int, unit) Hashtbl.t;
  params : (string, value) Hashtbl.t;
  blocks : (int, block) Hashtbl.t;  (** old block id -> Fir block *)
  phis : (int, (Ir.ty * (Ir.value * int) list) list) Hashtbl.t;
      (** old block id -> its phis, in order *)
  mutable entry_temps : inst list;  (** aggregate slots, placed in the entry block *)
  mutable loc : int;
  locs : (string * int * int, int) Hashtbl.t;
  variadic : (string, int) Hashtbl.t;  (** variadic function -> fixed parameter count *)
}

let define cx id v =
  if Hashtbl.mem cx.defined id then unsupported "%%v%d defined twice" id;
  Hashtbl.replace cx.defined id ();
  (match Hashtbl.find_opt cx.locals id with
  | Some placeholder -> Vec.set cx.f.defs placeholder (Alias v)
  | None -> ());
  Hashtbl.replace cx.locals id v;
  if not (Hashtbl.mem cx.f.names v) then Hashtbl.replace cx.f.names v ("v" ^ string_of_int id)

let local cx id =
  match Hashtbl.find_opt cx.locals id with
  | Some v -> v
  | None ->
      let v = new_value cx.f Unset i1 in
      Hashtbl.replace cx.locals id v;
      v

(* An entry-block stack slot of [size] bytes *)
let temp_slot cx size align =
  let i = make_inst cx.f (Alloca (iconst cx.f i64 (Int64.of_int size), max 1 align)) [| ptr |] in
  cx.entry_temps <- i :: cx.entry_temps;
  (inst cx.f i).results.(0)

let emit1 cx b op ty = emit1 ~loc:cx.loc cx.f b op ty
let emit0 cx b op = emit0 ~loc:cx.loc cx.f b op

let rec value cx b (v : Ir.value) =
  let f = cx.f in
  match v with
  | Ir.Local (id, _) -> local cx id
  | Ir.Param (name, _) -> (
      match Hashtbl.find_opt cx.params name with
      | Some p -> p
      | None -> unsupported "unknown parameter %s" name)
  | Ir.Global (name, _) -> sym f name
  | Ir.Null t -> null f (ty_of_old t)
  | Ir.Undef t when is_aggregate t ->
      let size, align = layout cx.lc t in
      temp_slot cx size align
  | Ir.Zero t when is_aggregate t ->
      let size, align = layout cx.lc t in
      let s = temp_slot cx size align in
      emit0 cx b (Memset (false, s, iconst f i8 0L, iconst f i64 (Int64.of_int size)));
      s
  | Ir.Undef t -> poison f (ty_of_old t)
  | Ir.Zero t -> zero f (ty_of_old t)
  | Ir.Const (((Ir.F32 | Ir.F64) as t), bits) -> scalar_float f t bits
  | Ir.Const (t, n) -> iconst f (ty_of_old t) n
  | Ir.Const_vector ((Ir.Vector (_, e) as t), vals) ->
      let lanes =
        List.map
          (fun n -> match e with Ir.F32 | Ir.F64 -> scalar_float f e n | _ -> iconst f (ty_of_old e) n)
          vals
      in
      vconst f (ty_of_old t) (Array.of_list lanes)
  | Ir.Const_vector _ -> unsupported "vector constant of a non-vector type"

and scalar_float f t bits =
  match t with
  | Ir.F32 -> fconst_bits f F32 [| Int64.logand bits 0xffffffffL |]
  | _ -> fconst_bits f F64 [| bits |]

let ibin_of_old : Ir.binop -> [ `I of ibin | `F of fbin ] = function
  | Ir.Add -> `I Add | Ir.Sub -> `I Sub | Ir.Mul -> `I Mul | Ir.Sdiv -> `I Sdiv
  | Ir.Srem -> `I Srem | Ir.Udiv -> `I Udiv | Ir.Urem -> `I Urem | Ir.And -> `I And
  | Ir.Or -> `I Or | Ir.Xor -> `I Xor | Ir.Shl -> `I Shl | Ir.Lshr -> `I Lshr
  | Ir.Ashr -> `I Ashr | Ir.Fadd -> `F Fadd | Ir.Fsub -> `F Fsub | Ir.Fmul -> `F Fmul
  | Ir.Fdiv -> `F Fdiv

let cmp_of_old : Ir.cmp -> [ `I of icmp | `F of fcmp ] = function
  | Ir.Eq -> `I Eq | Ir.Ne -> `I Ne | Ir.Slt -> `I Slt | Ir.Sle -> `I Sle | Ir.Sgt -> `I Sgt
  | Ir.Sge -> `I Sge | Ir.Ult -> `I Ult | Ir.Ule -> `I Ule | Ir.Ugt -> `I Ugt | Ir.Uge -> `I Uge
  | Ir.Foeq -> `F Foeq | Ir.Fune -> `F Fune | Ir.Folt -> `F Folt | Ir.Fole -> `F Fole
  | Ir.Fogt -> `F Fogt | Ir.Foge -> `F Foge

let conv_of_old = function
  | "trunc" -> Trunc | "zext" -> Zext | "sext" -> Sext | "fptrunc" -> Fptrunc
  | "fpext" -> Fpext | "fptosi" -> Fptosi | "fptoui" -> Fptoui | "sitofp" -> Sitofp
  | "uitofp" -> Uitofp | "ptrtoint" -> Ptrtoint | "inttoptr" -> Inttoptr
  | "bitcast" -> Bitcast | k -> unsupported "cast %s" k

let starts p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* Converts an index value to i64 the way GEP does (sign extension) *)
let index64 cx b v =
  let x = value cx b v in
  match ty_of cx.f x with
  | Int 64 -> x
  | Int w when w < 64 -> (
      match int_value cx.f x with
      | Some n -> iconst cx.f i64 n
      | None -> emit1 cx b (Conv (Sext, x)) i64)
  | _ -> unsupported "GEP index wider than 64 bits"

(* A typed GEP as ptradds. Constant indices accumulate into [disp]; each
   variable index emits ptradd(cur, idx, scale, disp). *)
let gep cx b (ty : Ir.ty) p (idxs : Ir.gep_index list) =
  let cur = ref (value cx b p) and disp = ref 0 in
  let step size (idx : Ir.gep_index) =
    match idx with
    | Ir.Zero -> ()
    | Ir.Index v -> (
        let x = index64 cx b v in
        match int_value cx.f x with
        | Some n -> disp := !disp + (Int64.to_int n * size)
        | None ->
            cur := emit1 cx b (Ptradd (0, !cur, x, size, !disp)) ptr;
            disp := 0)
  in
  let rec walk (ty : Ir.ty) = function
    | [] -> ()
    | idx :: rest -> (
        match ty with
        | Ir.Struct name ->
            let n =
              match idx with
              | Ir.Zero -> 0
              | Ir.Index (Ir.Const (_, n)) -> Int64.to_int n
              | Ir.Index _ -> unsupported "struct field index is not constant"
            in
            let off, fty = field_offset cx.lc name n in
            disp := !disp + off;
            walk fty rest
        | Ir.Array (_, e) | Ir.Vector (_, e) ->
            step (size_of_old cx.lc e) idx;
            walk e rest
        | _ -> unsupported "gep into a scalar")
  in
  (match idxs with
  | [] -> ()
  | first :: rest ->
      step (size_of_old cx.lc ty) first;
      walk ty rest);
  if !disp = 0 then !cur else emit1 cx b (Ptradd (0, !cur, iconst cx.f i64 0L, 1, !disp)) ptr

let const_int v = match v with Ir.Const (_, n) -> Some n | _ -> None
let rt_ty (t : Ir.ty) = ty_of_old t

(* The result of an [llvm.*] call, or None when the intrinsic is dropped *)
let intrinsic cx b name (ret : Ir.ty) (args : (Ir.ty * Ir.extension * Ir.value) list) =
  let a k = let _, _, v = List.nth args k in value cx b v in
  let r = if ret = Ir.Void then i1 else rt_ty ret in
  let one op = Some (emit1 cx b op r) in
  if starts "llvm.lifetime." name then None
  else if name = "llvm.trap" then (emit0 cx b Trap; None)
  else if starts "llvm.vector.reduce.or." name then one (Reduce (Red_or, a 0))
  else if starts "llvm.fptosi.sat." name then one (Conv (Fptosi_sat, a 0))
  else if starts "llvm.fptoui.sat." name then one (Conv (Fptoui_sat, a 0))
  else if starts "llvm.sqrt." name then one (Fun (Sqrt, 0, a 0))
  else if starts "llvm.floor." name then one (Fun (Floor, 0, a 0))
  else if starts "llvm.ceil." name then one (Fun (Ceil, 0, a 0))
  else if starts "llvm.roundeven." name then one (Fun (Roundeven, 0, a 0))
  else if starts "llvm.fabs." name then one (Fun (Fabs, 0, a 0))
  else if starts "llvm.fma." name then one (Fma (0, a 0, a 1, a 2))
  else if starts "llvm.fshl." name then one (Fsh (true, a 0, a 1, a 2))
  else if starts "llvm.fshr." name then one (Fsh (false, a 0, a 1, a 2))
  else if starts "llvm.ctpop." name then one (Iun (Popcnt, 0, a 0))
  else if starts "llvm.cttz." name || starts "llvm.ctlz." name then begin
    let _, _, zp = List.nth args 1 in
    let fl = match const_int zp with Some 0L -> 0 | Some _ -> f_zero_poison | None -> unsupported "%s with a variable flag" name in
    one (Iun ((if starts "llvm.cttz." name then Ctz else Clz), fl, a 0))
  end
  else if starts "llvm.uadd.sat." name then one (Ibin (Uadd_sat, 0, a 0, a 1))
  else if starts "llvm.sadd.sat." name then one (Ibin (Sadd_sat, 0, a 0, a 1))
  else if starts "llvm.usub.sat." name then one (Ibin (Usub_sat, 0, a 0, a 1))
  else if starts "llvm.ssub.sat." name then one (Ibin (Ssub_sat, 0, a 0, a 1))
  else if name = "llvm.fas.umulh" then one (Ibin (Umulh, 0, a 0, a 1))
  else if name = "llvm.fas.smulh" then one (Ibin (Smulh, 0, a 0, a 1))
  else if starts "llvm.experimental.vector.compress." name then begin
    (match List.nth args 2 with
    | _, _, (Ir.Zero _ | Ir.Undef _) -> ()
    | _ -> unsupported "vector.compress with a non-zero passthru");
    one (Compress (a 0, a 1))
  end
  else if starts "llvm.masked.load." name then begin
    let _, _, al = List.nth args 1 in
    let al = match const_int al with Some n -> Int64.to_int n | None -> unsupported "masked.load alignment" in
    one (Masked_load (al, a 0, a 2, a 3))
  end
  else if starts "llvm.masked.store." name then begin
    let _, _, al = List.nth args 2 in
    let al = match const_int al with Some n -> Int64.to_int n | None -> unsupported "masked.store alignment" in
    emit0 cx b (Masked_store (al, a 0, a 1, a 3));
    None
  end
  else unsupported "intrinsic %s" name

let mem align = plain_mem (max 1 align)
let vmem align = { (plain_mem (max 1 align)) with volatile = true }

let convert_call cx b result ext (ret : Ir.ty) callee args =
  let f = cx.f in
  let fixed =
    match callee with
    | Direct n -> Hashtbl.find_opt cx.variadic n
    | Indirect _ -> None
  in
  let nfixed = match fixed with Some k -> k | None -> List.length args in
  let param_abis = List.filteri (fun k _ -> k < nfixed) (List.map (fun (t, e, _) -> abi_of_old cx.lc t e) args) in
  let sig_ = { param_abis; result_abis = results_of_old cx.lc ret ext; variadic = fixed <> None } in
  let argv = Array.of_list (List.map (fun (_, _, v) -> value cx b v) args) in
  let tys = Array.of_list (List.map Fir_verify.abi_ty sig_.result_abis) in
  let rs = emit ~loc:cx.loc f b (Call { sig_; callee; args = argv; attrs = 0; va_aggs = [] }) tys in
  match (result, rs) with
  | Some id, [| r |] -> define cx id r
  | None, _ -> ()
  | Some _, _ -> unsupported "call result of a void function"

let convert_instr cx b (ins : Ir.instr) =
  let f = cx.f in
  let v = value cx b in
  match ins with
  | Ir.Location (line, col) ->
      let file = match cx.old.source with Some (file, _) -> file | None -> "" in
      let key = (file, line, col) in
      cx.loc <-
        (match Hashtbl.find_opt cx.locs key with
        | Some l -> l
        | None ->
            let l = add_loc cx.m file line col in
            Hashtbl.replace cx.locs key l;
            l)
  | Ir.Bin (id, op, t, x, y) -> (
      let r = rt_ty t in
      match ibin_of_old op with
      | `I k -> define cx id (emit1 cx b (Ibin (k, 0, v x, v y)) r)
      | `F k -> define cx id (emit1 cx b (Fbin (k, 0, v x, v y)) r))
  | Ir.Fneg (id, t, x) -> define cx id (emit1 cx b (Fun (Fneg, 0, v x)) (rt_ty t))
  | Ir.Cmp (id, c, t, x, y) -> (
      let r = with_elem (rt_ty t) i1 in
      match cmp_of_old c with
      | `I k -> define cx id (emit1 cx b (Icmp (k, v x, v y)) r)
      | `F k -> define cx id (emit1 cx b (Fcmp (k, 0, v x, v y)) r))
  | Ir.Alloca (id, t, align) ->
      let size = size_of_old cx.lc t in
      define cx id (emit1 cx b (Alloca (iconst f i64 (Int64.of_int size), max 1 align)) ptr)
  | Ir.Load (id, t, p, align) | Ir.Load_volatile (id, t, p, align) ->
      let volatile = match ins with Ir.Load_volatile _ -> true | _ -> false in
      if is_aggregate t then begin
        let size, al = layout cx.lc t in
        let s = temp_slot cx size (max al align) in
        emit0 cx b (Memcpy (volatile, s, v p, iconst f i64 (Int64.of_int size)));
        define cx id s
      end
      else define cx id (emit1 cx b (Load ((if volatile then vmem align else mem align), v p)) (rt_ty t))
  | Ir.Store (t, x, p, align) | Ir.Store_volatile (t, x, p, align) ->
      let volatile = match ins with Ir.Store_volatile _ -> true | _ -> false in
      if is_aggregate t then begin
        let size = iconst f i64 (Int64.of_int (size_of_old cx.lc t)) in
        match x with
        | Ir.Zero _ | Ir.Undef _ | Ir.Null _ -> emit0 cx b (Memset (volatile, v p, iconst f i8 0L, size))
        | _ -> emit0 cx b (Memcpy (volatile, v p, v x, size))
      end
      else emit0 cx b (Store ((if volatile then vmem align else mem align), v x, v p))
  | Ir.Gep (id, t, p, idxs) ->
      let r = gep cx b t p idxs in
      define cx id r
  | Ir.Cast (id, k, from, x, t) ->
      if is_aggregate from || is_aggregate t then unsupported "aggregate cast"
      else begin
        let x = v x in
        let k = conv_of_old k in
        let rt = rt_ty t in
        (* pointers are opaque: a pointer bitcast is the value itself, and
           so is any cast whose source already has the target type *)
        if k = Bitcast && ty_of f x = rt then define cx id x
        else define cx id (emit1 cx b (Conv (k, x)) rt)
      end
  | Ir.Call (result, _, ret, name, args) when Ir.is_atomic_name name -> (
      (* `__fas_atomic_*` pseudo-calls (Ir.atomic_name) become Fir atomics *)
      let op, order =
        match Ir.parse_atomic_name name with Some x -> x | None -> unsupported "atomic %s" name
      in
      let fo : ordering =
        match order with
        | Ir.Relaxed -> Relaxed
        | Ir.Acquire -> Acquire
        | Ir.Release -> Release
        | Ir.Acq_rel -> Acq_rel
        | Ir.Seq_cst -> Seq_cst
      in
      let a i = let _, _, x = List.nth args i in v x in
      let am t = { (mem (size_of_old cx.lc t)) with order = fo } in
      let def r = match result with Some id -> define cx id r | None -> () in
      match op with
      | Ir.A_fence -> emit0 cx b (Fence fo)
      | Ir.A_load -> def (emit1 cx b (Load (am ret, a 0)) (rt_ty ret))
      | Ir.A_store ->
          let t, _, x = List.nth args 1 in
          emit0 cx b (Store (am t, v x, a 0))
      | Ir.A_cas ->
          let fail : ordering =
            match Ir.cas_failure_order order with
            | Ir.Relaxed -> Relaxed
            | Ir.Acquire -> Acquire
            | _ -> Seq_cst
          in
          let rs =
            emit ~loc:cx.loc cx.f b
              (Cmpxchg { mem = am ret; fail; weak = false; p = a 0; expected = a 1; desired = a 2 })
              [| rt_ty ret; i1 |]
          in
          def rs.(0)
      | Ir.A_add | Ir.A_sub | Ir.A_and | Ir.A_or | Ir.A_xor | Ir.A_xchg ->
          let k =
            match op with
            | Ir.A_add -> Radd
            | Ir.A_sub -> Rsub
            | Ir.A_and -> Rand
            | Ir.A_or -> Ror
            | Ir.A_xor -> Rxor
            | _ -> Xchg
          in
          def (emit1 cx b (Rmw (k, am ret, a 0, a 1)) (rt_ty ret)))
  | Ir.Call (result, _, ret, name, args) when starts "llvm." name -> (
      match (intrinsic cx b name ret args, result) with
      | Some r, Some id -> define cx id r
      | _ -> ())
  | Ir.Call (result, ext, ret, name, args) -> convert_call cx b result ext ret (Direct name) args
  | Ir.Call_indirect (result, ext, ret, callee, args) ->
      convert_call cx b result ext ret (Indirect (v callee)) args
  | Ir.Phi _ -> ()  (* became block parameters *)
  | Ir.Select (id, c, x, y) ->
      if is_aggregate (Ir.value_ty x) then unsupported "aggregate select";
      define cx id (emit1 cx b (Select (v c, v x, v y)) (rt_ty (Ir.value_ty x)))
  | Ir.Extract (id, vt, x, l) ->
      let e = match vt with Ir.Vector (_, e) -> rt_ty e | _ -> unsupported "extract from a non-vector" in
      define cx id (emit1 cx b (Extract (v x, v l)) e)
  | Ir.Insert (id, vt, x, l, e) -> define cx id (emit1 cx b (Insert (v x, v e, v l)) (rt_ty vt))
  | Ir.Shuffle_zero (id, vt, x) ->
      let n = match vt with Ir.Vector (n, _) -> n | _ -> unsupported "shuffle of a non-vector" in
      let xv = v x in
      define cx id (emit1 cx b (Shuffle (xv, poison f (ty_of f xv), Array.make n 0)) (rt_ty vt))
  | Ir.Shufflevector (id, _, x, y, mask) ->
      let lanes =
        match mask with
        | Ir.Const_vector (_, l) -> Array.of_list (List.map Int64.to_int l)
        | Ir.Zero (Ir.Vector (n, _)) -> Array.make n 0
        | Ir.Undef (Ir.Vector (n, _)) -> Array.make n (-1)
        | _ -> unsupported "shufflevector with a variable mask"
      in
      let xv = v x in
      let e = elem (ty_of f xv) in
      define cx id (emit1 cx b (Shuffle (xv, v y, lanes)) (Vec (Array.length lanes, e)))
  | Ir.String_ptr (id, index, _) -> define cx id (sym f (".str." ^ string_of_int index))
  | Ir.Global_ptr (id, name, _) -> define cx id (sym f name)
  | Ir.Trap -> emit0 cx b Trap
  | Ir.Syscall (id, args) ->
      define cx id (emit1 cx b (Syscall (Array.of_list (List.map v args))) i64)

let convert_func lc m variadic (old : Ir.func) =
  let param_abis = List.map (fun (p : Ir.param) -> abi_of_old lc p.ty p.extension) old.params in
  let sig_ = { param_abis; result_abis = results_of_old lc old.ret old.ret_extension; variadic = old.variadic } in
  let linkage = match old.linkage with Ir.Internal -> Local | Ir.External -> Global in
  let defined = old.blocks <> [] in
  let f = new_func ~linkage ~defined old.name sig_ in
  f.display <- old.display;
  if defined then begin
    let cx =
      { lc; m; f; old; locals = Hashtbl.create 64; defined = Hashtbl.create 64; params = Hashtbl.create 8;
        blocks = Hashtbl.create 16; phis = Hashtbl.create 16; entry_temps = []; loc = 0;
        locs = Hashtbl.create 16; variadic }
    in
    (* blocks first, so branches and phis can refer to any of them; the
       first old block is the entry *)
    List.iter
      (fun (ob : Ir.block) ->
        let b = add_block ~name:("b" ^ string_of_int ob.id) f in
        Hashtbl.replace cx.blocks ob.id b)
      old.blocks;
    let entry_old = (List.hd old.blocks).id in
    List.iter
      (fun (p : Ir.param) ->
        let v = add_param f entry (Fir_verify.abi_ty (abi_of_old lc p.ty p.extension)) in
        Hashtbl.replace cx.params p.name v;
        Hashtbl.replace f.names v p.name)
      old.params;
    List.iter
      (fun (ob : Ir.block) ->
        let b = Hashtbl.find cx.blocks ob.id in
        let phis =
          List.filter_map
            (function
              | Ir.Phi (id, t, incoming) ->
                  if is_aggregate t then unsupported "aggregate phi";
                  if ob.id = entry_old then unsupported "phi in the entry block";
                  let p = add_param f b (rt_ty t) in
                  define cx id p;
                  Some (t, incoming)
              | _ -> None)
            ob.instrs
        in
        Hashtbl.replace cx.phis ob.id phis)
      old.blocks;
    List.iter
      (fun (ob : Ir.block) ->
        let b = Hashtbl.find cx.blocks ob.id in
        cx.loc <- 0;
        List.iter (convert_instr cx b) ob.instrs;
        let target dest =
          if dest = entry_old then unsupported "branch to the entry block";
          let fb = match Hashtbl.find_opt cx.blocks dest with Some x -> x | None -> unsupported "unknown block %d" dest in
          let args =
            List.map
              (fun (_, incoming) ->
                match List.assoc_opt ob.id (List.map (fun (v, p) -> (p, v)) incoming) with
                | Some v -> value cx b v
                | None -> unsupported "phi in b%d has no value for edge from b%d" dest ob.id)
              (Hashtbl.find cx.phis dest)
          in
          { dest = fb; args = Array.of_list args }
        in
        let term =
          match ob.terminator with
          | Ir.Ret None -> Ret [||]
          | Ir.Ret (Some (_, x)) -> Ret [| value cx b x |]
          | Ir.Br d -> Jump (target d)
          | Ir.CondBr (c, t, e) -> Br (value cx b c, target t, target e, -1)
          | Ir.Switch (t, x, cases, d) ->
              let w = int_width (rt_ty t) in
              let seen = Hashtbl.create 8 in
              (* LLVM rejects duplicate keys; keep the first like it would
                 have been written *)
              let cases =
                List.filter_map
                  (fun (k, dst) ->
                    let k = limbs_of_int64 w k in
                    if Hashtbl.mem seen k then None
                    else (
                      Hashtbl.replace seen k ();
                      Some (k, target dst)))
                  cases
              in
              Switch (value cx b x, target d, Array.of_list cases)
          | Ir.Unreachable -> Unreachable
        in
        set_term ~loc:cx.loc f b term)
      old.blocks;
    (* aggregate slots go first in the entry block *)
    if cx.entry_temps <> [] then begin
      let eb = block f entry in
      let insts = Vec.create ~capacity:(Vec.length eb.insts + List.length cx.entry_temps) 0 in
      List.iter (fun i -> ignore (Vec.push insts i)) (List.rev cx.entry_temps);
      Vec.iter (fun i -> ignore (Vec.push insts i)) eb.insts;
      eb.insts <- insts
    end;
    Hashtbl.iter (fun id _ -> if not (Hashtbl.mem cx.defined id) then unsupported "use of undefined %%v%d" id) cx.locals;
    resolve_all f
  end;
  f

let little_endian size (x : int64) =
  String.init size (fun k -> Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical x (8 * k)) 0xffL)))

let convert_global lc (g : Ir.global) =
  let base gname size galign =
    { gname; glinkage = Local; gvisibility = Default; section = None; galign = max 1 galign;
      readonly = true; thread_local = false; size; init = None; gdefined = true }
  in
  match g with
  | Ir.String_global { name; bytes } ->
      let g = base name (String.length bytes) 1 in
      g.init <- Some [ Bytes bytes ];
      g
  | Ir.Array_global { name; elem_ty; elems; align } ->
      let es = size_of_old lc elem_ty in
      let g = base name (es * List.length elems) align in
      g.init <- Some [ Bytes (String.concat "" (List.map (little_endian es) elems)) ];
      g
  | Ir.Storage_global { name; size; bytes; pointers; readonly; align; linkage; _ } ->
      let g = base name size align in
      g.readonly <- readonly;
      (match linkage with
      | Ast.Internal_global -> g.glinkage <- Local
      | Ast.Export_c -> g.glinkage <- Global
      | Ast.Import_c | Ast.Import_const_c ->
          g.glinkage <- Global;
          g.gdefined <- false);
      if g.gdefined then begin
        match (bytes, pointers) with
        | None, [] -> g.init <- None
        | _ ->
            let data = match bytes with Some d -> d | None -> String.make size '\000' in
            let pieces = ref [] and cursor = ref 0 in
            let bytes_until stop =
              if stop > !cursor then pieces := Bytes (String.sub data !cursor (stop - !cursor)) :: !pieces;
              cursor := max !cursor stop
            in
            List.iter
              (fun (off, sym, addend) ->
                bytes_until off;
                pieces := Reloc { sym; addend = Int64.of_int addend; width = 8 } :: !pieces;
                cursor := off + 8)
              (List.sort compare pointers);
            bytes_until size;
            (* all-zero data without relocations needs no bytes *)
            g.init <-
              (if pointers = [] && String.for_all (( = ) '\000') data then None else Some (List.rev !pieces))
      end;
      g

let convert (old : Ir.module_) =
  let lc = { structs = Hashtbl.create 16 } in
  List.iter (fun (d : Ir.struct_def) -> Hashtbl.replace lc.structs d.name d) old.structs;
  let m = create_module () in
  m.triple <- old.target_triple;
  let variadic = Hashtbl.create 8 in
  List.iter (fun (f : Ir.func) -> if f.variadic then Hashtbl.replace variadic f.name (List.length f.params)) old.funcs;
  List.iter (fun g -> add_global m (convert_global lc g)) old.globals;
  List.iter
    (fun (of_ : Ir.func) ->
      let f = convert_func lc m variadic of_ in
      if List.mem of_.name old.no_inline_functions then f.fattrs <- f.fattrs lor fa_noinline;
      if List.mem of_.name old.inline_functions then f.fattrs <- f.fattrs lor fa_inline;
      add_func m f)
    old.funcs;
  m
