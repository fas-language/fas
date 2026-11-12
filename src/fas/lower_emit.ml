(* Builds Fir for lower.ml (roadmap v0.6: lower.ml emits Fir directly).

   lower.ml reserves a result id with [Lower.fresh], then calls one of the
   emitters below with the id and its operands ([Low.value]s). The emitter
   appends Fir to the current block at once and records the Fir value of
   that id. Operands that are not ids become Fir constants or symbols.

   The translations (they were Fir_of_ir's, which converted the old Ir):
   - Phis become block parameters. [phi] adds the parameter; [finish] makes
     each predecessor's branch pass its incoming value.
   - Typed address steps ([gep]) become [ptradd] chains: constant indices
     fold into the displacement, each variable index is one scaled ptradd.
   - Aggregates (structs and arrays loaded, passed or returned by value)
     are pointers to stack copies. An aggregate load copies into a fresh
     entry-block slot. An aggregate store copies out of it, or clears the
     destination for zero/undef.
   - Calls to [llvm.*] names become Fir opcodes; [atomic] emits Fir
     atomics.
   - [location] sets the source location of the instructions that follow.
   - Undef becomes poison.

   A malformed request (a front-end bug) raises [Unsupported]; Lower turns
   it into an internal error. *)

exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

open Fir

(* ---------- layouts ---------- *)

(* struct name -> (size, align, agg fields), from the front end's layouts.
   The fields are the struct's flattened scalar leaves (offset, Fir type),
   which Fir_abi classifies when the struct crosses a C call by value; they
   are [] above 16 bytes, where the struct is MEMORY class anyway *)
type layouts = (string, int * int * (int * Fir.ty) list) Hashtbl.t

let rec layout (lay : layouts) (t : Low.ty) =
  match t with
  | Low.I1 | Low.I8 -> (1, 1)
  | Low.I16 -> (2, 2)
  | Low.I32 | Low.F32 -> (4, 4)
  | Low.I64 | Low.F64 -> (8, 8)
  | Low.Pointer _ ->
      (* the target's pointer size *)
      let n = (Target_layout.current ()).pointer_size in
      (n, n)
  | Low.I128 -> (16, 16)
  | Low.Void -> (0, 1)
  | Low.Vector (n, e) -> (
      let bits =
        match e with
        | Low.I1 -> 1 | Low.I8 -> 8 | Low.I16 -> 16 | Low.I32 | Low.F32 -> 32
        | Low.I64 | Low.F64 -> 64 | Low.I128 -> 128
        | Low.Pointer _ -> 8 * (Target_layout.current ()).pointer_size
        | _ -> unsupported "vector of aggregates"
      in
      match Target_layout.vector (Target_layout.current ()) n bits with
      | Ok (size, align) -> (size, align)
      | Error e -> unsupported "vector layout: %s" e)
  | Low.Array (n, e) ->
      let s, a = layout lay e in
      (n * s, a)
  | Low.Struct name -> (
      match Hashtbl.find_opt lay name with
      | None -> unsupported "unknown struct %s" name
      | Some (size, align, _) -> (size, align))

let size_of lay t = fst (layout lay t)
let is_aggregate = function Low.Struct _ | Low.Array _ -> true | _ -> false

let rec fir_ty (t : Low.ty) =
  match t with
  | Low.I1 -> i1
  | Low.I8 -> i8
  | Low.I16 -> i16
  | Low.I32 -> i32
  | Low.I64 -> i64
  | Low.I128 -> i128
  | Low.F32 -> F32
  | Low.F64 -> F64
  | Low.Pointer _ -> ptr
  | Low.Vector (n, e) -> Vec (n, fir_ty e)
  (* aggregates live in memory: their SSA stand-in is a pointer *)
  | Low.Struct _ | Low.Array _ -> ptr
  | Low.Void -> unsupported "void value"

let abi lay (t : Low.ty) (e : Low.extension) =
  if is_aggregate t then
    let size, align = layout lay t in
    let fields =
      match t with
      | Low.Struct name -> (
          match Hashtbl.find_opt lay name with Some (_, _, fields) -> fields | None -> [])
      | _ -> []
    in
    Agg { size; align; fields }
  else
    match e with
    | Low.No_extension -> Scalar (fir_ty t)
    | Low.Zero_extension -> Ext (fir_ty t, Zx)
    | Low.Sign_extension -> Ext (fir_ty t, Sx)

let result_abis lay (t : Low.ty) e = if t = Low.Void then [] else [ abi lay t e ]

(* ---------- module ---------- *)

type modctx = {
  m : modul;
  lay : layouts;
  variadic : (string, int) Hashtbl.t;  (** variadic function -> fixed parameter count *)
  locs : (string * int * int, int) Hashtbl.t;
}

let create_module ~triple lay =
  let m = create_module () in
  m.triple <- triple;
  { m; lay; variadic = Hashtbl.create 8; locs = Hashtbl.create 64 }

let loc_id mc file line col =
  let key = (file, line, col) in
  match Hashtbl.find_opt mc.locs key with
  | Some l -> l
  | None ->
      let l = add_loc mc.m file line col in
      Hashtbl.replace mc.locs key l;
      l

let signature mc ~params ~ret ~ret_extension ~variadic =
  {
    param_abis = List.map (fun (t, e) -> abi mc.lay t e) params;
    result_abis = result_abis mc.lay ret ret_extension;
    variadic;
  }

(* A function without a body (an external one) *)
let declare mc ~name ~params ~ret ~ret_extension ~variadic ~display =
  let f =
    new_func ~linkage:Global ~defined:false name (signature mc ~params ~ret ~ret_extension ~variadic)
  in
  f.display <- display;
  add_func mc.m f

(* ---------- functions ---------- *)

type fn = {
  mc : modctx;
  f : func;
  file : string option;
      (** the source file; a function without one gets no locations, as on
          the old Ir's LLVM path (no DISubprogram, no !dbg) *)
  locals : (int, value) Hashtbl.t;  (** result ids, or placeholders for later ones *)
  defined : (int, unit) Hashtbl.t;
  params : (string, value) Hashtbl.t;
  phis : (block, (Low.value * block) list list) Hashtbl.t;
      (** a block's phis' incoming values, in parameter order (reversed) *)
  closed : (block, unit) Hashtbl.t;  (** blocks with a terminator *)
  mutable entry_temps : inst list;  (** aggregate slots, for the entry block (reversed) *)
  mutable entry_allocas : inst list;  (** [alloca_entry]'s, for the entry block (reversed) *)
  mutable cur : block;
  mutable loc : int;
  last_loc : (block, int) Hashtbl.t;
      (** each block's location at its end: its terminator's (set by [finish]) *)
}

(* Starts a function with a body. [params] are (name, type, extension); the
   entry block is block 0 and current. *)
let start mc ~name ~linkage ~params ~ret ~ret_extension ~variadic ~display ~source =
  let sig_ =
    signature mc ~params:(List.map (fun (_, t, e) -> (t, e)) params) ~ret ~ret_extension ~variadic
  in
  let f = new_func ~linkage ~defined:true name sig_ in
  f.display <- display;
  let fn =
    {
      mc; f; file = Option.map fst source;
      locals = Hashtbl.create 64; defined = Hashtbl.create 64; params = Hashtbl.create 8;
      phis = Hashtbl.create 16; closed = Hashtbl.create 16; entry_temps = []; entry_allocas = [];
      cur = entry; loc = 0; last_loc = Hashtbl.create 16;
    }
  in
  (* the declaration line (the LLVM path's DISubprogram line), column 0 *)
  (match source with Some (file, line) -> f.decl_loc <- loc_id mc file line 0 | None -> ());
  let b = add_block ~name:"b0" f in
  assert (b = entry);
  List.iter2
    (fun (pname, _, _) a ->
      let v = add_param f entry (Fir_verify.abi_ty a) in
      Hashtbl.replace fn.params pname v;
      Hashtbl.replace f.names v pname)
    params sig_.param_abis;
  fn

let new_block fn = add_block fn.f ~name:("b" ^ string_of_int (num_blocks fn.f))

(* Makes [b] current. [location] is the source position its code continues
   at, if any. *)
let enter fn b location =
  fn.cur <- b;
  fn.loc <-
    (match (location, fn.file) with
    | Some (line, col), Some file -> loc_id fn.mc file line col
    | _ -> 0);
  Hashtbl.replace fn.last_loc b fn.loc

let location fn line col =
  fn.loc <- (match fn.file with Some file -> loc_id fn.mc file line col | None -> 0);
  Hashtbl.replace fn.last_loc fn.cur fn.loc
let is_open fn b = not (Hashtbl.mem fn.closed b)

let define fn id v =
  if Hashtbl.mem fn.defined id then unsupported "%%v%d defined twice" id;
  Hashtbl.replace fn.defined id ();
  (match Hashtbl.find_opt fn.locals id with
  | Some placeholder -> Vec.set fn.f.defs placeholder (Alias v)
  | None -> ());
  Hashtbl.replace fn.locals id v;
  if not (Hashtbl.mem fn.f.names v) then Hashtbl.replace fn.f.names v ("v" ^ string_of_int id)

let local fn id =
  match Hashtbl.find_opt fn.locals id with
  | Some v -> v
  | None ->
      let v = new_value fn.f Unset i1 in
      Hashtbl.replace fn.locals id v;
      v

(* An entry-block stack slot of [size] bytes *)
let temp_slot fn size align =
  let i = make_inst fn.f (Alloca (iconst fn.f i64 (Int64.of_int size), max 1 align)) [| ptr |] in
  fn.entry_temps <- i :: fn.entry_temps;
  (inst fn.f i).results.(0)

let emit1 fn op ty = emit1 ~loc:fn.loc fn.f fn.cur op ty
let emit0 fn op = emit0 ~loc:fn.loc fn.f fn.cur op

let scalar_float f (t : Low.ty) bits =
  match t with
  | Low.F32 -> fconst_bits f F32 [| Int64.logand bits 0xffffffffL |]
  | _ -> fconst_bits f F64 [| bits |]

(* The Fir value of an operand, in the current block *)
let value fn (v : Low.value) =
  let f = fn.f in
  match v with
  | Low.Local (id, _) -> local fn id
  | Low.Param (name, _) -> (
      match Hashtbl.find_opt fn.params name with
      | Some p -> p
      | None -> unsupported "unknown parameter %s" name)
  | Low.Global (name, _) -> sym f name
  | Low.Null t -> null f (fir_ty t)
  | Low.Undef t when is_aggregate t ->
      let size, align = layout fn.mc.lay t in
      temp_slot fn size align
  | Low.Zero t when is_aggregate t ->
      let size, align = layout fn.mc.lay t in
      let s = temp_slot fn size align in
      emit0 fn (Memset (false, s, iconst f i8 0L, iconst f i64 (Int64.of_int size)));
      s
  | Low.Undef t -> poison f (fir_ty t)
  | Low.Zero t -> zero f (fir_ty t)
  | Low.Const (((Low.F32 | Low.F64) as t), bits) -> scalar_float f t bits
  | Low.Const (t, n) -> iconst f (fir_ty t) n
  | Low.Const_vector ((Low.Vector (_, e) as t), vals) ->
      let lanes =
        List.map
          (fun n -> match e with Low.F32 | Low.F64 -> scalar_float f e n | _ -> iconst f (fir_ty e) n)
          vals
      in
      vconst f (fir_ty t) (Array.of_list lanes)
  | Low.Const_vector _ -> unsupported "vector constant of a non-vector type"

let mem align = plain_mem (max 1 align)
let vmem align = { (plain_mem (max 1 align)) with volatile = true }
let starts p s = String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* ---------- instructions ---------- *)

let bin fn id (op : Low.binop) t x y =
  let x = value fn x and y = value fn y and r = fir_ty t in
  let i k = Ibin (k, 0, x, y) and fl k = Fbin (k, 0, x, y) in
  let op =
    match op with
    | Low.Add -> i Add | Low.Sub -> i Sub | Low.Mul -> i Mul | Low.Sdiv -> i Sdiv
    | Low.Srem -> i Srem | Low.Udiv -> i Udiv | Low.Urem -> i Urem | Low.And -> i And
    | Low.Or -> i Or | Low.Xor -> i Xor | Low.Shl -> i Shl | Low.Lshr -> i Lshr
    | Low.Ashr -> i Ashr | Low.Fadd -> fl Fadd | Low.Fsub -> fl Fsub | Low.Fmul -> fl Fmul
    | Low.Fdiv -> fl Fdiv
  in
  define fn id (emit1 fn op r)

let fneg fn id t x = define fn id (emit1 fn (Fun (Fneg, 0, value fn x)) (fir_ty t))

(* [t] is the operands' type; the result is i1 (or a vector of them) *)
let cmp fn id (c : Low.cmp) t x y =
  let x = value fn x and y = value fn y in
  let r = with_elem (fir_ty t) i1 in
  let i k = Icmp (k, x, y) and fl k = Fcmp (k, 0, x, y) in
  let op =
    match c with
    | Low.Eq -> i Eq | Low.Ne -> i Ne | Low.Slt -> i Slt | Low.Sle -> i Sle | Low.Sgt -> i Sgt
    | Low.Sge -> i Sge | Low.Ult -> i Ult | Low.Ule -> i Ule | Low.Ugt -> i Ugt | Low.Uge -> i Uge
    | Low.Foeq -> fl Foeq | Low.Fune -> fl Fune | Low.Folt -> fl Folt | Low.Fole -> fl Fole
    | Low.Fogt -> fl Fogt | Low.Foge -> fl Foge
  in
  define fn id (emit1 fn op r)

(* A stack slot holding one [t], in the current block *)
let alloca fn id t align =
  let size = size_of fn.mc.lay t in
  define fn id (emit1 fn (Alloca (iconst fn.f i64 (Int64.of_int size), max 1 align)) ptr)

(* A stack slot placed at the top of the entry block when the function is
   finished, whatever block is current *)
let alloca_entry fn id t align =
  let size = size_of fn.mc.lay t in
  let i =
    make_inst fn.f (Alloca (iconst fn.f i64 (Int64.of_int size), max 1 align)) [| ptr |]
  in
  fn.entry_allocas <- i :: fn.entry_allocas;
  define fn id (inst fn.f i).results.(0)

let load_gen ~volatile fn id t p align =
  let f = fn.f in
  if is_aggregate t then begin
    let size, al = layout fn.mc.lay t in
    let s = temp_slot fn size (max al align) in
    emit0 fn (Memcpy (volatile, s, value fn p, iconst f i64 (Int64.of_int size)));
    define fn id s
  end
  else define fn id (emit1 fn (Load ((if volatile then vmem align else mem align), value fn p)) (fir_ty t))

let load fn id t p align = load_gen ~volatile:false fn id t p align
let load_volatile fn id t p align = load_gen ~volatile:true fn id t p align

let store_gen ~volatile fn t (x : Low.value) p align =
  let f = fn.f in
  if is_aggregate t then begin
    let size = iconst f i64 (Int64.of_int (size_of fn.mc.lay t)) in
    match x with
    | Low.Zero _ | Low.Undef _ | Low.Null _ -> emit0 fn (Memset (volatile, value fn p, iconst f i8 0L, size))
    | _ ->
        let src = value fn x in
        emit0 fn (Memcpy (volatile, value fn p, src, size))
  end
  else
    let x = value fn x in
    emit0 fn (Store ((if volatile then vmem align else mem align), x, value fn p))

let store fn t x p align = store_gen ~volatile:false fn t x p align
let store_volatile fn t x p align = store_gen ~volatile:true fn t x p align

(* Converts an index value to i64 the way GEP does (sign extension) *)
let index64 fn v =
  let x = value fn v in
  match ty_of fn.f x with
  | Int 64 -> x
  | Int w when w < 64 -> (
      match int_value fn.f x with
      | Some n -> iconst fn.f i64 n
      | None -> emit1 fn (Conv (Sext, x)) i64)
  | _ -> unsupported "GEP index wider than 64 bits"

(* The address of an element: [p] points at a [t] (or an array of them, the
   first index stepping over whole [t]s), and each further index steps into
   an array or vector. Constant indices accumulate into one displacement;
   each variable index emits ptradd(cur, idx, scale, disp). Struct fields
   are addressed by byte offsets in lower.ml, so a struct is never stepped
   into. *)
let gep fn id (t : Low.ty) p (idxs : Low.gep_index list) =
  let lay = fn.mc.lay in
  let cur = ref (value fn p) and disp = ref 0 in
  let step size (idx : Low.gep_index) =
    match idx with
    | Low.Zero -> ()
    | Low.Index v -> (
        let x = index64 fn v in
        match int_value fn.f x with
        | Some n -> disp := !disp + (Int64.to_int n * size)
        | None ->
            cur := emit1 fn (Ptradd (0, !cur, x, size, !disp)) ptr;
            disp := 0)
  in
  let rec walk (t : Low.ty) = function
    | [] -> ()
    | idx :: rest -> (
        match t with
        | Low.Array (_, e) | Low.Vector (_, e) ->
            step (size_of lay e) idx;
            walk e rest
        | _ -> unsupported "gep into a scalar or struct")
  in
  (match idxs with
  | [] -> ()
  | first :: rest ->
      step (size_of lay t) first;
      walk t rest);
  define fn id (if !disp = 0 then !cur else emit1 fn (Ptradd (0, !cur, iconst fn.f i64 0L, 1, !disp)) ptr)

let conv_of_name = function
  | "trunc" -> Trunc | "zext" -> Zext | "sext" -> Sext | "fptrunc" -> Fptrunc
  | "fpext" -> Fpext | "fptosi" -> Fptosi | "fptoui" -> Fptoui | "sitofp" -> Sitofp
  | "uitofp" -> Uitofp | "ptrtoint" -> Ptrtoint | "inttoptr" -> Inttoptr
  | "bitcast" -> Bitcast | k -> unsupported "cast %s" k

(* [kind] is the LLVM cast name (trunc, zext, ..., bitcast) *)
let cast fn id kind from x t =
  if is_aggregate from || is_aggregate t then unsupported "aggregate cast";
  let x = value fn x in
  let k = conv_of_name kind in
  let rt = fir_ty t in
  (* pointers are opaque: a pointer bitcast is the value itself, and so is
     any cast whose source already has the target type *)
  if k = Bitcast && ty_of fn.f x = rt then define fn id x else define fn id (emit1 fn (Conv (k, x)) rt)

let const_int v = match v with Low.Const (_, n) -> Some n | _ -> None

(* The result of an [llvm.*] call, or None when the intrinsic has none *)
let intrinsic fn name (ret : Low.ty) (args : (Low.ty * Low.extension * Low.value) list) =
  let a k = let _, _, v = List.nth args k in value fn v in
  let r = if ret = Low.Void then i1 else fir_ty ret in
  let one op = Some (emit1 fn op r) in
  if starts "llvm.lifetime." name then begin
    let size = match List.nth args 0 with _, _, Low.Const (_, n) -> Int64.to_int n | _ -> -1 in
    if size >= 0 then emit0 fn (Lifetime (starts "llvm.lifetime.start." name, size, a 1));
    None
  end
  else if name = "llvm.trap" then (emit0 fn Trap; None)
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
    let fl =
      match const_int zp with
      | Some 0L -> 0
      | Some _ -> f_zero_poison
      | None -> unsupported "%s with a variable flag" name
    in
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
    | _, _, (Low.Zero _ | Low.Undef _) -> ()
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
    emit0 fn (Masked_store (al, a 0, a 1, a 3));
    None
  end
  else unsupported "intrinsic %s" name

let emit_call fn result ext (ret : Low.ty) callee args =
  let lay = fn.mc.lay in
  let fixed = match callee with Direct n -> Hashtbl.find_opt fn.mc.variadic n | Indirect _ -> None in
  let nfixed = match fixed with Some k -> k | None -> List.length args in
  let param_abis = List.filteri (fun k _ -> k < nfixed) (List.map (fun (t, e, _) -> abi lay t e) args) in
  let sig_ = { param_abis; result_abis = result_abis lay ret ext; variadic = fixed <> None } in
  let argv = Array.of_list (List.map (fun (_, _, v) -> value fn v) args) in
  (* a struct through `...` names its agg ABI on the call (SPEC §5) *)
  let va_aggs =
    List.concat
      (List.mapi
         (fun k (t, e, _) -> if k >= nfixed && is_aggregate t then [ (k, abi lay t e) ] else [])
         args)
  in
  let tys = Array.of_list (List.map Fir_verify.abi_ty sig_.result_abis) in
  let rs = emit ~loc:fn.loc fn.f fn.cur (Call { sig_; callee; args = argv; attrs = 0; va_aggs }) tys in
  match (result, rs) with
  | Some id, [| r |] -> define fn id r
  | None, _ -> ()
  | Some _, _ -> unsupported "call result of a void function"

(* A call by name; [llvm.*] names are Fir opcodes *)
let call fn result ext ret name args =
  if starts "llvm." name then
    match (intrinsic fn name ret args, result) with
    | Some r, Some id -> define fn id r
    | None, Some _ -> unsupported "%s has no result" name
    | _ -> ()
  else emit_call fn result ext ret (Direct name) args

let call_indirect fn result ext ret callee args =
  let c = value fn callee in
  emit_call fn result ext ret (Indirect c) args

(* An atomic access of [ty] (the loaded, stored or exchanged type; Void for
   a fence). [values] are (p), (p, v) or (p, expected, desired); the result
   is the old value. *)
let atomic fn result (op : Hir.atomic_op) (order : Hir.atomic_ordering) (ty : Low.ty) values =
  let fo : ordering =
    match order with
    | Hir.Relaxed -> Relaxed
    | Hir.Acquire -> Acquire
    | Hir.Release -> Release
    | Hir.Acq_rel -> Acq_rel
    | Hir.Seq_cst -> Seq_cst
  in
  let a i = value fn (List.nth values i) in
  let am = { (mem (size_of fn.mc.lay ty)) with order = fo } in
  let def r = match result with Some id -> define fn id r | None -> () in
  match op with
  | Hir.Atomic_fence -> emit0 fn (Fence fo)
  | Hir.Atomic_load -> def (emit1 fn (Load (am, a 0)) (fir_ty ty))
  | Hir.Atomic_store ->
      let p = a 0 in
      emit0 fn (Store (am, a 1, p))
  | Hir.Atomic_cas ->
      (* C's failure ordering: a failed exchange does not store, so it
         cannot release *)
      let fail : ordering =
        match order with
        | Hir.Release | Hir.Relaxed -> Relaxed
        | Hir.Acquire | Hir.Acq_rel -> Acquire
        | Hir.Seq_cst -> Seq_cst
      in
      let p = a 0 in
      let expected = a 1 in
      let desired = a 2 in
      let rs =
        emit ~loc:fn.loc fn.f fn.cur
          (Cmpxchg { mem = am; fail; weak = false; p; expected; desired })
          [| fir_ty ty; i1 |]
      in
      def rs.(0)
  | Hir.Atomic_add | Hir.Atomic_sub | Hir.Atomic_and | Hir.Atomic_or | Hir.Atomic_xor
  | Hir.Atomic_xchg ->
      let k =
        match op with
        | Hir.Atomic_add -> Radd
        | Hir.Atomic_sub -> Rsub
        | Hir.Atomic_and -> Rand
        | Hir.Atomic_or -> Ror
        | Hir.Atomic_xor -> Rxor
        | _ -> Xchg
      in
      let p = a 0 in
      def (emit1 fn (Rmw (k, am, p, a 1)) (fir_ty ty))

(* A phi of the current block (before its other instructions): a block
   parameter, with each predecessor's value passed by [finish] *)
let phi fn id t (incoming : (Low.value * block) list) =
  if is_aggregate t then unsupported "aggregate phi";
  if fn.cur = entry then unsupported "phi in the entry block";
  let p = add_param fn.f fn.cur (fir_ty t) in
  define fn id p;
  let prev = Option.value ~default:[] (Hashtbl.find_opt fn.phis fn.cur) in
  Hashtbl.replace fn.phis fn.cur (incoming :: prev)

let select fn id c x y =
  let t = Low.value_ty x in
  if is_aggregate t then unsupported "aggregate select";
  let c = value fn c in
  let x = value fn x in
  let y = value fn y in
  define fn id (emit1 fn (Select (c, x, y)) (fir_ty t))

let extract fn id vt x l =
  let e = match vt with Low.Vector (_, e) -> fir_ty e | _ -> unsupported "extract from a non-vector" in
  let x = value fn x in
  define fn id (emit1 fn (Extract (x, value fn l)) e)

let insert fn id vt x l e =
  let x = value fn x in
  let e = value fn e in
  define fn id (emit1 fn (Insert (x, e, value fn l)) (fir_ty vt))

(* lane 0 of [x] in every lane of a [vt] *)
let shuffle_zero fn id vt x =
  let n = match vt with Low.Vector (n, _) -> n | _ -> unsupported "shuffle of a non-vector" in
  let xv = value fn x in
  define fn id (emit1 fn (Shuffle (xv, zero fn.f (ty_of fn.f xv), Array.make n 0)) (fir_ty vt))

let shufflevector fn id _t x y (mask : Low.value) =
  let lanes =
    match mask with
    | Low.Const_vector (_, l) -> Array.of_list (List.map Int64.to_int l)
    | Low.Zero (Low.Vector (n, _)) -> Array.make n 0
    | Low.Undef (Low.Vector (n, _)) -> Array.make n (-1)
    | _ -> unsupported "shufflevector with a variable mask"
  in
  let xv = value fn x in
  let yv = value fn y in
  let e = elem (ty_of fn.f xv) in
  define fn id (emit1 fn (Shuffle (xv, yv, lanes)) (Vec (Array.length lanes, e)))

(* the address of string literal [index] (global .str.<index>) *)
let string_ptr fn id index = define fn id (sym fn.f (".str." ^ string_of_int index))
let global_ptr fn id name = define fn id (sym fn.f name)
let trap fn = emit0 fn Trap

let syscall fn id args =
  let args = List.map (value fn) args in
  define fn id (emit1 fn (Syscall (Array.of_list args)) i64)

(* ---------- terminators ---------- *)

(* Branch arguments for the phis are filled in by [finish] *)
let set_term fn t =
  set_term ~loc:fn.loc fn.f fn.cur t;
  Hashtbl.replace fn.closed fn.cur ()

let ret fn (v : (Low.ty * Low.value) option) =
  set_term fn (Ret (match v with None -> [||] | Some (_, x) -> [| value fn x |]))

let br fn dest = set_term fn (Jump (target dest))

let cond_br fn c yes no =
  let c = value fn c in
  set_term fn (Br (c, target yes, target no, -1))

let switch fn t x (cases : (int64 * block) list) default =
  let w = int_width (fir_ty t) in
  let seen = Hashtbl.create 8 in
  (* LLVM rejects duplicate keys; keep the first, as the old Ir printed it *)
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
  let x = value fn x in
  set_term fn (Switch (x, target default, Array.of_list cases))

let unreachable fn = set_term fn Unreachable

(* ---------- finishing ---------- *)

(* Completes the function and adds it to the module. Error: a block that
   has no terminator. *)
let finish fn =
  let f = fn.f in
  let unterminated = ref None in
  for b = num_blocks f - 1 downto 0 do
    if not (Hashtbl.mem fn.closed b) then unterminated := Some b
  done;
  match !unterminated with
  | Some b -> Error b
  | None ->
      (* branch arguments: each phi's value for the edge *)
      for b = 0 to num_blocks f - 1 do
        let br = block f b in
        let saved = fn.cur in
        fn.cur <- b;
        let target (t : target) =
          match Hashtbl.find_opt fn.phis t.dest with
          | None -> t
          | Some phis ->
              if t.dest = entry then unsupported "branch to the entry block";
              let args =
                List.rev_map
                  (fun incoming ->
                    match List.find_opt (fun (_, p) -> p = b) incoming with
                    | Some (v, _) -> value fn v
                    | None -> unsupported "phi in b%d has no value for edge from b%d" t.dest b)
                  phis
              in
              { t with args = Array.of_list args }
        in
        br.term <-
          (match br.term with
          | Jump t -> Jump (target t)
          | Br (c, t, e, w) -> Br (c, target t, target e, w)
          | Switch (x, d, cases) -> Switch (x, target d, Array.map (fun (k, t) -> (k, target t)) cases)
          | t -> t);
        fn.cur <- saved
      done;
      (* a terminator is at its block's last location, even when code was
         lowered into the block after it (as the old Ir placed it) *)
      for b = 0 to num_blocks f - 1 do
        (block f b).term_loc <- Option.value ~default:0 (Hashtbl.find_opt fn.last_loc b)
      done;
      (* stack slots go first in the entry block: aggregate temporaries,
         then the variables' slots *)
      let tops = List.rev_append fn.entry_temps (List.rev fn.entry_allocas) in
      if tops <> [] then begin
        let eb = block f entry in
        let insts = Vec.create ~capacity:(Vec.length eb.insts + List.length tops) 0 in
        List.iter (fun i -> ignore (Vec.push insts i)) tops;
        Vec.iter (fun i -> ignore (Vec.push insts i)) eb.insts;
        eb.insts <- insts
      end;
      Hashtbl.iter
        (fun id _ -> if not (Hashtbl.mem fn.defined id) then unsupported "use of undefined %%v%d" id)
        fn.locals;
      resolve_all f;
      add_func fn.mc.m f;
      Ok ()

(* ---------- globals ---------- *)

let little_endian size (x : int64) =
  String.init size (fun k -> Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical x (8 * k)) 0xffL)))

let base_global gname size galign =
  { gname; glinkage = Local; gvisibility = Default; section = None; galign = max 1 galign;
    readonly = true; thread_local = false; size; init = None; gdefined = true }

(* string literal [index]'s bytes, as global .str.<index> *)
let string_global mc index bytes =
  let g = base_global (".str." ^ string_of_int index) (String.length bytes) 1 in
  g.init <- Some [ Bytes bytes ];
  add_global mc.m g

(* a read-only array of integers *)
let array_global mc ~name ~elem_ty ~elems ~align =
  let es = size_of mc.lay elem_ty in
  let g = base_global name (es * List.length elems) align in
  g.init <- Some [ Bytes (String.concat "" (List.map (little_endian es) elems)) ];
  add_global mc.m g

(* A global variable: [bytes] is its initial image (None = zero) and
   [pointers] the (offset, symbol, addend) addresses stored in it *)
let storage_global mc ~name ~size ~bytes ~pointers ~readonly ~align ~(linkage : Ast.global_linkage) =
  let g = base_global name size align in
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
        let width = (Target_layout.current ()).pointer_size in
        let pieces = ref [] and cursor = ref 0 in
        let bytes_until stop =
          if stop > !cursor then pieces := Bytes (String.sub data !cursor (stop - !cursor)) :: !pieces;
          cursor := max !cursor stop
        in
        List.iter
          (fun (off, sym, addend) ->
            bytes_until off;
            pieces := Reloc { sym; addend = Int64.of_int addend; width } :: !pieces;
            cursor := off + width)
          (List.sort compare pointers);
        bytes_until size;
        (* all-zero data without relocations needs no bytes *)
        g.init <- (if pointers = [] && String.for_all (( = ) '\000') data then None else Some (List.rev !pieces))
  end;
  add_global mc.m g
