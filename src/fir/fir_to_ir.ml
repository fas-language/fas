(* Fir -> old Ir, the inverse of Fir_of_ir.

   This exists for testing only: FAS_FIR=roundtrip sends every module
   through Ir -> Fir -> verify -> Ir before the old native backend, which
   checks the forward converter against real programs. It is not tuned for
   code quality; the old backend's own passes clean most of it up.

   The main translations:
   - Block parameters become phis. When one terminator sends different
     arguments to the same block (legal in Fir, impossible with phis), the
     second edge goes through a new trampoline block.
   - [ptradd p, i, s, d] becomes a typed GEP on i8/i16/i32/i64 (or a multiply
     and an i8 GEP for other scales), then an i8 GEP for the displacement.
   - Aggregates are [N x i8] arrays: an aggregate parameter or call result
     is stored into an entry-block slot, and an aggregate argument or return
     value is loaded from its pointer. Constant-size memcpy/memset become
     [N x i8] loads and stores, which the old backend copies bytewise.
   - Intrinsic-like ops become the llvm.* calls the old backend knows. *)

exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

open Fir

let rec ir_ty (t : ty) : Ir.ty =
  match t with
  | Int 1 -> Ir.I1
  | Int 8 -> Ir.I8
  | Int 16 -> Ir.I16
  | Int 32 -> Ir.I32
  | Int 64 -> Ir.I64
  | Int 128 -> Ir.I128
  | F32 -> Ir.F32
  | F64 -> Ir.F64
  | Ptr _ -> Ir.Pointer Ir.I8
  | Vec (n, e) -> Ir.Vector (n, ir_ty e)
  | t -> unsupported "type %s" (ty_to_string t)

let bytes n = Ir.Array (n, Ir.I8)

(* LLVM's overloaded-intrinsic suffix: i32, f64, v4i32, ... *)
let rec suffix (t : ty) =
  match t with
  | Int w -> "i" ^ string_of_int w
  | F32 -> "f32"
  | F64 -> "f64"
  | Ptr _ -> "p0"
  | Vec (n, e) -> Printf.sprintf "v%d%s" n (suffix e)
  | t -> unsupported "intrinsic on %s" (ty_to_string t)

(* ---------- aggregates by value: the x86-64 SysV classification ----------

   A Fir [agg] is classified here, where its field types and padding are
   still known, and becomes one of two reserved named Ir structs that the
   native backend passes by the SysV rules (see [Native_gen.sysv_agg]):
   - [fir.sysv.r.CC]: at most 16 bytes, every field at its natural
     alignment. One I64 (C = 'i', INTEGER) or F64 (C = 'f', SSE) field per
     eightbyte; an eightbyte holding any integer is INTEGER. Passed in
     registers when enough of both kinds remain for all of it, else on the
     stack; returned in rax/rdx and xmm0/xmm1.
   - [fir.sysv.m.N]: MEMORY class (larger than 16 bytes, or a misaligned
     field), N the size rounded up to 8. Copied onto the stack; returned
     through a hidden pointer in rdi.
   Both are padded to whole eightbytes, so values are copied into padded
   slots and never read past the front end's own bytes.

   An aggregate this cannot classify (vector, i128 or x87 fields, an empty
   or overaligned one) keeps the plain [N x i8] form, which the native
   backend passes by address: fine between functions it compiles, refused
   at an external boundary. *)
let sysv_structs : (string, Ir.struct_def) Hashtbl.t = Hashtbl.create 8

let sysv_agg ~size ~align fields =
  let natural = function
    | Int (1 | 8) -> Some 1 | Int 16 -> Some 2 | Int 32 | F32 -> Some 4 | Int 64 | Ptr _ | F64 -> Some 8
    | _ -> None
  in
  let sizes = List.map (fun (off, t) -> (off, t, natural t)) fields in
  if size <= 0 || align > 8 || List.exists (fun (_, _, n) -> n = None) sizes then None
  else
    let aligned = List.for_all (fun (off, _, n) -> off mod Option.get n = 0) sizes in
    if size > 16 || not aligned then begin
      let n = (size + 7) / 8 * 8 in
      let name = Printf.sprintf "fir.sysv.m.%d" n in
      Hashtbl.replace sysv_structs name { Ir.name; fields = [ bytes n ]; tail_padding = 0 };
      Some (Ir.Struct name, n)
    end
    else begin
      let words = (size + 7) / 8 in
      (* None = no field yet, Some true = SSE, Some false = INTEGER *)
      let cls = Array.make words None in
      List.iter
        (fun (off, t, _) ->
          let w = off / 8 in
          let sse = t = F32 || t = F64 in
          cls.(w) <- Some (match cls.(w) with None -> sse | Some c -> c && sse))
        sizes;
      if Array.exists (( = ) None) cls then None
      else
        let letters = Array.to_list (Array.map (fun c -> if c = Some true then "f" else "i") cls) in
        let name = "fir.sysv.r." ^ String.concat "" letters in
        let fields = List.map (fun l -> if l = "f" then Ir.F64 else Ir.I64) letters in
        Hashtbl.replace sysv_structs name { Ir.name; fields; tail_padding = 0 };
        Some (Ir.Struct name, 8 * words)
    end

(* The Ir type of an aggregate and its size there (at least [size]) *)
let agg_ir ~size ~align fields =
  match sysv_agg ~size ~align fields with Some r -> r | None -> (bytes size, size)

let abi_ir (a : abi) =
  match a with
  | Scalar t -> (ir_ty t, Ir.No_extension)
  | Ext (t, Zx) -> (ir_ty t, Ir.Zero_extension)
  | Ext (t, Sx) -> (ir_ty t, Ir.Sign_extension)
  | Agg { size; align; fields } -> (fst (agg_ir ~size ~align fields), Ir.No_extension)

type fctx = {
  m : modul;
  f : func;
  map : (value, Ir.value) Hashtbl.t;  (** results and params -> Ir values *)
  mutable next_id : int;
  mutable cur : Ir.instr list;  (** current block, reversed *)
  mutable entry_slots : Ir.instr list;  (** allocas for the entry block, reversed *)
  mutable cur_loc : int;
}

let fresh cx =
  let id = cx.next_id in
  cx.next_id <- id + 1;
  id

let add cx i = cx.cur <- i :: cx.cur

let limb0 = function [||] -> 0L | l -> l.(0)

let vv cx (v : value) : Ir.value =
  let f = cx.f in
  let v = resolve f v in
  let t = ty_of f v in
  match def_of f v with
  | Const c -> (
      match c with
      | Cint (_, limbs) ->
          if Array.length limbs > 1 then begin
            let lo = limbs.(0) in
            let sign = if Int64.compare lo 0L < 0 then -1L else 0L in
            for k = 1 to Array.length limbs - 1 do
              if limbs.(k) <> sign then unsupported "integer constant wider than 64 bits"
            done
          end;
          Ir.Const (ir_ty t, limb0 limbs)
      | Cfloat (_, limbs) -> Ir.Const (ir_ty t, limb0 limbs)
      | Cnull _ -> Ir.Null (ir_ty t)
      | Cpoison _ -> Ir.Undef (ir_ty t)
      | Czero _ -> Ir.Zero (ir_ty t)
      | Cvec (_, lanes) ->
          let lane x =
            match const_of f x with
            | Some (Cint (_, l)) | Some (Cfloat (_, l)) -> limb0 l
            | Some (Cpoison _ | Czero _) -> 0L
            | _ -> unsupported "vector constant with a non-constant lane"
          in
          Ir.Const_vector (ir_ty t, Array.to_list (Array.map lane lanes))
      | Csym (name, 0L) -> Ir.Global (name, Ir.Pointer Ir.I8)
      | Csym (name, addend) ->
          let id = fresh cx in
          add cx (Ir.Gep (id, Ir.I8, Ir.Global (name, Ir.Pointer Ir.I8), [ Ir.Index (Ir.Const (Ir.I64, addend)) ]));
          Ir.Local (id, Ir.Pointer Ir.I8)
      | Cblockaddr _ -> unsupported "block address")
  | Result _ | Param _ -> (
      match Hashtbl.find_opt cx.map v with
      | Some x -> x
      | None -> Ir.Local (v, ir_ty t))
  | Alias _ | Unset -> unsupported "unresolved value %d" v

(* Defines Fir value [v] as the result of an Ir instruction built from a
   fresh-or-own id *)
let def cx v mk =
  let t = ir_ty (ty_of cx.f v) in
  add cx (mk v t);
  Hashtbl.replace cx.map v (Ir.Local (v, t))

let alias cx v x = Hashtbl.replace cx.map v x

let const_int cx v = int_value cx.f v

let call cx result ret name args = add cx (Ir.Call (result, Ir.No_extension, ret, name, args))

let intrinsic cx v name args =
  let t = ir_ty (ty_of cx.f v) in
  call cx (Some v) t name args;
  Hashtbl.replace cx.map v (Ir.Local (v, t))

let arg cx x = (ir_ty (ty_of cx.f x), Ir.No_extension, vv cx x)

let icmp_ir : icmp -> Ir.cmp = function
  | Eq -> Ir.Eq | Ne -> Ir.Ne | Slt -> Ir.Slt | Sle -> Ir.Sle | Sgt -> Ir.Sgt | Sge -> Ir.Sge
  | Ult -> Ir.Ult | Ule -> Ir.Ule | Ugt -> Ir.Ugt | Uge -> Ir.Uge

let fcmp_ir (k : fcmp) : Ir.cmp =
  match k with
  | Foeq -> Ir.Foeq | Fune -> Ir.Fune | Folt -> Ir.Folt | Fole -> Ir.Fole | Fogt -> Ir.Fogt
  | Foge -> Ir.Foge
  | k -> unsupported "fcmp %s" (Fir_print.fcmp_name k)

let conv_ir = function
  | Trunc -> "trunc" | Zext -> "zext" | Sext -> "sext" | Fptrunc -> "fptrunc" | Fpext -> "fpext"
  | Fptosi -> "fptosi" | Fptoui -> "fptoui" | Sitofp -> "sitofp" | Uitofp -> "uitofp"
  | Ptrtoint -> "ptrtoint" | Inttoptr -> "inttoptr" | Bitcast -> "bitcast"
  | k -> unsupported "conversion %s" (Fir_print.conv_name k)

let mem_align (m : mem) = m.align

let atomic_order : ordering -> Ir.atomic_order = function
  | Relaxed -> Ir.Relaxed
  | Acquire -> Ir.Acquire
  | Release -> Ir.Release
  | Acq_rel -> Ir.Acq_rel
  | Seq_cst -> Ir.Seq_cst
  | Plain -> unsupported "plain ordering on an atomic"

(* An entry-block slot of [n] bytes *)
let slot cx n align =
  let id = fresh cx in
  cx.entry_slots <- Ir.Alloca (id, bytes n, max 1 align) :: cx.entry_slots;
  Ir.Local (id, Ir.Pointer Ir.I8)

(* Loads the [n]-byte aggregate at pointer [p] as an Ir value *)
let load_agg cx n p =
  let id = fresh cx in
  add cx (Ir.Load (id, bytes n, p, 1));
  Ir.Local (id, bytes n)

(* Loads the [size]-byte aggregate at [p] as a value of its ABI type (see
   [agg_ir]), first copying it into a padded slot when that type is larger *)
let load_abi_agg cx ~size ~align fields p =
  let ty, padded = agg_ir ~size ~align fields in
  let p =
    if padded = size then p
    else begin
      let s = slot cx padded 8 in
      add cx (Ir.Store (bytes size, load_agg cx size p, s, 1));
      s
    end
  in
  let id = fresh cx in
  add cx (Ir.Load (id, ty, p, 1));
  Ir.Local (id, ty)

let convert_inst cx (i : inst) =
  let f = cx.f in
  let ir = inst f i in
  if ir.loc <> 0 && ir.loc <> cx.cur_loc then begin
    let _, line, col = Vec.get cx.m.locs ir.loc in
    add cx (Ir.Location (line, col));
    cx.cur_loc <- ir.loc
  end;
  let r k = ir.results.(k) in
  let v = vv cx in
  match ir.op with
  | Nop | Dbg_value _ -> ()
  | Ibin (k, _, a, b) -> (
      let bin op = def cx (r 0) (fun id t -> Ir.Bin (id, op, t, v a, v b)) in
      let minmax c =
        let t = ir_ty (ty_of f a) in
        let cid = fresh cx in
        add cx (Ir.Cmp (cid, c, t, v a, v b));
        let ct = match t with Ir.Vector (n, _) -> Ir.Vector (n, Ir.I1) | _ -> Ir.I1 in
        def cx (r 0) (fun id _ -> Ir.Select (id, Ir.Local (cid, ct), v a, v b))
      in
      let sfx = suffix (ty_of f a) in
      match k with
      | Add -> bin Ir.Add | Sub -> bin Ir.Sub | Mul -> bin Ir.Mul | Sdiv -> bin Ir.Sdiv
      | Udiv -> bin Ir.Udiv | Srem -> bin Ir.Srem | Urem -> bin Ir.Urem | And -> bin Ir.And
      | Or -> bin Ir.Or | Xor -> bin Ir.Xor | Shl -> bin Ir.Shl | Lshr -> bin Ir.Lshr
      | Ashr -> bin Ir.Ashr
      | Smin -> minmax Ir.Slt | Smax -> minmax Ir.Sgt | Umin -> minmax Ir.Ult | Umax -> minmax Ir.Ugt
      | Umulh | Smulh -> (
          (* trunc ((ext a * ext b) >> w) in twice the width, the same shape
             Fas lowers `mul_hi` to. LLVM matches it to one mul; the native
             scalarize pass (mul_high) turns the i128 form back into its
             llvm.fas.umulh/smulh pseudo-intrinsic, which LLVM itself does
             not know. *)
          let t = ir_ty (ty_of f a) in
          let wide =
            match t with
            | Ir.I8 -> Ir.I16 | Ir.I16 -> Ir.I32 | Ir.I32 -> Ir.I64 | Ir.I64 -> Ir.I128
            | _ -> unsupported "vector or i1 mul-high"
          in
          let w = match t with Ir.I8 -> 8L | Ir.I16 -> 16L | Ir.I32 -> 32L | _ -> 64L in
          let signed = k = Smulh in
          let ext x =
            let id = fresh cx in
            add cx (Ir.Cast (id, (if signed then "sext" else "zext"), t, v x, wide));
            Ir.Local (id, wide)
          in
          let xa = ext a in
          let xb = ext b in
          let p = fresh cx in
          add cx (Ir.Bin (p, Ir.Mul, wide, xa, xb));
          let h = fresh cx in
          add cx
            (Ir.Bin
               (h, (if signed then Ir.Ashr else Ir.Lshr), wide, Ir.Local (p, wide), Ir.Const (wide, w)));
          def cx (r 0) (fun id t -> Ir.Cast (id, "trunc", wide, Ir.Local (h, wide), t)))
      | Uadd_sat -> intrinsic cx (r 0) ("llvm.uadd.sat." ^ sfx) [ arg cx a; arg cx b ]
      | Sadd_sat -> intrinsic cx (r 0) ("llvm.sadd.sat." ^ sfx) [ arg cx a; arg cx b ]
      | Usub_sat -> intrinsic cx (r 0) ("llvm.usub.sat." ^ sfx) [ arg cx a; arg cx b ]
      | Ssub_sat -> intrinsic cx (r 0) ("llvm.ssub.sat." ^ sfx) [ arg cx a; arg cx b ])
  | Iun (k, fl, a) -> (
      let sfx = suffix (ty_of f a) in
      let zp = Ir.Const (Ir.I1, if fl land f_zero_poison <> 0 then 1L else 0L) in
      match k with
      | Ctz -> intrinsic cx (r 0) ("llvm.cttz." ^ sfx) [ arg cx a; (Ir.I1, Ir.No_extension, zp) ]
      | Clz -> intrinsic cx (r 0) ("llvm.ctlz." ^ sfx) [ arg cx a; (Ir.I1, Ir.No_extension, zp) ]
      | Popcnt -> intrinsic cx (r 0) ("llvm.ctpop." ^ sfx) [ arg cx a ]
      | k -> unsupported "%s" (Fir_print.iun_name k))
  | Fbin (k, _, a, b) -> (
      let bin op = def cx (r 0) (fun id t -> Ir.Bin (id, op, t, v a, v b)) in
      match k with
      | Fadd -> bin Ir.Fadd | Fsub -> bin Ir.Fsub | Fmul -> bin Ir.Fmul | Fdiv -> bin Ir.Fdiv
      | k -> unsupported "%s" (Fir_print.fbin_name k))
  | Fun (k, _, a) -> (
      let sfx = suffix (ty_of f a) in
      let un name = intrinsic cx (r 0) (name ^ sfx) [ arg cx a ] in
      match k with
      | Fneg -> def cx (r 0) (fun id t -> Ir.Fneg (id, t, v a))
      | Fabs -> un "llvm.fabs." | Sqrt -> un "llvm.sqrt." | Floor -> un "llvm.floor."
      | Ceil -> un "llvm.ceil." | Roundeven -> un "llvm.roundeven."
      | k -> unsupported "%s" (Fir_print.fun_name k))
  | Fma (_, a, b, c) -> intrinsic cx (r 0) ("llvm.fma." ^ suffix (ty_of f a)) [ arg cx a; arg cx b; arg cx c ]
  | Fsh (left, a, b, c) ->
      intrinsic cx (r 0)
        ((if left then "llvm.fshl." else "llvm.fshr.") ^ suffix (ty_of f a))
        [ arg cx a; arg cx b; arg cx c ]
  | Ovf _ -> unsupported "overflow arithmetic"
  | Icmp (k, a, b) -> def cx (r 0) (fun id _ -> Ir.Cmp (id, icmp_ir k, ir_ty (ty_of f a), v a, v b))
  | Fcmp (k, _, a, b) -> def cx (r 0) (fun id _ -> Ir.Cmp (id, fcmp_ir k, ir_ty (ty_of f a), v a, v b))
  | Select (c, a, b) -> def cx (r 0) (fun id _ -> Ir.Select (id, v c, v a, v b))
  | Freeze a -> alias cx (r 0) (v a)  (* the old backend never exploits poison *)
  | Conv (Fptosi_sat, a) | Conv (Fptoui_sat, a) ->
      let s = match ir.op with Conv (Fptosi_sat, _) -> "llvm.fptosi.sat." | _ -> "llvm.fptoui.sat." in
      intrinsic cx (r 0) (s ^ suffix (ty_of f (r 0)) ^ "." ^ suffix (ty_of f a)) [ arg cx a ]
  | Conv (k, a) ->
      let st = ir_ty (ty_of f a) in
      def cx (r 0) (fun id t -> Ir.Cast (id, conv_ir k, st, v a, t))
  | Alloca (n, align) -> (
      match const_int cx n with
      | Some n -> def cx (r 0) (fun id _ -> Ir.Alloca (id, bytes (Int64.to_int n), align))
      | None -> unsupported "variable-size alloca")
  | Load (m, p) when m.order <> Plain ->
      let t = ir_ty (ty_of f (r 0)) in
      intrinsic cx (r 0) (Ir.atomic_name ~ty:t Ir.A_load (atomic_order m.order))
        [ (Ir.Pointer Ir.I8, Ir.No_extension, v p) ]
  | Store (m, x, p) when m.order <> Plain ->
      let t = ir_ty (ty_of f x) in
      call cx None Ir.Void (Ir.atomic_name ~ty:t Ir.A_store (atomic_order m.order))
        [ (Ir.Pointer Ir.I8, Ir.No_extension, v p); arg cx x ]
  | Load (m, p) ->
      let al = mem_align m in
      def cx (r 0) (fun id t -> if m.volatile then Ir.Load_volatile (id, t, v p, al) else Ir.Load (id, t, v p, al))
  | Store (m, x, p) ->
      let al = mem_align m in
      let t = ir_ty (ty_of f x) in
      add cx (if m.volatile then Ir.Store_volatile (t, v x, v p, al) else Ir.Store (t, v x, v p, al))
  | Ptradd (_, p, idx, scale, disp) ->
      let base = v p in
      let cur =
        match const_int cx idx with
        | Some n ->
            let total = Int64.add (Int64.mul n (Int64.of_int scale)) (Int64.of_int disp) in
            if total = 0L then base
            else begin
              let id = fresh cx in
              add cx (Ir.Gep (id, Ir.I8, base, [ Ir.Index (Ir.Const (Ir.I64, total)) ]));
              Ir.Local (id, Ir.Pointer Ir.I8)
            end
        | None ->
            let xi = v idx in
            let gid = fresh cx in
            (match scale with
            | 1 | 2 | 4 | 8 ->
                let et = match scale with 1 -> Ir.I8 | 2 -> Ir.I16 | 4 -> Ir.I32 | _ -> Ir.I64 in
                add cx (Ir.Gep (gid, et, base, [ Ir.Index xi ]))
            | s ->
                let it = ir_ty (ty_of f idx) in
                let mid = fresh cx in
                add cx (Ir.Bin (mid, Ir.Mul, it, xi, Ir.Const (it, Int64.of_int s)));
                add cx (Ir.Gep (gid, Ir.I8, base, [ Ir.Index (Ir.Local (mid, it)) ])));
            let g = Ir.Local (gid, Ir.Pointer Ir.I8) in
            if disp = 0 then g
            else begin
              let id = fresh cx in
              add cx (Ir.Gep (id, Ir.I8, g, [ Ir.Index (Ir.Const (Ir.I64, Int64.of_int disp)) ]));
              Ir.Local (id, Ir.Pointer Ir.I8)
            end
      in
      alias cx (r 0) cur
  | Memcpy (vol, d, s, n) | Memmove (vol, d, s, n) -> (
      (* a whole-array load then store: the old backend copies through a
         slot, so overlap is fine *)
      match const_int cx n with
      | Some n ->
          let n = Int64.to_int n in
          let id = fresh cx in
          add cx (if vol then Ir.Load_volatile (id, bytes n, v s, 1) else Ir.Load (id, bytes n, v s, 1));
          let x = Ir.Local (id, bytes n) in
          add cx (if vol then Ir.Store_volatile (bytes n, x, v d, 1) else Ir.Store (bytes n, x, v d, 1))
      | None -> unsupported "variable-size memcpy")
  | Memset (vol, d, x, n) -> (
      match (const_int cx x, const_int cx n) with
      | Some 0L, Some n ->
          let n = Int64.to_int n in
          let z : Ir.value = Ir.Zero (bytes n) in
          add cx (if vol then Ir.Store_volatile (bytes n, z, v d, 1) else Ir.Store (bytes n, z, v d, 1))
      | _ -> unsupported "memset other than a constant-size clear")
  | Rmw (k, m, p, x) ->
      (* back to `__fas_atomic_*` pseudo-calls (Ir.atomic_name) *)
      let op =
        match k with
        | Xchg -> Ir.A_xchg
        | Radd -> Ir.A_add
        | Rsub -> Ir.A_sub
        | Rand -> Ir.A_and
        | Ror -> Ir.A_or
        | Rxor -> Ir.A_xor
        | _ -> unsupported "atomic_rmw %s" (Fir_print.rmw_name k)
      in
      let t = ir_ty (ty_of f x) in
      intrinsic cx (r 0) (Ir.atomic_name ~ty:t op (atomic_order m.order)) [ (Ir.Pointer Ir.I8, Ir.No_extension, v p); arg cx x ]
  | Cmpxchg c ->
      (* the old Ir keeps only the old value; the success flag is old = expected *)
      let t = ir_ty (ty_of f c.expected) in
      intrinsic cx (r 0) (Ir.atomic_name ~ty:t Ir.A_cas (atomic_order c.mem.order))
        [ (Ir.Pointer Ir.I8, Ir.No_extension, v c.p); arg cx c.expected; arg cx c.desired ];
      def cx (r 1) (fun id _ -> Ir.Cmp (id, Ir.Eq, t, vv cx (r 0), v c.expected))
  | Fence o -> call cx None Ir.Void (Ir.atomic_name Ir.A_fence (atomic_order o)) []
  | Extract (x, l) -> def cx (r 0) (fun id _ -> Ir.Extract (id, ir_ty (ty_of f x), v x, v l))
  | Insert (x, e, l) -> def cx (r 0) (fun id t -> Ir.Insert (id, t, v x, v l, v e))
  | Shuffle (a, b, mask) ->
      let poison_b = match const_of f b with Some (Cpoison _) -> true | _ -> false in
      if poison_b && Array.for_all (fun l -> l = 0 || l = -1) mask then
        def cx (r 0) (fun id t -> Ir.Shuffle_zero (id, t, v a))
      else
        let mt = Ir.Vector (Array.length mask, Ir.I32) in
        let lanes = Array.to_list (Array.map (fun l -> Int64.of_int (max 0 l)) mask) in
        def cx (r 0) (fun id t -> Ir.Shufflevector (id, t, v a, v b, Ir.Const_vector (mt, lanes)))
  | Splat x ->
      let vt = ir_ty (ty_of f (r 0)) in
      let id = fresh cx in
      add cx (Ir.Insert (id, vt, Ir.Undef vt, Ir.Const (Ir.I32, 0L), v x));
      def cx (r 0) (fun rid t -> Ir.Shuffle_zero (rid, t, Ir.Local (id, vt)))
  | Reduce (Red_or, x) -> intrinsic cx (r 0) ("llvm.vector.reduce.or." ^ suffix (ty_of f x)) [ arg cx x ]
  | Reduce _ -> unsupported "reduction other than or"
  | Masked_load (al, p, mask, pass) ->
      let vt = ty_of f (r 0) in
      intrinsic cx (r 0)
        ("llvm.masked.load." ^ suffix vt ^ ".p0")
        [ arg cx p; (Ir.I32, Ir.No_extension, Ir.Const (Ir.I32, Int64.of_int al)); arg cx mask; arg cx pass ]
  | Masked_store (al, x, p, mask) ->
      call cx None Ir.Void
        ("llvm.masked.store." ^ suffix (ty_of f x) ^ ".p0")
        [ arg cx x; arg cx p; (Ir.I32, Ir.No_extension, Ir.Const (Ir.I32, Int64.of_int al)); arg cx mask ]
  | Compress (x, mask) ->
      let vt = ty_of f x in
      intrinsic cx (r 0)
        ("llvm.experimental.vector.compress." ^ suffix vt)
        [ arg cx x; arg cx mask; (ir_ty vt, Ir.No_extension, (Ir.Zero (ir_ty vt) : Ir.value)) ]
  | Expand _ -> unsupported "expand"
  | Call { sig_; callee; args; _ } ->
      let nfixed = List.length sig_.param_abis in
      let ir_args =
        List.mapi
          (fun k x ->
            if k < nfixed then
              match List.nth sig_.param_abis k with
              | Agg { size; align; fields } ->
                  let ty, _ = agg_ir ~size ~align fields in
                  (ty, Ir.No_extension, load_abi_agg cx ~size ~align fields (v x))
              | a ->
                  let t, e = abi_ir a in
                  (t, e, v x)
            else arg cx x)
          (Array.to_list args)
      in
      let ret, ext = match sig_.result_abis with [] -> (Ir.Void, Ir.No_extension) | [ a ] -> abi_ir a | _ -> unsupported "multiple results" in
      let result = if ret = Ir.Void then None else Some (fresh cx) in
      add cx
        (match callee with
        | Direct name -> Ir.Call (result, ext, ret, name, ir_args)
        | Indirect p -> Ir.Call_indirect (result, ext, ret, v p, ir_args));
      (match (result, sig_.result_abis) with
      | Some id, [ Agg { size; align; fields } ] ->
          let ty, padded = agg_ir ~size ~align fields in
          let s = slot cx padded (max 8 align) in
          add cx (Ir.Store (ty, Ir.Local (id, ty), s, 1));
          alias cx (r 0) s
      | Some id, _ -> alias cx (r 0) (Ir.Local (id, ret))
      | None, _ -> ())
  | Syscall args -> def cx (r 0) (fun id _ -> Ir.Syscall (id, List.map v (Array.to_list args)))
  | Va_start p -> call cx None Ir.Void "llvm.va_start.p0" [ arg cx p ]
  | Va_arg p ->
      (* the suffix names what to read: the native backend expands these
         inline (llvm.* calls only use rax/rcx/rdx there) *)
      let suffix = match ty_of cx.f (r 0) with Int n -> "i" ^ string_of_int n | Ptr _ -> "ptr" | F32 -> "f32" | F64 -> "f64" | t -> unsupported "va_arg of %s" (ty_to_string t) in
      intrinsic cx (r 0) ("llvm.fas.va_arg." ^ suffix) [ arg cx p ]
  | Trap -> add cx Ir.Trap

let convert_func (m : modul) ~source (f : func) : Ir.func =
  let param_abis = f.sig_.param_abis in
  let params_ir = List.map abi_ir param_abis in
  let ret, ret_extension =
    match f.sig_.result_abis with [] -> (Ir.Void, Ir.No_extension) | [ a ] -> abi_ir a | _ -> unsupported "multiple results"
  in
  let used = Hashtbl.create 8 in
  let pnames =
    List.mapi
      (fun k _ ->
        let base =
          if f.defined then match Hashtbl.find_opt f.names (block f entry).params.(k) with Some n -> n | None -> "p" ^ string_of_int k
          else "p" ^ string_of_int k
        in
        let rec uniq n s = let c = if n = 0 then s else s ^ "." ^ string_of_int n in if Hashtbl.mem used c then uniq (n + 1) s else c in
        let name = uniq 0 base in
        Hashtbl.replace used name ();
        name)
      param_abis
  in
  let params = List.map2 (fun name (ty, extension) -> { Ir.name; ty; extension }) pnames params_ir in
  let linkage = match f.linkage with Local -> Ir.Internal | Global | Weak -> Ir.External in
  let blocks =
    if not f.defined then []
    else begin
      let cx =
        { m; f; map = Hashtbl.create 64; next_id = num_values f; cur = []; entry_slots = []; cur_loc = 0 }
      in
      let nb = num_blocks f in
      (* entry parameters are the function parameters *)
      let entry_prefix = ref [] in
      List.iteri
        (fun k (p : Ir.param) ->
          let fv = (block f entry).params.(k) in
          match List.nth param_abis k with
          | Agg { size; align; fields } ->
              let s = slot cx (snd (agg_ir ~size ~align fields)) (max 8 align) in
              entry_prefix := Ir.Store (p.ty, Ir.Param (p.name, p.ty), s, 1) :: !entry_prefix;
              alias cx fv s
          | _ -> alias cx fv (Ir.Param (p.name, p.ty)))
        params;
      let incoming = Hashtbl.create 16 in  (* dest -> (from, args) list, reversed *)
      let extra = ref [] in  (* trampolines *)
      let next_block = ref nb in
      let bodies =
        Array.init nb (fun b ->
            let br = block f b in
            cx.cur <- [];
            cx.cur_loc <- 0;
            Vec.iter (convert_inst cx) br.insts;
            if br.term_loc <> 0 && br.term_loc <> cx.cur_loc then begin
              let _, line, col = Vec.get m.locs br.term_loc in
              add cx (Ir.Location (line, col))
            end;
            let seen = Hashtbl.create 4 in
            let edge (t : target) =
              let args = Array.map (vv cx) t.args in
              let from =
                match Hashtbl.find_opt seen t.dest with
                | None ->
                    Hashtbl.replace seen t.dest args;
                    b
                | Some first when first = args -> -1
                | Some _ ->
                    let tb = !next_block in
                    incr next_block;
                    extra := { Ir.id = tb; label = "b" ^ string_of_int tb; instrs = []; terminator = Ir.Br t.dest } :: !extra;
                    Hashtbl.replace incoming t.dest ((tb, args) :: (try Hashtbl.find incoming t.dest with Not_found -> []));
                    tb
              in
              if from = b then
                Hashtbl.replace incoming t.dest ((b, args) :: (try Hashtbl.find incoming t.dest with Not_found -> []));
              (* the branch goes to the destination itself, or to the
                 trampoline standing in for it *)
              if from = b || from = -1 then t.dest else from
            in
            let term =
              match br.term with
              | Jump t -> Ir.Br (edge t)
              | Br (c, t, e, _) ->
                  let c = vv cx c in
                  let t = edge t in
                  let e = edge e in
                  Ir.CondBr (c, t, e)
              | Switch (x, d, cases) ->
                  let xt = ty_of f x in
                  let xv = vv cx x in
                  let d = edge d in
                  let cases = Array.to_list (Array.map (fun (k, t) -> (limb0 k, edge t)) cases) in
                  ignore xt;
                  Ir.Switch (ir_ty (ty_of f x), xv, cases, d)
              | Ret [||] -> Ir.Ret None
              | Ret [| x |] -> (
                  match f.sig_.result_abis with
                  | [ Agg { size; align; fields } ] ->
                      let ty, _ = agg_ir ~size ~align fields in
                      Ir.Ret (Some (ty, load_abi_agg cx ~size ~align fields (vv cx x)))
                  | _ -> Ir.Ret (Some (ir_ty (ty_of f x), vv cx x)))
              | Ret _ -> unsupported "multiple return values"
              | Unreachable -> Ir.Unreachable
              | Indirect_br _ -> unsupported "indirect branch"
            in
            (List.rev cx.cur, term))
      in
      let block_of b =
        let br = block f b in
        let instrs, terminator = bodies.(b) in
        let phis =
          if b = entry then []
          else
            let inc = List.rev (try Hashtbl.find incoming b with Not_found -> []) in
            Array.to_list
              (Array.mapi
                 (fun k p -> Ir.Phi (p, ir_ty (ty_of f p), List.map (fun (from, args) -> (args.(k), from)) inc))
                 br.params)
        in
        let instrs = if b = entry then List.rev cx.entry_slots @ List.rev !entry_prefix @ instrs else instrs in
        { Ir.id = b; label = "b" ^ string_of_int b; instrs = phis @ instrs; terminator }
      in
      List.init nb block_of @ List.rev !extra
    end
  in
  { Ir.name = f.name; params; ret; ret_extension; blocks; linkage; variadic = f.sig_.variadic; source; display = f.display }

let convert_global (g : global) : Ir.global =
  let linkage =
    if not g.gdefined then if g.readonly then Ast.Import_const_c else Ast.Import_c
    else match g.glinkage with Local -> Ast.Internal_global | Global | Weak -> Ast.Export_c
  in
  let bytes_, pointers =
    match g.init with
    | None -> (None, [])
    | Some pieces ->
        let buf = Buffer.create g.size and ptrs = ref [] in
        List.iter
          (function
            | Bytes s -> Buffer.add_string buf s
            | Zeros n -> Buffer.add_string buf (String.make n '\000')
            | Reloc { sym; addend; width } ->
                if width <> 8 then unsupported "relocation of width %d" width;
                ptrs := (Buffer.length buf, sym, Int64.to_int addend) :: !ptrs;
                Buffer.add_string buf (String.make 8 '\000'))
          pieces;
        (Some (Buffer.contents buf), List.rev !ptrs)
  in
  Ir.Storage_global
    { name = g.gname; storage_ty = bytes g.size; size = g.size; bytes = bytes_; pointers;
      readonly = g.readonly; align = g.galign; linkage }

(* [like] supplies what Fir does not carry: the data layout and the source
   file of each function *)
let convert ~(like : Ir.module_) (m : modul) : Ir.module_ =
  let sources = Hashtbl.create 64 in
  Hashtbl.reset sysv_structs;
  List.iter (fun (f : Ir.func) -> Hashtbl.replace sources f.name f.source) like.funcs;
  let funcs = ref [] and globals = ref [] in
  Vec.iter
    (fun f ->
      let source = Option.join (Hashtbl.find_opt sources f.name) in
      funcs := convert_func m ~source f :: !funcs)
    m.funcs;
  Vec.iter (fun g -> globals := convert_global g :: !globals) m.globals;
  let names p = Vec.fold (fun acc f -> if p f then f.name :: acc else acc) [] m.funcs |> List.rev in
  { like with
    Ir.target_triple = m.triple;
    structs = List.of_seq (Hashtbl.to_seq_values sysv_structs);
    globals = List.rev !globals;
    funcs = List.rev !funcs;
    no_inline_functions = names (fun f -> f.fattrs land fa_noinline <> 0);
    inline_functions = names (fun f -> f.fattrs land fa_inline <> 0) }
