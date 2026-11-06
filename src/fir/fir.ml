(* Fir: the Fas intermediate representation. See SPEC.md next to this file.

   A function owns dense tables:
   - values: what defines each value (instruction result, block parameter,
     constant, or an alias left behind by replace-all-uses) and its type;
   - instructions: an [op] plus the values it defines;
   - blocks: parameters, an ordered list of instruction ids, a terminator.

   Nothing here is global: a module and its functions carry every table, so
   functions can later be compiled independently and in parallel. *)

module Vec = Fir_vec

(* ---------- types ---------- *)

type ty =
  | Int of int  (** iN, 1 <= N <= 8_388_608 *)
  | F16
  | F32
  | F64
  | F80
  | Ptr of int  (** address space *)
  | Vec of int * ty  (** lanes, element (integer, float or pointer) *)

let i1 = Int 1
let i8 = Int 8
let i16 = Int 16
let i32 = Int 32
let i64 = Int 64
let i128 = Int 128
let ptr = Ptr 0
let max_int_width = 8_388_608

let is_int = function Int _ -> true | _ -> false
let is_float = function F16 | F32 | F64 | F80 -> true | _ -> false
let is_ptr = function Ptr _ -> true | _ -> false
let elem = function Vec (_, t) -> t | t -> t
let lanes = function Vec (n, _) -> n | _ -> 1
let int_width = function Int n -> n | Vec (_, Int n) -> n | _ -> 0

(* Same shape (scalar or vector of the same lane count) with another element *)
let with_elem ty e = match ty with Vec (n, _) -> Vec (n, e) | _ -> e

(* Storage size in bytes, as laid out in memory (f80 takes 16 bytes,
   integers round up to a power of two up to 16 bytes, then to 8-byte limbs) *)
let rec size_of = function
  | Int n when n <= 8 -> 1
  | Int n when n <= 16 -> 2
  | Int n when n <= 32 -> 4
  | Int n when n <= 64 -> 8
  | Int n when n <= 128 -> 16
  | Int n -> (n + 63) / 64 * 8
  | F16 -> 2
  | F32 -> 4
  | F64 -> 8
  | F80 -> 16
  | Ptr _ -> 8
  | Vec (n, Int 1) -> (n + 7) / 8
  | Vec (n, t) -> n * size_of t

let rec ty_to_string = function
  | Int n -> "i" ^ string_of_int n
  | F16 -> "f16"
  | F32 -> "f32"
  | F64 -> "f64"
  | F80 -> "f80"
  | Ptr 0 -> "ptr"
  | Ptr k -> Printf.sprintf "ptr(%d)" k
  | Vec (n, t) -> Printf.sprintf "<%d x %s>" n (ty_to_string t)

(* ---------- ids ---------- *)

type value = int
type inst = int
type block = int

(* ---------- operations ---------- *)

type ibin =
  | Add
  | Sub
  | Mul
  | Udiv
  | Sdiv
  | Urem
  | Srem
  | Shl
  | Lshr
  | Ashr
  | And
  | Or
  | Xor
  | Smin
  | Smax
  | Umin
  | Umax
  | Smulh
  | Umulh
  | Sadd_sat
  | Uadd_sat
  | Ssub_sat
  | Usub_sat

type iun = Ctz | Clz | Popcnt | Bswap | Bitrev | Abs
type fbin = Fadd | Fsub | Fmul | Fdiv | Frem | Fmin | Fmax | Copysign
type fun_ = Fneg | Fabs | Sqrt | Floor | Ceil | Ftrunc | Roundeven

(* overflow-checked arithmetic: results are the value and an i1 flag *)
type ovf = Sadd_ovf | Uadd_ovf | Ssub_ovf | Usub_ovf | Smul_ovf | Umul_ovf
type icmp = Eq | Ne | Slt | Sle | Sgt | Sge | Ult | Ule | Ugt | Uge

type fcmp =
  | Ffalse
  | Foeq
  | Fogt
  | Foge
  | Folt
  | Fole
  | Fone
  | Ford
  | Fueq
  | Fugt
  | Fuge
  | Fult
  | Fule
  | Fune
  | Funo
  | Ftrue

type conv =
  | Trunc
  | Zext
  | Sext
  | Fptrunc
  | Fpext
  | Fptosi
  | Fptoui
  | Fptosi_sat
  | Fptoui_sat
  | Sitofp
  | Uitofp
  | Ptrtoint
  | Inttoptr
  | Bitcast

type ordering = Plain | Relaxed | Acquire | Release | Acq_rel | Seq_cst
type rmw = Xchg | Radd | Rsub | Rand | Ror | Rxor | Rmin | Rmax | Rumin | Rumax | Rfadd | Rfsub

type reduce =
  | Red_add
  | Red_mul
  | Red_and
  | Red_or
  | Red_xor
  | Red_smin
  | Red_smax
  | Red_umin
  | Red_umax
  | Red_fmin
  | Red_fmax
  | Red_fadd_ordered

(* Flags: a bit set shared by every instruction kind; the verifier checks
   that each instruction only carries flags that mean something for it. *)
let f_nsw = 1
let f_nuw = 2
let f_exact = 4
let f_disjoint = 8
let f_inbounds = 16
let f_zero_poison = 32
let f_contract = 64
let f_fast = 128
let f_poison_min = 256
let has flags f = flags land f <> 0

(* Memory access attributes. [alias] is 0 for "no information", otherwise an
   index into the module's alias-tag table. [noalias] lists the noalias
   scopes the access belongs to. *)
type mem = { align : int; volatile : bool; order : ordering; alias : int; noalias : int list }

let plain_mem align = { align; volatile = false; order = Plain; alias = 0; noalias = [] }

(* ABI types of call parameters and results (SPEC section 7) *)
type ext = Zx | Sx

type abi =
  | Scalar of ty
  | Ext of ty * ext
  | Agg of { size : int; align : int; fields : (int * ty) list }

type signature = { param_abis : abi list; result_abis : abi list; variadic : bool }
type callee = Direct of string | Indirect of value

(* call attributes *)
let a_noreturn = 1
let a_returns_twice = 2
let a_nounwind = 4
let a_tail = 8

type op =
  | Nop  (** removed instruction, dropped by [compact] *)
  | Ibin of ibin * int * value * value
  | Iun of iun * int * value
  | Fbin of fbin * int * value * value
  | Fun of fun_ * int * value
  | Fma of int * value * value * value
  | Fsh of bool * value * value * value  (** true = fshl *)
  | Ovf of ovf * value * value
  | Icmp of icmp * value * value
  | Fcmp of fcmp * int * value * value
  | Select of value * value * value
  | Freeze of value
  | Conv of conv * value  (** result type is the instruction's *)
  | Alloca of value * int  (** size in bytes, alignment *)
  | Load of mem * value
  | Store of mem * value * value  (** value, pointer *)
  | Ptradd of int * value * value * int * int
      (** flags, pointer, index, scale, displacement: p + idx * scale + disp;
          idx may be a zero constant *)
  | Memcpy of bool * value * value * value  (** volatile, dst, src, size *)
  | Memmove of bool * value * value * value
  | Memset of bool * value * value * value  (** volatile, dst, byte (i8), size *)
  | Rmw of rmw * mem * value * value  (** pointer, operand *)
  | Cmpxchg of { mem : mem; fail : ordering; weak : bool; p : value; expected : value; desired : value }
  | Fence of ordering
  | Extract of value * value
  | Insert of value * value * value  (** vector, element, index *)
  | Shuffle of value * value * int array  (** -1 = poison lane *)
  | Splat of value
  | Reduce of reduce * value
  | Masked_load of int * value * value * value  (** align, pointer, mask, passthru *)
  | Masked_store of int * value * value * value  (** align, value, pointer, mask *)
  | Compress of value * value  (** vector, mask *)
  | Expand of value * value
  | Call of { sig_ : signature; callee : callee; args : value array; attrs : int }
  | Syscall of value array
  | Va_start of value
      (** fills the va_list at the pointer so it reads the variadic arguments of
          the current (variadic) function; x86-64 SysV va_list: 24 bytes, align 8 *)
  | Va_arg of value
      (** reads the next variadic argument through the va_list at the pointer
          and advances it; the result type says what to read (int, ptr, float) *)
  | Stack_save
      (** the current stack pointer, as a ptr for [Stack_restore]: C front ends
          bracket a block holding variable-size allocas (VLAs) with the pair *)
  | Stack_restore of value
      (** frees every alloca made since the [Stack_save] that gave the ptr *)
  | Trap
  | Dbg_value of int * value  (** variable id, value *)

type target = { dest : block; args : value array }

type term =
  | Jump of target
  | Br of value * target * target * int  (** weight of the true edge in 1/1000, -1 = unknown *)
  | Switch of value * target * (int64 array * target) array  (** case keys as limbs *)
  | Indirect_br of value * block array
  | Ret of value array
  | Unreachable

(* ---------- constants ---------- *)

(* Integer constants are little-endian 64-bit limbs, normalized: exactly
   ceil(width / 64) limbs, bits above the width cleared. Floats are kept as
   exact bit patterns in the same limb form. *)
type const =
  | Cint of ty * int64 array
  | Cfloat of ty * int64 array
  | Cnull of ty
  | Cpoison of ty
  | Czero of ty
  | Cvec of ty * value array  (** lanes are constant values *)
  | Csym of string * int64  (** address of a global or function, plus addend *)
  | Cblockaddr of string * block

let limbs_for width = (width + 63) / 64

let normalize_limbs width (l : int64 array) =
  let n = limbs_for width in
  let a = Array.init n (fun i -> if i < Array.length l then l.(i) else 0L) in
  let top = width mod 64 in
  if top <> 0 then a.(n - 1) <- Int64.logand a.(n - 1) (Int64.pred (Int64.shift_left 1L top));
  a

(* Sign-extends an int64 into [width] bits worth of limbs *)
let limbs_of_int64 width (x : int64) =
  let n = limbs_for width in
  let fill = if Int64.compare x 0L < 0 then -1L else 0L in
  normalize_limbs width (Array.init n (fun i -> if i = 0 then x else fill))

(* ---------- function and module ---------- *)

type def =
  | Result of inst * int
  | Param of block * int
  | Const of const
  | Alias of value  (** replaced: read the target instead *)
  | Unset

type inst_rec = { mutable op : op; mutable results : value array; mutable loc : int }

type block_rec = {
  mutable params : value array;
  mutable insts : inst Vec.t;
  mutable term : term;
  mutable term_loc : int;  (** source location of the terminator *)
  mutable bname : string;  (** label for printing; may be empty *)
}

type linkage = Local | Global | Weak
type visibility = Default | Hidden | Protected

(* function attributes *)
let fa_inline = 1
let fa_noinline = 2
let fa_cold = 4
let fa_noreturn = 8

type func = {
  name : string;
  sig_ : signature;
  mutable linkage : linkage;
  mutable visibility : visibility;
  mutable fattrs : int;
  mutable defined : bool;  (** false: declaration only *)
  defs : def Vec.t;
  tys : ty Vec.t;
  insts : inst_rec Vec.t;
  blocks : block_rec Vec.t;
  consts : (const, value) Hashtbl.t;
  names : (value, string) Hashtbl.t;  (** optional value names for printing *)
  mutable display : string option;  (** source-level name *)
}

type init_piece =
  | Bytes of string
  | Zeros of int
  | Reloc of { sym : string; addend : int64; width : int }  (** width 4 or 8 *)

type global = {
  gname : string;
  mutable glinkage : linkage;
  mutable gvisibility : visibility;
  mutable section : string option;
  mutable galign : int;
  mutable readonly : bool;
  mutable thread_local : bool;
  mutable size : int;
  mutable init : init_piece list option;  (** None = zero-filled, defined *)
  mutable gdefined : bool;  (** false: external declaration *)
}

type modul = {
  funcs : func Vec.t;
  globals : global Vec.t;
  locs : (string * int * int) Vec.t;  (** location id -> file, line, column; 0 = none *)
  alias_tags : (string * int) Vec.t;  (** tag name, parent tag (0 = root) *)
  mutable strict_aliasing : bool;
  mutable triple : string;
}

let dummy_block = { params = [||]; insts = Vec.create 0; term = Unreachable; term_loc = 0; bname = "" }
let dummy_inst = { op = Nop; results = [||]; loc = 0 }

let create_module () =
  let locs = Vec.create ("", 0, 0) in
  ignore (Vec.push locs ("", 0, 0));
  let alias_tags = Vec.create ("", 0) in
  ignore (Vec.push alias_tags ("", 0));
  {
    funcs = Vec.create { name = ""; sig_ = { param_abis = []; result_abis = []; variadic = false };
                         linkage = Local; visibility = Default; fattrs = 0; defined = false;
                         defs = Vec.create Unset; tys = Vec.create i1; insts = Vec.create dummy_inst;
                         blocks = Vec.create dummy_block; consts = Hashtbl.create 1;
                         names = Hashtbl.create 1; display = None };
    globals =
      Vec.create
        { gname = ""; glinkage = Local; gvisibility = Default; section = None; galign = 1;
          readonly = false; thread_local = false; size = 0; init = None; gdefined = false };
    locs;
    alias_tags;
    strict_aliasing = true;
    triple = "x86_64-unknown-linux-gnu";
  }

let new_func ?(linkage = Global) ?(defined = true) name sig_ =
  {
    name;
    sig_;
    linkage;
    visibility = Default;
    fattrs = 0;
    defined;
    defs = Vec.create ~capacity:64 Unset;
    tys = Vec.create ~capacity:64 i1;
    insts = Vec.create ~capacity:64 dummy_inst;
    blocks = Vec.create ~capacity:8 dummy_block;
    consts = Hashtbl.create 16;
    names = Hashtbl.create 16;
    display = None;
  }

let add_func m f = ignore (Vec.push m.funcs f)
let add_global m g = ignore (Vec.push m.globals g)

let add_loc m file line col =
  Vec.push m.locs (file, line, col)

(* ---------- values ---------- *)

let new_value f def ty =
  let v = Vec.push f.defs def in
  ignore (Vec.push f.tys ty);
  v

let num_values f = Vec.length f.defs
let ty_of f v = Vec.get f.tys v
let def_of f v = Vec.get f.defs v

(* Follows replace-all-uses aliases to the value that is really meant *)
let rec resolve f v = match Vec.get f.defs v with Alias w -> resolve f w | _ -> v

(* Every later read of [v] means [w] *)
let replace_all_uses f v w =
  if v <> w then begin
    let w = resolve f w in
    if v = w then invalid_arg "Fir.replace_all_uses: cycle";
    Vec.set f.defs v (Alias w)
  end

let const f c ty =
  match Hashtbl.find_opt f.consts c with
  | Some v -> v
  | None ->
      let v = new_value f (Const c) ty in
      Hashtbl.add f.consts c v;
      v

let const_of f v = match def_of f (resolve f v) with Const c -> Some c | _ -> None

let iconst f ty (x : int64) =
  match ty with
  | Int w -> const f (Cint (ty, limbs_of_int64 w x)) ty
  | _ -> invalid_arg "Fir.iconst"

let iconst_limbs f ty limbs =
  match ty with
  | Int w -> const f (Cint (ty, normalize_limbs w limbs)) ty
  | _ -> invalid_arg "Fir.iconst_limbs"

let fconst_bits f ty (bits : int64 array) = const f (Cfloat (ty, bits)) ty
let f64const f x = fconst_bits f F64 [| Int64.bits_of_float x |]

let f32const f x =
  fconst_bits f F32 [| Int64.logand (Int64.of_int32 (Int32.bits_of_float x)) 0xffffffffL |]

let null f ty = const f (Cnull ty) ty
let poison f ty = const f (Cpoison ty) ty
let zero f ty = const f (Czero ty) ty
let sym f ?(addend = 0L) name = const f (Csym (name, addend)) ptr
let vconst f ty lanes = const f (Cvec (ty, lanes)) ty

(* An integer constant as int64 (low limb, sign-extended from its width) *)
let int_value f v =
  match const_of f v with
  | Some (Cint (Int w, l)) ->
      let x = l.(0) in
      if w >= 64 then Some x
      else
        let s = 64 - w in
        Some (Int64.shift_right (Int64.shift_left x s) s)
  | Some (Czero (Int _)) -> Some 0L
  | _ -> None

(* ---------- blocks and instructions ---------- *)

let add_block ?(name = "") f =
  Vec.push f.blocks { params = [||]; insts = Vec.create ~capacity:8 0; term = Unreachable; term_loc = 0; bname = name }

let block f b = Vec.get f.blocks b
let num_blocks f = Vec.length f.blocks
let entry = 0

let add_param f b ty =
  let br = block f b in
  let v = new_value f (Param (b, Array.length br.params)) ty in
  br.params <- Array.append br.params [| v |];
  v

let inst f i = Vec.get f.insts i

(* Creates an instruction defining values of [tys], without placing it *)
let make_inst ?(loc = 0) f op tys =
  let i = Vec.push f.insts { op; results = [||]; loc } in
  let rs = Array.mapi (fun k ty -> new_value f (Result (i, k)) ty) tys in
  (Vec.get f.insts i).results <- rs;
  i

(* Appends an instruction to block [b] and returns its results *)
let emit ?loc f b op tys =
  let i = make_inst ?loc f op tys in
  ignore (Vec.push (block f b).insts i);
  (Vec.get f.insts i).results

let emit1 ?loc f b op ty = (emit ?loc f b op [| ty |]).(0)
let emit0 ?loc f b op = ignore (emit ?loc f b op [||])
let set_term ?(loc = 0) f b t =
  let br = block f b in
  br.term <- t;
  br.term_loc <- loc
let target ?(args = [||]) dest = { dest; args }

(* ---------- traversal ---------- *)

(* Calls [g] on every value an op reads *)
let iter_op_uses g = function
  | Nop | Trap | Fence _ | Stack_save -> ()
  | Ibin (_, _, a, b) | Fbin (_, _, a, b) | Ovf (_, a, b) | Icmp (_, a, b) | Fcmp (_, _, a, b)
  | Extract (a, b) | Compress (a, b) | Expand (a, b) | Store (_, a, b) | Rmw (_, _, a, b)
  | Shuffle (a, b, _) ->
      g a;
      g b
  | Iun (_, _, a) | Fun (_, _, a) | Freeze a | Conv (_, a) | Alloca (a, _) | Load (_, a)
  | Splat a | Reduce (_, a) | Dbg_value (_, a) | Va_start a | Va_arg a | Stack_restore a ->
      g a
  | Fma (_, a, b, c) | Fsh (_, a, b, c) | Select (a, b, c) | Insert (a, b, c)
  | Memcpy (_, a, b, c) | Memmove (_, a, b, c) | Memset (_, a, b, c)
  | Masked_load (_, a, b, c) | Masked_store (_, a, b, c) ->
      g a;
      g b;
      g c
  | Ptradd (_, p, i, _, _) ->
      g p;
      g i
  | Cmpxchg { p; expected; desired; _ } ->
      g p;
      g expected;
      g desired
  | Call { callee; args; _ } ->
      (match callee with Indirect v -> g v | Direct _ -> ());
      Array.iter g args
  | Syscall args -> Array.iter g args

let iter_term_uses g = function
  | Jump t -> Array.iter g t.args
  | Br (c, t, e, _) ->
      g c;
      Array.iter g t.args;
      Array.iter g e.args
  | Switch (v, d, cases) ->
      g v;
      Array.iter g d.args;
      Array.iter (fun (_, t) -> Array.iter g t.args) cases
  | Indirect_br (v, _) -> g v
  | Ret vs -> Array.iter g vs
  | Unreachable -> ()

(* Rewrites every value an op reads *)
let map_op_uses g op =
  match op with
  | Nop | Trap | Fence _ | Stack_save -> op
  | Ibin (k, fl, a, b) -> Ibin (k, fl, g a, g b)
  | Iun (k, fl, a) -> Iun (k, fl, g a)
  | Fbin (k, fl, a, b) -> Fbin (k, fl, g a, g b)
  | Fun (k, fl, a) -> Fun (k, fl, g a)
  | Fma (fl, a, b, c) -> Fma (fl, g a, g b, g c)
  | Fsh (l, a, b, c) -> Fsh (l, g a, g b, g c)
  | Ovf (k, a, b) -> Ovf (k, g a, g b)
  | Icmp (k, a, b) -> Icmp (k, g a, g b)
  | Fcmp (k, fl, a, b) -> Fcmp (k, fl, g a, g b)
  | Select (a, b, c) -> Select (g a, g b, g c)
  | Freeze a -> Freeze (g a)
  | Conv (k, a) -> Conv (k, g a)
  | Alloca (a, al) -> Alloca (g a, al)
  | Load (m, p) -> Load (m, g p)
  | Store (m, v, p) -> Store (m, g v, g p)
  | Ptradd (fl, p, i, s, d) -> Ptradd (fl, g p, g i, s, d)
  | Memcpy (vol, a, b, c) -> Memcpy (vol, g a, g b, g c)
  | Memmove (vol, a, b, c) -> Memmove (vol, g a, g b, g c)
  | Memset (vol, a, b, c) -> Memset (vol, g a, g b, g c)
  | Rmw (k, m, p, v) -> Rmw (k, m, g p, g v)
  | Cmpxchg c -> Cmpxchg { c with p = g c.p; expected = g c.expected; desired = g c.desired }
  | Extract (a, b) -> Extract (g a, g b)
  | Insert (a, b, c) -> Insert (g a, g b, g c)
  | Shuffle (a, b, m) -> Shuffle (g a, g b, m)
  | Splat a -> Splat (g a)
  | Reduce (k, a) -> Reduce (k, g a)
  | Masked_load (al, a, b, c) -> Masked_load (al, g a, g b, g c)
  | Masked_store (al, a, b, c) -> Masked_store (al, g a, g b, g c)
  | Compress (a, b) -> Compress (g a, g b)
  | Expand (a, b) -> Expand (g a, g b)
  | Call c ->
      Call
        {
          c with
          callee = (match c.callee with Indirect v -> Indirect (g v) | d -> d);
          args = Array.map g c.args;
        }
  | Syscall args -> Syscall (Array.map g args)
  | Va_start a -> Va_start (g a)
  | Va_arg a -> Va_arg (g a)
  | Stack_restore a -> Stack_restore (g a)
  | Dbg_value (var, a) -> Dbg_value (var, g a)

let map_target g t = { t with args = Array.map g t.args }

let map_term_uses g = function
  | Jump t -> Jump (map_target g t)
  | Br (c, t, e, w) -> Br (g c, map_target g t, map_target g e, w)
  | Switch (v, d, cases) ->
      Switch (g v, map_target g d, Array.map (fun (k, t) -> (k, map_target g t)) cases)
  | Indirect_br (v, bs) -> Indirect_br (g v, bs)
  | Ret vs -> Ret (Array.map g vs)
  | Unreachable -> Unreachable

let successors = function
  | Jump t -> [ t.dest ]
  | Br (_, t, e, _) -> [ t.dest; e.dest ]
  | Switch (_, d, cases) -> d.dest :: Array.to_list (Array.map (fun (_, t) -> t.dest) cases)
  | Indirect_br (_, bs) -> Array.to_list bs
  | Ret _ | Unreachable -> []

(* Ops with effects beyond their results: never removed when unused *)
let has_side_effects = function
  | Store _ | Memcpy _ | Memmove _ | Memset _ | Rmw _ | Cmpxchg _ | Fence _ | Masked_store _
  | Call _ | Syscall _ | Trap | Dbg_value _ | Va_start _ | Va_arg _ | Stack_save | Stack_restore _ ->
      true
  | Load (m, _) -> m.volatile || m.order <> Plain
  | _ -> false

(* Rewrites aliased operands to their final values everywhere, so later
   passes and printers see no aliases *)
let resolve_all f =
  let r = resolve f in
  Vec.iter (fun (ir : inst_rec) -> ir.op <- map_op_uses r ir.op) f.insts;
  Vec.iter (fun (b : block_rec) -> b.term <- map_term_uses r b.term) f.blocks

(* Removes [Nop]s from blocks *)
let drop_nops f =
  Vec.iter
    (fun (b : block_rec) ->
      let keep = Vec.create ~capacity:(Vec.length b.insts) 0 in
      Vec.iter (fun i -> if (inst f i).op <> Nop then ignore (Vec.push keep i)) b.insts;
      b.insts <- keep)
    f.blocks
