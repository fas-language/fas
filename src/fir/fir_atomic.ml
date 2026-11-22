(* Fir_atomic: rewrites, in place, the atomic accesses that neither
   backend can do as one instruction into ones they can. Both backends
   run it first (Fir_wide.run calls it; Fir_llvm.render too: LLVM takes
   atomics only on power-of-two byte-sized integers).

   - Odd widths up to 64 bits (i1, i7, i33, ...): the access works on the
     containing i8/i16/i32/i64, with the bits above N zero, as every other
     Fir store leaves them. load/store/xchg/and/or/xor/cmpxchg become one
     access of the container; add/sub/min/max/umin/umax a compare-and-swap
     loop (the old value is the container's, so bits above N that some
     other writer left set cannot make the loop spin).
   - 16 bytes at a 16-aligned address (i65..i128, a 16-byte vector without
     pointer lanes): every access becomes a 128-bit cmpxchg, which both
     backends do inline as lock cmpxchg16b (the CPU needs CMPXCHG16B,
     x86-64-v2; Fir_wide gives the native backend a cmpxchg2, the LLVM path
     sets +cx16). A load is cmpxchg(0, 0) (so it writes: the memory must be
     writable, as clang -mcx16 and libatomic need too), a store, xchg or
     arithmetic a compare-and-swap loop whose first guess is 0. A narrower
     integer works on the i128 container (bits above N zero), a vector on
     its 128-bit image. libatomic does 16-byte objects the same way on such
     CPUs, so this code and C code using libatomic stay atomic together.
   - Otherwise wider than 64 bits, and f80: libatomic's generic entry points
     (__atomic_load, __atomic_store, __atomic_exchange,
     __atomic_compare_exchange) on a stack copy, the arithmetic as a
     compare-exchange loop. clang does the same for _Atomic __int128,
     _BitInt(N > 64) and long double (without -mcx16, libcalls __atomic_*_16
     or the generic ones, which share libatomic's implementation), so Fir
     code and C code updating one object stay atomic with respect to each
     other. Such programs link libatomic (-latomic), as their C versions do.
     The copies are zeroed first: libatomic compares whole objects, so
     padding (f80's six bytes, the top of an i100) is kept zero, as clang
     clears it.
   - Vectors (also before Fir_scalarize, which calls this): one of 1, 2, 4
     or 8 bytes without pointer lanes works on the integer of its bytes
     (see [how_of]); any other through libatomic on its bytes, as above.
     The arithmetic is lane-wise, cmpxchg compares bit for bit.

   The block holding a rewritten loop is split: the instructions after the
   access and the terminator move to a new block, the old results become
   aliases of the new ones (Fir.replace_all_uses). *)

open Fir

let legal = function 8 | 16 | 32 | 64 -> true | _ -> false

(* how an atomic access of [ty] is done: on an integer container of
   [bits], or by libatomic on an object of [bytes] *)
type how = Native | Narrow of int | Lib of int | Image of int | Cas16

(* A vector of 1, 2, 4 or 8 bytes (without pointer lanes) is an [Image]:
   the access works on the integer of its bytes, bitcast to and from the
   vector (a bool vector's bits above its lanes zero, as for an odd-width
   integer); and/or/xor/xchg are bitwise there, the other operations a
   lane-wise compare-and-swap loop. Any other vector goes through
   libatomic on its bytes, like a wide integer. *)
let how_of ~align = function
  | Int n when n > 64 && n <= 128 && align >= 16 -> Cas16
  | Vec (_, t) as v when size_of v = 16 && align >= 16 && (match t with Ptr _ -> false | _ -> true) -> Cas16
  | Int n when n <= 64 && not (legal n) -> Narrow (8 * size_of (Int n))
  | Int n when n > 64 -> Lib (size_of (Int n))
  | F80 -> Lib 16
  | Vec (_, Ptr _) as t -> Lib (size_of t)
  | Vec _ as t when legal (8 * size_of t) -> Image (8 * size_of t)
  | Vec _ as t -> Lib (size_of t)
  | _ -> Native

(* C11's memory_order numbering, what libatomic takes *)
let order_num = function Plain | Relaxed -> 0 | Acquire -> 2 | Release -> 3 | Acq_rel -> 4 | Seq_cst -> 5

(* the failure ordering of a compare-exchange that implements an rmw *)
let fail_of = function Release -> Relaxed | Acq_rel -> Acquire | o -> o

let i8 = Int 8
let i32 = Int 32

let libatomic =
  let s params result = { param_abis = List.map (fun t -> Scalar t) params; result_abis = result; variadic = false; ms_abi = false; ms_xmm_result = false } in
  [
    ("__atomic_load", s [ i64; ptr; ptr; i32 ] []);
    ("__atomic_store", s [ i64; ptr; ptr; i32 ] []);
    ("__atomic_exchange", s [ i64; ptr; ptr; ptr; i32 ] []);
    ("__atomic_compare_exchange", s [ i64; ptr; ptr; ptr; i32; i32 ] [ Ext (i8, Zx) ]);
  ]

type cx = {
  m : modul;
  f : func;
  mutable cur : int;  (** the block code is appended to *)
  mutable loc : int;
}

let e cx op ty = emit1 ~loc:cx.loc cx.f cx.cur op ty
let e0 cx op = emit0 ~loc:cx.loc cx.f cx.cur op
let c64 cx x = iconst cx.f i64 (Int64.of_int x)
let c32 cx x = iconst cx.f i32 (Int64.of_int x)

let declare cx name =
  let present = ref false in
  Vec.iter (fun (g : func) -> if g.name = name then present := true) cx.m.funcs;
  if not !present then add_func cx.m (new_func ~defined:false name (List.assoc name libatomic))

let call cx name args =
  declare cx name;
  let sig_ = List.assoc name libatomic in
  let tys = Array.of_list (List.map (function Scalar t | Ext (t, _) -> t | Agg _ | Cplx _ -> ptr) sig_.result_abis) in
  emit ~loc:cx.loc cx.f cx.cur (Call { sig_; callee = Direct name; args; attrs = a_nounwind; va_aggs = [] }) tys

(* a zeroed stack object of [bytes], 16-aligned *)
let slot cx bytes =
  let p = emit1 cx.f entry (Alloca (c64 cx bytes, 16)) ptr in
  p

let zero cx p bytes = e0 cx (Memset (false, p, iconst cx.f i8 0L, c64 cx bytes))

(* a zeroed copy of [v] *)
let copy_of cx bytes v =
  let p = slot cx bytes in
  zero cx p bytes;
  e0 cx (Store (plain_mem 16, v, p));
  p

(* the new value of an rmw from the old one *)
let rmw_value cx k ty old v =
  let cond = match ty with Vec (n, _) -> Vec (n, Int 1) | _ -> i1 in
  let pick c = e cx (Select (e cx (Icmp (c, old, v)) cond, old, v)) ty in
  match k with
  | Xchg -> v
  | Radd -> e cx (Ibin (Add, 0, old, v)) ty
  | Rsub -> e cx (Ibin (Sub, 0, old, v)) ty
  | Rand -> e cx (Ibin (And, 0, old, v)) ty
  | Ror -> e cx (Ibin (Or, 0, old, v)) ty
  | Rxor -> e cx (Ibin (Xor, 0, old, v)) ty
  | Rmin -> pick Slt
  | Rmax -> pick Sgt
  | Rumin -> pick Ult
  | Rumax -> pick Ugt
  | Rfadd -> e cx (Fbin (Fadd, 0, old, v)) ty
  | Rfsub -> e cx (Fbin (Fsub, 0, old, v)) ty

(* starts a loop block after the current one; returns it and the block
   the loop leaves to *)
let loop_blocks cx = (add_block cx.f, add_block cx.f)

(* the integer image of a vector [v] of type [ty] on [c] bits, and back *)
let to_image cx ty c v =
  match ty with
  | Vec (n, Int 1) when n < c -> e cx (Conv (Zext, e cx (Conv (Bitcast, v)) (Int n))) (Int c)
  | _ -> e cx (Conv (Bitcast, v)) (Int c)

let of_image cx ty c x =
  match ty with
  | Vec (n, Int 1) when n < c -> e cx (Conv (Bitcast, e cx (Conv (Trunc, x)) (Int n))) ty
  | _ -> e cx (Conv (Bitcast, x)) ty

(* Cas16: a value of [ty] as the i128 a 16-byte cmpxchg works on, and back *)
let into16 cx ty v =
  match ty with
  | Int 128 -> v
  | Int _ -> e cx (Conv (Zext, v)) (Int 128)
  | _ -> to_image cx ty 128 v

let out16 cx ty x =
  match ty with
  | Int 128 -> x
  | Int _ -> e cx (Conv (Trunc, x)) ty
  | _ -> of_image cx ty 128 x

(* a strong 128-bit compare-exchange: (old, ok) *)
let cas16 cx mem fail p expected desired =
  let rs =
    emit ~loc:cx.loc cx.f cx.cur (Cmpxchg { mem; fail; weak = false; p; expected; desired }) [| Int 128; i1 |]
  in
  (rs.(0), rs.(1))

(* the old value of an rmw [k] of [v] (of [ty]) at [p], as a loop of 128-bit
   compare-exchanges whose first guess is 0 (no separate read) *)
let rmw16 cx k (m : mem) ty p v =
  let loop, out = loop_blocks cx in
  set_term ~loc:cx.loc cx.f cx.cur (Jump (target ~args:[| iconst cx.f (Int 128) 0L |] loop));
  cx.cur <- loop;
  let guess = add_param cx.f loop (Int 128) in
  let old = out16 cx ty guess in
  let nw = into16 cx ty (rmw_value cx k ty old v) in
  let seen, ok = cas16 cx m (fail_of m.order) p guess nw in
  set_term ~loc:cx.loc cx.f cx.cur (Br (ok, target out, target ~args:[| seen |] loop, -1));
  cx.cur <- out;
  old

(* Rewrites [ir] at the end of block cx.cur (its later instructions are
   already detached); returns false when it needs nothing *)
let rewrite cx (ir : inst_rec) =
  let r k = ir.results.(k) in
  let set k v = replace_all_uses cx.f (r k) v in
  let ty_of v = ty_of cx.f v in
  match ir.op with
  | Load (m, p) when m.order <> Plain -> (
      match how_of ~align:m.align (ty_of (r 0)) with
      | Native -> false
      | Cas16 ->
          let z = iconst cx.f (Int 128) 0L in
          let old, _ = cas16 cx m m.order p z z in
          set 0 (out16 cx (ty_of (r 0)) old);
          true
      | Narrow c ->
          set 0 (e cx (Conv (Trunc, e cx (Load (m, p)) (Int c))) (ty_of (r 0)));
          true
      | Image c ->
          set 0 (of_image cx (ty_of (r 0)) c (e cx (Load (m, p)) (Int c)));
          true
      | Lib bytes ->
          let tmp = slot cx bytes in
          ignore (call cx "__atomic_load" [| c64 cx bytes; p; tmp; c32 cx (order_num m.order) |]);
          set 0 (e cx (Load (plain_mem 16, tmp)) (ty_of (r 0)));
          true)
  | Store (m, v, p) when m.order <> Plain -> (
      match how_of ~align:m.align (ty_of v) with
      | Native -> false
      | Cas16 ->
          ignore (rmw16 cx Xchg m (ty_of v) p v);
          true
      | Narrow c ->
          e0 cx (Store (m, e cx (Conv (Zext, v)) (Int c), p));
          true
      | Image c ->
          e0 cx (Store (m, to_image cx (ty_of v) c v, p));
          true
      | Lib bytes ->
          let tmp = copy_of cx bytes v in
          ignore (call cx "__atomic_store" [| c64 cx bytes; p; tmp; c32 cx (order_num m.order) |]);
          true)
  | Rmw (k, m, p, v) -> (
      let ty = ty_of v in
      match how_of ~align:m.align ty with
      | Native -> false
      | Cas16 ->
          set 0 (rmw16 cx k m ty p v);
          true
      | (Narrow c | Image c) when (match k with Xchg | Rand | Ror | Rxor -> true | _ -> false) ->
          (* zero above N in, zero above N out *)
          let into, out_of =
            match how_of ~align:m.align ty with
            | Image _ -> (to_image cx ty c, of_image cx ty c)
            | _ -> ((fun v -> e cx (Conv (Zext, v)) (Int c)), fun x -> e cx (Conv (Trunc, x)) ty)
          in
          let old = e cx (Rmw (k, m, p, into v)) (Int c) in
          set 0 (out_of old);
          true
      | Narrow c | Image c ->
          let image = match how_of ~align:m.align ty with Image _ -> true | _ -> false in
          let ct = Int c in
          let init = e cx (Load ({ m with order = Relaxed }, p)) ct in
          let loop, out = loop_blocks cx in
          let old_c = add_param cx.f loop ct in
          set_term ~loc:cx.loc cx.f cx.cur (Jump (target ~args:[| init |] loop));
          cx.cur <- loop;
          let old = if image then of_image cx ty c old_c else e cx (Conv (Trunc, old_c)) ty in
          let nv = rmw_value cx k ty old v in
          let nw = if image then to_image cx ty c nv else e cx (Conv (Zext, nv)) ct in
          let rs =
            emit ~loc:cx.loc cx.f cx.cur
              (Cmpxchg { mem = m; fail = fail_of m.order; weak = false; p; expected = old_c; desired = nw })
              [| ct; i1 |]
          in
          set_term ~loc:cx.loc cx.f cx.cur (Br (rs.(1), target out, target ~args:[| rs.(0) |] loop, -1));
          cx.cur <- out;
          set 0 old;
          true
      | Lib bytes when k = Xchg ->
          let tv = copy_of cx bytes v and ret = slot cx bytes in
          ignore (call cx "__atomic_exchange" [| c64 cx bytes; p; tv; ret; c32 cx (order_num m.order) |]);
          set 0 (e cx (Load (plain_mem 16, ret)) ty);
          true
      | Lib bytes ->
          (* exp holds the object as last seen: the first load, then what
             a failed compare-exchange wrote back *)
          let exp = slot cx bytes and des = slot cx bytes in
          ignore (call cx "__atomic_load" [| c64 cx bytes; p; exp; c32 cx 0 |]);
          let loop, out = loop_blocks cx in
          set_term ~loc:cx.loc cx.f cx.cur (Jump (target loop));
          cx.cur <- loop;
          let old = e cx (Load (plain_mem 16, exp)) ty in
          zero cx des bytes;
          e0 cx (Store (plain_mem 16, rmw_value cx k ty old v, des));
          let ok =
            (call cx "__atomic_compare_exchange"
               [| c64 cx bytes; p; exp; des; c32 cx (order_num m.order); c32 cx (order_num (fail_of m.order)) |]).(0)
          in
          let ok = e cx (Icmp (Ne, ok, iconst cx.f i8 0L)) i1 in
          set_term ~loc:cx.loc cx.f cx.cur (Br (ok, target out, target loop, -1));
          cx.cur <- out;
          set 0 old;
          true)
  | Cmpxchg c -> (
      let ty = ty_of c.expected in
      match how_of ~align:c.mem.align ty with
      | Native -> false
      | Cas16 ->
          let old, ok = cas16 cx c.mem c.fail c.p (into16 cx ty c.expected) (into16 cx ty c.desired) in
          set 0 (out16 cx ty old);
          set 1 ok;
          true
      | Narrow bits ->
          let ct = Int bits in
          let z v = e cx (Conv (Zext, v)) ct in
          let rs = emit ~loc:cx.loc cx.f cx.cur (Cmpxchg { c with expected = z c.expected; desired = z c.desired }) [| ct; i1 |] in
          set 0 (e cx (Conv (Trunc, rs.(0))) ty);
          set 1 rs.(1);
          true
      | Image bits ->
          (* compares the whole vector bit for bit *)
          let z = to_image cx ty bits in
          let rs =
            emit ~loc:cx.loc cx.f cx.cur (Cmpxchg { c with expected = z c.expected; desired = z c.desired }) [| Int bits; i1 |]
          in
          set 0 (of_image cx ty bits rs.(0));
          set 1 rs.(1);
          true
      | Lib bytes ->
          (* on failure libatomic writes the current value to exp; on
             success it holds the expected value, which was the old one *)
          let exp = copy_of cx bytes c.expected and des = copy_of cx bytes c.desired in
          let ok =
            (call cx "__atomic_compare_exchange"
               [| c64 cx bytes; c.p; exp; des; c32 cx (order_num c.mem.order); c32 cx (order_num c.fail) |]).(0)
          in
          set 0 (e cx (Load (plain_mem 16, exp)) ty);
          set 1 (e cx (Icmp (Ne, ok, iconst cx.f i8 0L)) i1);
          true)
  | _ -> false

let needs_rewrite f (ir : inst_rec) =
  let t (m : mem) v = how_of ~align:m.align (ty_of f v) <> Native in
  match ir.op with
  | Load (m, _) -> m.order <> Plain && t m ir.results.(0)
  | Store (m, v, _) -> m.order <> Plain && t m v
  | Rmw (_, m, _, v) -> t m v
  (* the 128-bit cmpxchg Cas16 leaves is itself what the backends take *)
  | Cmpxchg c -> (
      match ty_of f c.expected with Int 128 when c.mem.align >= 16 -> false | _ -> t c.mem c.expected)
  | _ -> false

let run_func m (f : func) =
  if f.defined then begin
    (* new blocks are appended, so this also visits the code moved into
       them (where nothing needs rewriting any more) *)
    let b = ref 0 in
    while !b < num_blocks f do
      let br = block f !b in
      let insts = Vec.to_array br.insts in
      (match Array.find_index (fun i -> needs_rewrite f (inst f i)) insts with
      | None -> incr b
      | Some j ->
          let term = br.term and term_loc = br.term_loc in
          br.insts <- Vec.of_list 0 (Array.to_list (Array.sub insts 0 j));
          let ir = inst f insts.(j) in
          let cx = { m; f; cur = !b; loc = ir.loc } in
          ignore (rewrite cx ir);
          (* the rest of the block after what was emitted. Block !b is
             scanned again (it may hold another such access); a block the
             rest moved to is new, at the end, so visited later *)
          let rest = Array.sub insts (j + 1) (Array.length insts - j - 1) in
          Array.iter (fun i -> ignore (Vec.push (block f cx.cur).insts i)) rest;
          set_term ~loc:term_loc f cx.cur term)
    done;
    (* a split entry block moved its later allocas (Fir.hoist_const_allocas) *)
    hoist_const_allocas f
  end

let run (m : modul) =
  (* a snapshot: declarations added on the way are not visited *)
  List.iter (run_func m) (Vec.to_list m.funcs)
