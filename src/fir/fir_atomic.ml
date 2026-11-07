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
   - Wider than 64 bits, and f80: libatomic's generic entry points
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

   The block holding a rewritten loop is split: the instructions after the
   access and the terminator move to a new block, the old results become
   aliases of the new ones (Fir.replace_all_uses). *)

open Fir

let legal = function 8 | 16 | 32 | 64 -> true | _ -> false

(* how an atomic access of [ty] is done: on an integer container of
   [bits], or by libatomic on an object of [bytes] *)
type how = Native | Narrow of int | Lib of int

let how_of = function
  | Int n when n <= 64 && not (legal n) -> Narrow (8 * size_of (Int n))
  | Int n when n > 64 -> Lib (size_of (Int n))
  | F80 -> Lib 16
  | _ -> Native

(* C11's memory_order numbering, what libatomic takes *)
let order_num = function Plain | Relaxed -> 0 | Acquire -> 2 | Release -> 3 | Acq_rel -> 4 | Seq_cst -> 5

(* the failure ordering of a compare-exchange that implements an rmw *)
let fail_of = function Release -> Relaxed | Acq_rel -> Acquire | o -> o

let i8 = Int 8
let i32 = Int 32

let libatomic =
  let s params result = { param_abis = List.map (fun t -> Scalar t) params; result_abis = result; variadic = false } in
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
  let tys = Array.of_list (List.map (function Scalar t | Ext (t, _) -> t | Agg _ -> ptr) sig_.result_abis) in
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
  let pick c = e cx (Select (e cx (Icmp (c, old, v)) i1, old, v)) ty in
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

(* Rewrites [ir] at the end of block cx.cur (its later instructions are
   already detached); returns false when it needs nothing *)
let rewrite cx (ir : inst_rec) =
  let r k = ir.results.(k) in
  let set k v = replace_all_uses cx.f (r k) v in
  let ty_of v = ty_of cx.f v in
  match ir.op with
  | Load (m, p) when m.order <> Plain -> (
      match how_of (ty_of (r 0)) with
      | Native -> false
      | Narrow c ->
          set 0 (e cx (Conv (Trunc, e cx (Load (m, p)) (Int c))) (ty_of (r 0)));
          true
      | Lib bytes ->
          let tmp = slot cx bytes in
          ignore (call cx "__atomic_load" [| c64 cx bytes; p; tmp; c32 cx (order_num m.order) |]);
          set 0 (e cx (Load (plain_mem 16, tmp)) (ty_of (r 0)));
          true)
  | Store (m, v, p) when m.order <> Plain -> (
      match how_of (ty_of v) with
      | Native -> false
      | Narrow c ->
          e0 cx (Store (m, e cx (Conv (Zext, v)) (Int c), p));
          true
      | Lib bytes ->
          let tmp = copy_of cx bytes v in
          ignore (call cx "__atomic_store" [| c64 cx bytes; p; tmp; c32 cx (order_num m.order) |]);
          true)
  | Rmw (k, m, p, v) -> (
      let ty = ty_of v in
      match how_of ty with
      | Native -> false
      | Narrow c when (match k with Xchg | Rand | Ror | Rxor -> true | _ -> false) ->
          (* zero above N in, zero above N out *)
          let old = e cx (Rmw (k, m, p, e cx (Conv (Zext, v)) (Int c))) (Int c) in
          set 0 (e cx (Conv (Trunc, old)) ty);
          true
      | Narrow c ->
          let ct = Int c in
          let init = e cx (Load ({ m with order = Relaxed }, p)) ct in
          let loop, out = loop_blocks cx in
          let old_c = add_param cx.f loop ct in
          set_term ~loc:cx.loc cx.f cx.cur (Jump (target ~args:[| init |] loop));
          cx.cur <- loop;
          let old = e cx (Conv (Trunc, old_c)) ty in
          let nw = e cx (Conv (Zext, rmw_value cx k ty old v)) ct in
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
      match how_of ty with
      | Native -> false
      | Narrow bits ->
          let ct = Int bits in
          let z v = e cx (Conv (Zext, v)) ct in
          let rs = emit ~loc:cx.loc cx.f cx.cur (Cmpxchg { c with expected = z c.expected; desired = z c.desired }) [| ct; i1 |] in
          set 0 (e cx (Conv (Trunc, rs.(0))) ty);
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
  let t v = how_of (ty_of f v) <> Native in
  match ir.op with
  | Load (m, _) -> m.order <> Plain && t ir.results.(0)
  | Store (m, v, _) -> m.order <> Plain && t v
  | Rmw (_, _, _, v) -> t v
  | Cmpxchg c -> t c.expected
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
    done
  end

let run (m : modul) =
  (* a snapshot: declarations added on the way are not visited *)
  List.iter (run_func m) (Vec.to_list m.funcs)
