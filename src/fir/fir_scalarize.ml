(* Fir_scalarize: vectors rewritten to their lanes, Fir to Fir.

   The machine-IR backend (backend/mir) computes on scalars only. This pass
   runs before its instruction selection and leaves no value, signature,
   block parameter or memory access of a vector type:

   - A vector value becomes n lane values of the element type. Lane-wise
     operations (arithmetic, compares, select, conversions, freeze, fma,
     funnel shifts, overflow ops) become n scalar operations.
   - Memory layout is kept: lane i of a vector is at byte i * size_of(elem),
     except bool vectors, which are bit packed (lane i is bit i of byte
     i / 8, ceil(n/8) bytes; the unused high bits of the last byte are
     stored as 0). A vector load or store is one access per lane (per byte
     for bool vectors). Atomic vector accesses are refused.
   - The calling convention is the old native backend's (scalarize.ml), so
     both backends' objects link together: a vector parameter is passed as
     its lanes, one parameter each; a vector result is returned through a
     hidden pointer parameter, appended after the fixed parameters (one per
     vector result, in order), that the callee stores the vector to in the
     memory layout above. The function returns no value for it. This is
     not the SysV vector ABI: C code compiled by clang passes vectors in
     xmm registers, so vector signatures don't cross into C.
   - [extract] and [insert] at a variable index compare the index with each
     lane (an index out of range gives an arbitrary lane: the result is
     poison, SPEC §8). [shuffle], [splat] and [reduce] pick and fold lanes;
     [fadd_ordered] folds left from lane 0.
   - [masked_load] and [masked_store] branch per lane (a masked-off lane
     must not be touched: its address may be unmapped), so they add blocks.
   - [compress] and [expand] go through a stack slot with a running count
     of the selected lanes.
   - A [bitcast] that changes the shape stores the source and loads the
     result through a stack slot.

   Block ids are kept (blockaddr constants stay valid); added blocks come
   after the original ones. Refused: vectors through [...] (variadic
   arguments and va_arg), in syscalls, atomic_rmw and cmpxchg on vectors.
   A [dbg_value] of a vector is dropped. *)

open Fir

exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt
let is_vec = function Vec _ -> true | _ -> false

(* ---------- signatures ---------- *)

(* the ABI slots one source parameter becomes *)
let param_abis (a : abi) : abi list =
  match a with
  | Scalar (Vec (n, t)) -> List.init n (fun _ -> Scalar t)
  | Ext (Vec (n, t), x) -> List.init n (fun _ -> Ext (t, x))
  | a -> [ a ]

let vec_result = function Scalar (Vec _) | Ext (Vec _, _) -> true | _ -> false

let sig_of (s : signature) =
  let hidden = List.filter_map (fun a -> if vec_result a then Some (Scalar ptr) else None) s.result_abis in
  {
    s with
    param_abis = List.concat_map param_abis s.param_abis @ hidden;
    result_abis = List.filter (fun a -> not (vec_result a)) s.result_abis;
  }

let abi_has_vec = function Scalar t | Ext (t, _) -> is_vec t | Agg _ -> false
let sig_changes (s : signature) = List.exists abi_has_vec s.param_abis || List.exists abi_has_vec s.result_abis

(* ---------- context ---------- *)

type cx = {
  f : func;  (** source *)
  g : func;  (** rebuilt *)
  map : (value, value array) Hashtbl.t;  (** source value -> its value, or its lanes, in g *)
  homes : (value, value) Hashtbl.t;
      (** source vector -> the slot that holds it (see "Vectors in slots") *)
  lanes_in : (value * block, value array) Hashtbl.t;  (** a homed vector's lanes loaded in a g block *)
  homes_in : (value * block, value) Hashtbl.t;  (** a lane vector stored to a slot in a g block *)
  mutable b : block;  (** insertion block in g *)
  mutable loc : int;
}

let e cx op ty = emit1 ~loc:cx.loc cx.g cx.b op ty
let e0 cx op = emit0 ~loc:cx.loc cx.g cx.b op
let c64 cx x = iconst cx.g i64 x

(* constant-size stack memory, in the entry block *)
let slot cx bytes =
  emit1 cx.g entry (Alloca (c64 cx (Int64.of_int ((max bytes 1 + 15) / 16 * 16)), 16)) ptr

let at cx p off = if off = 0 then p else e cx (Ptradd (0, p, c64 cx 0L, 1, off)) ptr

(* p + i * scale for a run-time i64 index *)
let at_var cx p i scale = e cx (Ptradd (0, p, i, scale, 0)) ptr

(* the alignment a lane access at byte [off] of an access aligned [align]
   can promise *)
let lane_align align off =
  let rec low a = if a >= align || off land a <> 0 then a else low (2 * a) in
  if off = 0 then align else min align (low 1)

(* ---------- vectors in slots ----------

   Most vectors are held as their lanes. A vector built by inserts at
   variable indices, as Fas lowers [compress] and [expand] (n steps of
   [acc = select m_i, (insert acc, x_i, k), acc]), would then cost n
   operations per step and n^2 in all. Such vectors live in a slot
   instead: an insert is a memcpy and one store, a select with a scalar
   condition picks a slot pointer, an extract is one load.

   A slot is written only by the instruction that defines its vector and is
   never changed while that vector is live: it is rewritten only when the
   defining instruction runs again, and by SSA every later use then means
   the new value. A select's result shares its operand's slot under the
   same rule. Block parameters always carry lanes (a pointer passed around
   a loop would alias the next iteration's value).

   The slot layout is unpacked: lane i at i * stride, i1 lanes as one byte
   each. The lanes of a homed vector are loaded, and a lane vector stored to
   a slot, at most once per block. *)

let stride t = if t = i1 then 1 else size_of t

let slot_load cx t p stride i =
  let a = at_var cx p i stride in
  if t = i1 then e cx (Conv (Trunc, e cx (Load (plain_mem 1, a)) i8)) i1 else e cx (Load (plain_mem (min 16 stride), a)) t

let slot_store cx t p stride i x =
  let a = at_var cx p i stride in
  let x = if t = i1 then e cx (Conv (Zext, x)) i8 else x in
  e0 cx (Store (plain_mem (min 16 stride), x, a))

(* ---------- values ---------- *)

let src_ty cx v = ty_of cx.f v

let zero_of cx t = zero cx.g t

let rec get cx (v : value) : value array =
  let v = resolve cx.f v in
  match Hashtbl.find_opt cx.map v with
  | Some l -> l
  | None when Hashtbl.mem cx.homes v -> (
      match Hashtbl.find_opt cx.lanes_in (v, cx.b) with
      | Some l -> l
      | None ->
          let p = Hashtbl.find cx.homes v in
          let n, t = match src_ty cx v with Vec (n, t) -> (n, t) | _ -> assert false in
          let l = Array.init n (fun i -> slot_load cx t p (stride t) (c64 cx (Int64.of_int i))) in
          Hashtbl.replace cx.lanes_in (v, cx.b) l;
          l)
  | None ->
      let l =
        match def_of cx.f v with
        | Const c -> (
            match (c, ty_of cx.f v) with
            | Cvec (_, lanes), Vec _ -> Array.map (get1 cx) lanes
            | (Czero _ | Cnull _), Vec (n, t) -> Array.make n (zero_of cx t)
            | Cpoison _, Vec (n, t) -> Array.make n (poison cx.g t)
            | _, (Vec _ as t) -> unsupported "constant of type %s" (ty_to_string t)
            | c, t -> [| const cx.g c t |])
        | _ -> unsupported "value %d used before its definition" v
      in
      Hashtbl.replace cx.map v l;
      l

and get1 cx v =
  match get cx v with [| x |] when not (is_vec (src_ty cx v)) -> x | _ -> unsupported "a vector where a scalar is needed"

let set cx v l = Hashtbl.replace cx.map v l
let set_home cx v p = Hashtbl.replace cx.homes v p
let homed cx v = Hashtbl.mem cx.homes (resolve cx.f v)

(* a slot holding vector [v] *)
let home cx v =
  let v = resolve cx.f v in
  match Hashtbl.find_opt cx.homes v with
  | Some p -> p
  | None -> (
      match Hashtbl.find_opt cx.homes_in (v, cx.b) with
      | Some p -> p
      | None ->
          let t = elem (src_ty cx v) in
          let l = get cx v in
          let p = slot cx (Array.length l * stride t) in
          Array.iteri (fun i x -> slot_store cx t p (stride t) (c64 cx (Int64.of_int i)) x) l;
          Hashtbl.replace cx.homes_in (v, cx.b) p;
          p)

(* a lane index as an i64 inside [0, n): an index out of range gives
   poison (SPEC §8), but must not reach outside the slot *)
let clamp_index cx idx n =
  let x = get1 cx idx in
  let x =
    match src_ty cx idx with
    | Int 64 -> x
    | Int w when w < 64 -> e cx (Conv (Zext, x)) i64
    | _ -> e cx (Conv (Trunc, x)) i64
  in
  if n land (n - 1) = 0 then e cx (Ibin (And, 0, x, c64 cx (Int64.of_int (n - 1)))) i64
  else
    let ok = e cx (Icmp (Ult, x, c64 cx (Int64.of_int n))) i1 in
    e cx (Select (ok, x, c64 cx 0L)) i64

(* ---------- memory ---------- *)

let check_mem (m : mem) what = if m.order <> Plain then unsupported "atomic %s of a vector" what

(* the lanes of a vector of type [vt] in memory at [p] *)
let load_vec cx (m : mem) vt p =
  check_mem m "load";
  match vt with
  | Vec (n, Int 1) ->
      let bytes =
        Array.init ((n + 7) / 8) (fun j -> e cx (Load ({ m with align = lane_align m.align j }, at cx p j)) i8)
      in
      Array.init n (fun i ->
          let byte = bytes.(i / 8) in
          let sh = if i mod 8 = 0 then byte else e cx (Ibin (Lshr, 0, byte, iconst cx.g i8 (Int64.of_int (i mod 8)))) i8 in
          e cx (Conv (Trunc, sh)) i1)
  | Vec (n, t) ->
      let sz = size_of t in
      Array.init n (fun i -> e cx (Load ({ m with align = lane_align m.align (i * sz) }, at cx p (i * sz))) t)
  | t -> unsupported "load_vec of %s" (ty_to_string t)

let store_vec cx (m : mem) vt (lanes : value array) p =
  check_mem m "store";
  match vt with
  | Vec (n, Int 1) ->
      for j = 0 to ((n + 7) / 8) - 1 do
        let byte = ref (iconst cx.g i8 0L) in
        for k = 0 to 7 do
          let i = (8 * j) + k in
          if i < n then begin
            let x = e cx (Conv (Zext, lanes.(i))) i8 in
            let x = if k = 0 then x else e cx (Ibin (Shl, 0, x, iconst cx.g i8 (Int64.of_int k))) i8 in
            byte := if i = 8 * j then x else e cx (Ibin (Or, 0, !byte, x)) i8
          end
        done;
        e0 cx (Store ({ m with align = lane_align m.align j }, !byte, at cx p j))
      done
  | Vec (n, t) ->
      let sz = size_of t in
      for i = 0 to n - 1 do
        e0 cx (Store ({ m with align = lane_align m.align (i * sz) }, lanes.(i), at cx p (i * sz)))
      done
  | t -> unsupported "store_vec of %s" (ty_to_string t)

(* a value of any type, written to / read from memory *)
let store_any cx (m : mem) t x p = if is_vec t then store_vec cx m t (get cx x) p else e0 cx (Store (m, get1 cx x, p))
let load_any cx (m : mem) t p = if is_vec t then load_vec cx m t p else [| e cx (Load (m, p)) t |]

(* ---------- operations ---------- *)

(* a constant lane index, read unsigned; a huge one comes back negative
   or past the lanes, both out of range *)
let const_index cx idx =
  match const_of cx.f idx with
  | Some (Cint (Int w, l)) ->
      let x = l.(0) in
      let x = if w < 64 then Int64.logand x (Int64.pred (Int64.shift_left 1L w)) else x in
      if Array.exists (fun y -> y <> 0L) (Array.sub l 1 (Array.length l - 1)) || Int64.compare x 0x4000_0000L > 0
      then Some (-1)
      else Some (Int64.to_int x)
  | Some (Czero (Int _)) -> Some 0
  | _ -> None

let reduce cx k t (l : value array) =
  let op a b =
    match k with
    | Red_add -> e cx (Ibin (Add, 0, a, b)) t
    | Red_mul -> e cx (Ibin (Mul, 0, a, b)) t
    | Red_and -> e cx (Ibin (And, 0, a, b)) t
    | Red_or -> e cx (Ibin (Or, 0, a, b)) t
    | Red_xor -> e cx (Ibin (Xor, 0, a, b)) t
    | Red_smin -> e cx (Ibin (Smin, 0, a, b)) t
    | Red_smax -> e cx (Ibin (Smax, 0, a, b)) t
    | Red_umin -> e cx (Ibin (Umin, 0, a, b)) t
    | Red_umax -> e cx (Ibin (Umax, 0, a, b)) t
    | Red_fmin -> e cx (Fbin (Fmin, 0, a, b)) t
    | Red_fmax -> e cx (Fbin (Fmax, 0, a, b)) t
    | Red_fadd_ordered -> e cx (Fbin (Fadd, 0, a, b)) t
  in
  let acc = ref l.(0) in
  for i = 1 to Array.length l - 1 do
    acc := op !acc l.(i)
  done;
  !acc

(* a vector's lanes in a fresh slot, with stride [stride]; i1 lanes are
   kept as i8 (one byte each) *)
(* compress: lane i goes to position k (the number of selected lanes
   before it); each later lane overwrites the slot at k until a selected
   lane moves k past it. Positions at or after the final k are zero. *)
let compress cx t (v : value array) (m : value array) =
  let n = Array.length v in
  let stride = if t = i1 then 1 else size_of t in
  let p = slot cx (n * stride) in
  let k = ref (c64 cx 0L) in
  for i = 0 to n - 1 do
    slot_store cx t p stride !k v.(i);
    k := e cx (Ibin (Add, 0, !k, e cx (Conv (Zext, m.(i))) i64)) i64
  done;
  Array.init n (fun j ->
      let x = slot_load cx t p stride (c64 cx (Int64.of_int j)) in
      let live = e cx (Icmp (Ult, c64 cx (Int64.of_int j), !k)) i1 in
      e cx (Select (live, x, zero_of cx t)) t)

(* expand: selected lane i reads source lane k (the number of selected
   lanes before it); the others are zero *)
let expand cx t v (m : value array) =
  let p = home cx v and stride = stride t in
  let k = ref (c64 cx 0L) in
  Array.init (Array.length m) (fun i ->
      let x = slot_load cx t p stride !k in
      let r = e cx (Select (m.(i), x, zero_of cx t)) t in
      k := e cx (Ibin (Add, 0, !k, e cx (Conv (Zext, m.(i))) i64)) i64;
      r)

(* masked_load: one branch per lane, joined by a block parameter *)
let masked_load cx align t p (mask : value array) (pass : value array) =
  let sz = size_of t in
  Array.mapi
    (fun i m ->
      let ld = add_block cx.g and join = add_block cx.g in
      let x = add_param cx.g join t in
      set_term ~loc:cx.loc cx.g cx.b (Br (m, target ld, target ~args:[| pass.(i) |] join, -1));
      cx.b <- ld;
      let y = e cx (Load (plain_mem (lane_align align (i * sz)), at cx p (i * sz))) t in
      set_term ~loc:cx.loc cx.g cx.b (Jump (target ~args:[| y |] join));
      cx.b <- join;
      x)
    mask

let masked_store cx align t (v : value array) p (mask : value array) =
  let sz = size_of t in
  Array.iteri
    (fun i m ->
      let st = add_block cx.g and cont = add_block cx.g in
      set_term ~loc:cx.loc cx.g cx.b (Br (m, target st, target cont, -1));
      cx.b <- st;
      e0 cx (Store (plain_mem (lane_align align (i * sz)), v.(i), at cx p (i * sz)));
      set_term ~loc:cx.loc cx.g cx.b (Jump (target cont));
      cx.b <- cont)
    mask

(* a bitcast that changes the shape: through memory *)
let bitcast_mem cx x dst =
  let st = src_ty cx x in
  let p = slot cx (max (size_of st) (size_of dst)) in
  store_any cx (plain_mem 16) st x p;
  load_any cx (plain_mem 16) dst p

(* ---------- instructions ---------- *)

let copy_inst cx (ir : inst_rec) =
  let op = map_op_uses (get1 cx) ir.op in
  let tys = Array.map (fun r -> ty_of cx.f r) ir.results in
  let rs = emit ~loc:cx.loc cx.g cx.b op tys in
  Array.iteri (fun k r -> set cx r [| rs.(k) |]) ir.results

let vec_value cx v = is_vec (src_ty cx v)

(* call arguments by the source signature: vector arguments become their
   lanes; [hidden] (the result slots) follow the fixed arguments *)
let call_args cx (s : signature) (args : value array) hidden =
  let nfixed = List.length s.param_abis in
  let fixed = Array.sub args 0 (min nfixed (Array.length args)) in
  let rest = Array.sub args (Array.length fixed) (Array.length args - Array.length fixed) in
  let fixed = Array.concat (List.map (get cx) (Array.to_list fixed)) in
  Array.iter (fun x -> if vec_value cx x then unsupported "a vector passed through ...") rest;
  let shift = Array.length fixed + List.length hidden - nfixed in
  (Array.concat [ fixed; Array.of_list hidden; Array.map (get1 cx) rest ], shift)

let convert_inst cx (i : inst) =
  let ir = inst cx.f i in
  cx.loc <- ir.loc;
  let r k = ir.results.(k) in
  let rty k = ty_of cx.f (r k) in
  let any_vec =
    Array.exists (fun r -> is_vec (ty_of cx.f r)) ir.results
    ||
    let found = ref false in
    iter_op_uses (fun v -> if vec_value cx v then found := true) ir.op;
    !found
  in
  (* n lanes of element t for result 0 *)
  let shape () = match rty 0 with Vec (n, t) -> (n, t) | t -> unsupported "vector op with result %s" (ty_to_string t) in
  let lanes1 f =
    let n, t = shape () in
    set cx (r 0) (Array.init n (fun i -> f i t))
  in
  match ir.op with
  | Call c ->
      let sig_ = sig_of c.sig_ in
      (* one entry-block slot per vector result *)
      let slots =
        List.filter_map
          (fun res -> match ty_of cx.f res with Vec _ as t -> Some (res, t, slot cx (size_of t)) | _ -> None)
          (Array.to_list ir.results)
      in
      let args, shift = call_args cx c.sig_ c.args (List.map (fun (_, _, p) -> p) slots) in
      let va_aggs = List.map (fun (k, a) -> (k + shift, a)) c.va_aggs in
      let callee = match c.callee with Indirect p -> Indirect (get1 cx p) | d -> d in
      let scalars = List.filter (fun res -> not (vec_value cx res)) (Array.to_list ir.results) in
      let rs =
        emit ~loc:cx.loc cx.g cx.b (Call { c with sig_; callee; args; va_aggs })
          (Array.of_list (List.map (ty_of cx.f) scalars))
      in
      List.iteri (fun k res -> set cx res [| rs.(k) |]) scalars;
      List.iter (fun (res, t, p) -> set cx res (load_vec cx (plain_mem 16) t p)) slots
  | _ when not any_vec -> copy_inst cx ir
  | Nop -> ()
  | Dbg_value _ -> ()
  | Ibin (k, fl, a, b) ->
      let x = get cx a and y = get cx b in
      lanes1 (fun i t -> e cx (Ibin (k, fl, x.(i), y.(i))) t)
  | Iun (k, fl, a) ->
      let x = get cx a in
      lanes1 (fun i t -> e cx (Iun (k, fl, x.(i))) t)
  | Fbin (k, fl, a, b) ->
      let x = get cx a and y = get cx b in
      lanes1 (fun i t -> e cx (Fbin (k, fl, x.(i), y.(i))) t)
  | Fun (k, fl, a) ->
      let x = get cx a in
      lanes1 (fun i t -> e cx (Fun (k, fl, x.(i))) t)
  | Fma (fl, a, b, d) ->
      let x = get cx a and y = get cx b and z = get cx d in
      lanes1 (fun i t -> e cx (Fma (fl, x.(i), y.(i), z.(i))) t)
  | Fsh (left, a, b, d) ->
      let x = get cx a and y = get cx b and z = get cx d in
      lanes1 (fun i t -> e cx (Fsh (left, x.(i), y.(i), z.(i))) t)
  | Ovf (k, a, b) ->
      let n, t = shape () in
      let x = get cx a and y = get cx b in
      let both = Array.init n (fun i -> emit ~loc:cx.loc cx.g cx.b (Ovf (k, x.(i), y.(i))) [| t; i1 |]) in
      set cx (r 0) (Array.map (fun rs -> rs.(0)) both);
      set cx (r 1) (Array.map (fun rs -> rs.(1)) both)
  | Icmp (k, a, b) ->
      let x = get cx a and y = get cx b in
      lanes1 (fun i t -> e cx (Icmp (k, x.(i), y.(i))) t)
  | Fcmp (k, fl, a, b) ->
      let x = get cx a and y = get cx b in
      lanes1 (fun i t -> e cx (Fcmp (k, fl, x.(i), y.(i))) t)
  | Select (c, a, b) when (not (vec_value cx c)) && (homed cx a || homed cx b) ->
      (* a scalar condition between vectors in slots: pick the slot *)
      let pa = home cx a and pb = home cx b in
      set_home cx (r 0) (e cx (Select (get1 cx c, pa, pb)) ptr)
  | Select (c, a, b) ->
      let x = get cx a and y = get cx b in
      let cl = if vec_value cx c then get cx c else [||] in
      lanes1 (fun i t -> e cx (Select ((if cl = [||] then get1 cx c else cl.(i)), x.(i), y.(i))) t)
  | Freeze a ->
      let x = get cx a in
      lanes1 (fun i t -> e cx (Freeze x.(i)) t)
  | Conv (Bitcast, a) -> (
      let st = src_ty cx a and dt = rty 0 in
      match (st, dt) with
      | Vec (n, s), Vec (m, d) when n = m && size_of s = size_of d && s <> i1 ->
          let x = get cx a in
          set cx (r 0) (Array.map (fun l -> if s = d then l else e cx (Conv (Bitcast, l)) d) x)
      | _ -> set cx (r 0) (bitcast_mem cx a dt))
  | Conv (k, a) ->
      let x = get cx a in
      lanes1 (fun i t -> e cx (Conv (k, x.(i))) t)
  | Load (m, p) -> set cx (r 0) (load_vec cx m (rty 0) (get1 cx p))
  | Store (m, v, p) -> store_vec cx m (src_ty cx v) (get cx v) (get1 cx p)
  | Extract (v, idx) -> (
      let n, t = match src_ty cx v with Vec (n, t) -> (n, t) | _ -> assert false in
      match const_index cx idx with
      | Some k when k < 0 || k >= n -> set cx (r 0) [| poison cx.g t |]
      | Some k when homed cx v && not (Hashtbl.mem cx.lanes_in (resolve cx.f v, cx.b)) ->
          set cx (r 0) [| slot_load cx t (home cx v) (stride t) (c64 cx (Int64.of_int k)) |]
      | Some k -> set cx (r 0) [| (get cx v).(k) |]
      | None -> set cx (r 0) [| slot_load cx t (home cx v) (stride t) (clamp_index cx idx n) |])
  | Insert (v, x, idx) when const_index cx idx <> None && not (homed cx v) ->
      let l = get cx v and x = get1 cx x in
      let k = Option.get (const_index cx idx) in
      let n, t = shape () in
      if k < 0 || k >= n then set cx (r 0) (Array.make n (poison cx.g t))
      else set cx (r 0) (Array.init n (fun i -> if i = k then x else l.(i)))
  | Insert (v, x, idx) ->
      (* into a copy of the vector's slot *)
      let n, t = shape () in
      let src = home cx v in
      let q = slot cx (n * stride t) in
      e0 cx (Memcpy (false, q, src, c64 cx (Int64.of_int (n * stride t))));
      let i =
        match const_index cx idx with
        | Some k when k >= 0 && k < n -> c64 cx (Int64.of_int k)
        | _ -> clamp_index cx idx n
      in
      slot_store cx t q (stride t) i (get1 cx x);
      set_home cx (r 0) q
  | Shuffle (a, b, mask) ->
      let x = get cx a and y = get cx b in
      let n = Array.length x in
      let _, t = shape () in
      set cx (r 0) (Array.map (fun k -> if k < 0 then poison cx.g t else if k < n then x.(k) else y.(k - n)) mask)
  | Splat a ->
      let x = get1 cx a in
      let n, _ = shape () in
      set cx (r 0) (Array.make n x)
  | Reduce (k, a) -> set cx (r 0) [| reduce cx k (rty 0) (get cx a) |]
  | Masked_load (align, p, m, pt) ->
      let _, t = shape () in
      if t = i1 then unsupported "masked_load of a bool vector";
      set cx (r 0) (masked_load cx align t (get1 cx p) (get cx m) (get cx pt))
  | Masked_store (align, v, p, m) ->
      let t = elem (src_ty cx v) in
      if t = i1 then unsupported "masked_store of a bool vector";
      masked_store cx align t (get cx v) (get1 cx p) (get cx m)
  | Compress (v, m) ->
      let _, t = shape () in
      set cx (r 0) (compress cx t (get cx v) (get cx m))
  | Expand (v, m) ->
      let _, t = shape () in
      set cx (r 0) (expand cx t v (get cx m))
  | _ ->
      let text = Fir_print.op_text cx.f (Fir_print.assign_names cx.f) ir in
      unsupported "%s on a vector" (List.hd (String.split_on_char ' ' text))

(* ---------- terminators ---------- *)

let target_args cx (t : target) = { t with args = Array.concat (List.map (get cx) (Array.to_list t.args)) }

let rebuild_term cx (b : block) hidden =
  let br = block cx.f b in
  cx.loc <- br.term_loc;
  let set_term t = Fir.set_term ~loc:br.term_loc cx.g cx.b t in
  match br.term with
  | Ret vs ->
      (* vector results go to the hidden pointers, in order *)
      let hidden = ref hidden in
      let scalars =
        List.filter_map
          (fun v ->
            match src_ty cx v with
            | Vec _ as t ->
                let p = List.hd !hidden in
                hidden := List.tl !hidden;
                store_vec cx (plain_mem 1) t (get cx v) p;
                None
            | _ -> Some (get1 cx v))
          (Array.to_list vs)
      in
      set_term (Ret (Array.of_list scalars))
  | Jump t -> set_term (Jump (target_args cx t))
  | Br (c, t, f, w) -> set_term (Br (get1 cx c, target_args cx t, target_args cx f, w))
  | Switch (v, d, cases) ->
      set_term (Switch (get1 cx v, target_args cx d, Array.map (fun (key, t) -> (key, target_args cx t)) cases))
  | Indirect_br (v, bs) -> set_term (Indirect_br (get1 cx v, bs))
  | Unreachable -> set_term Unreachable

(* ---------- functions ---------- *)

let func_needs (f : func) =
  sig_changes f.sig_
  || (f.defined
     &&
     let found = ref false in
     for v = 0 to num_values f - 1 do
       if is_vec (ty_of f v) then found := true
     done;
     Vec.iter (fun (ir : inst_rec) -> match ir.op with Call c when sig_changes c.sig_ -> found := true | _ -> ()) f.insts;
     !found)

let rebuild_func (f : func) : func =
  let g = new_func ~linkage:f.linkage ~defined:f.defined f.name (sig_of f.sig_) in
  g.visibility <- f.visibility;
  g.fattrs <- f.fattrs;
  g.display <- f.display;
  g.decl_loc <- f.decl_loc;
  if not f.defined then g
  else begin
    resolve_all f;
    let cx =
      { f; g; map = Hashtbl.create 256; homes = Hashtbl.create 16; lanes_in = Hashtbl.create 16;
        homes_in = Hashtbl.create 16; b = entry; loc = 0 }
    in
    let nb = num_blocks f in
    for b = 0 to nb - 1 do
      ignore (add_block ~name:(block f b).bname g)
    done;
    (* parameters: lanes for vectors, in every block; then the entry
       block's hidden result pointers *)
    for b = 0 to nb - 1 do
      Array.iter
        (fun p ->
          match ty_of f p with
          | Vec (n, t) -> set cx p (Array.init n (fun _ -> add_param g b t))
          | t -> set cx p [| add_param g b t |])
        (block f b).params
    done;
    let hidden =
      List.filter_map (fun a -> if vec_result a then Some (add_param g entry ptr) else None) f.sig_.result_abis
    in
    let rpo = Fir_dom.reverse_postorder (Fir_dom.cfg f) in
    Array.iter
      (fun b ->
        cx.b <- b;
        Vec.iter (fun i -> convert_inst cx i) (block f b).insts;
        rebuild_term cx b hidden)
      rpo;
    g
  end

(* Rewrites [m] in place. Afterwards no value, signature or memory access
   has a vector type. *)
let run (m : modul) =
  for i = 0 to Vec.length m.funcs - 1 do
    let f = Vec.get m.funcs i in
    if func_needs f then Vec.set m.funcs i (rebuild_func f)
  done
