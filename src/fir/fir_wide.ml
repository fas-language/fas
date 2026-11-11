(* Fir_wide: integers no register holds, rewritten to i64 limbs.

   The native backend computes in i1, i8, i16, i32 and i64. Every other
   integer width (i128 and wider, and odd widths such as i7, i33 or i100:
   C23's _BitInt(N)) is rewritten here, Fir to Fir, before MIR selection:

   - iN becomes k = ceil(N/64) i64 values ("limbs", least significant
     first). Odd widths below 64 are one limb.
   - Invariant: the bits of the top limb above N are zero (the value is
     normalized). Operations that can set them (add, mul, shl, ...) mask
     the top limb; operations that need the sign (ashr, sext, signed
     compares and division) sign-extend the top limb from bit N-1 first.
   - In memory, iN takes [size_of (Int N)] bytes (SPEC §2): one container
     integer (i8/i16/i32/i64) for one limb, else k limbs. Stores write the
     normalized value, so the storage bits above N are zero; loads mask
     them anyway.
   - At a signature (parameters, results, variadic arguments) a one-limb
     iN is its container integer (i7 -> i8, i33 -> i64), and a wider one an
     aggregate of k i64 fields: align 16 when k = 2 (C's __int128: two
     INTEGER eightbytes, 16-aligned on the stack), else 8 (MEMORY class, as
     the x86-64 psABI passes _BitInt(N > 128)). The rule depends only on the
     signature, so definitions, direct and indirect calls agree.
   - Block parameters become one parameter per limb.
   - Division and float conversions of more than one limb call helpers
     written in Fir (Fir_wide_runtime), appended to the module when used.

   f16 (IEEE binary16) is rewritten here too, to its bits in an i16, since
   x86-64 before AVX-512 FP16 has no half-precision arithmetic:

   - Arithmetic, comparisons and rounding functions convert the operands to
     f32 (exact), compute there and round the result back. For + - * / and
     sqrt that is correctly rounded: f32 has 24 bits, at least the
     2 * 11 + 2 needed so the double rounding never differs; the others
     (rem, min/max, floor..., compares) are exact in f32 already. neg, abs
     and copysign are bit operations, so NaN payloads pass untouched.
   - fma computes in f64 with a round-to-odd sum (__fir_fmah); narrowing
     conversions from f32, f64 and f80 round once, in integer code
     (__fir_round_h); integers convert through f32 (every integer below
     2^24 is exact there, every larger one is infinity in f16 either way).
   - In memory f16 is its i16. At a signature (parameters, results,
     variadic arguments) it stays f16, which the backend passes as SysV
     does: in the low 16 bits of an SSE register (or an 8-byte stack slot).
     va_arg reads it as an f32 slot and keeps the low 16 bits.

   Block ids are kept (blockaddr constants stay valid); blocks added here
   (switch chains) come after the original ones. Vectors of such integers
   are refused. Atomics on odd and wide integers and on f80 never get here:
   run first rewrites them (Fir_atomic). *)

open Fir

exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

let legal_width = function 1 | 8 | 16 | 32 | 64 -> true | _ -> false

(* a scalar integer this pass rewrites *)
let is_odd = function Int n -> not (legal_width n) | _ -> false

let rec mentions_odd = function
  | Int n -> not (legal_width n)
  | F16 -> true
  | Vec (_, t) -> mentions_odd t
  | _ -> false

let nlimbs n = (n + 63) / 64

(* the integer a one-limb iN is stored and passed as *)
let container n = 8 * size_of (Int n)

let wide_agg n =
  let k = nlimbs n in
  Agg { size = 8 * k; align = (if k = 2 then 16 else 8); fields = List.init k (fun i -> (8 * i, i64)) }

let abi_of (a : abi) : abi =
  match a with
  | Scalar (Int n) | Ext (Int n, _) when not (legal_width n) ->
      if nlimbs n = 1 then
        match a with
        | Ext (_, e) when container n < 64 -> Ext (Int (container n), e)
        | _ -> Scalar (Int (container n))
      else wide_agg n
  | Scalar F16 -> a
  | Scalar t | Ext (t, _) when mentions_odd t -> unsupported "vector of %s in a signature" (ty_to_string t)
  | a -> a

let sig_of (s : signature) =
  { s with param_abis = List.map abi_of s.param_abis; result_abis = List.map abi_of s.result_abis }

let sig_changes (s : signature) = sig_of s <> s

(* ---------- context ---------- *)

type cx = {
  m : modul;
  f : func;  (** source *)
  g : func;  (** rebuilt *)
  map : (value, value array) Hashtbl.t;  (** source value -> its value or limbs in g *)
  mutable b : block;  (** insertion block in g *)
  mutable loc : int;
  helpers : (string, unit) Hashtbl.t;  (** runtime helpers called, module-wide *)
}

let e cx op ty = emit1 ~loc:cx.loc cx.g cx.b op ty
let e0 cx op = emit0 ~loc:cx.loc cx.g cx.b op
let c64 cx x = iconst cx.g i64 x
let zero64 cx = c64 cx 0L
let ib cx k a b = e cx (Ibin (k, 0, a, b)) i64
let icmp cx k a b = e cx (Icmp (k, a, b)) i1
let sel cx c a b ty = e cx (Select (c, a, b)) ty
let and1 cx a b = e cx (Ibin (And, 0, a, b)) i1
let or1 cx a b = e cx (Ibin (Or, 0, a, b)) i1
let not1 cx a = e cx (Ibin (Xor, 0, a, iconst cx.g i1 1L)) i1
let zext64 cx a = e cx (Conv (Zext, a)) i64
let conv cx k a ty = e cx (Conv (k, a)) ty

(* constant-size stack memory, in the entry block *)
let slot cx bytes align =
  emit1 cx.g entry (Alloca (iconst cx.g i64 (Int64.of_int bytes), align)) ptr

let call_helper cx name params results args =
  Hashtbl.replace cx.helpers name ();
  let sig_ = { param_abis = List.map (fun t -> Scalar t) params; result_abis = List.map (fun t -> Scalar t) results; variadic = false } in
  emit ~loc:cx.loc cx.g cx.b (Call { sig_; callee = Direct name; args; attrs = a_nounwind; va_aggs = [] }) (Array.of_list results)

(* ---------- limb arithmetic on width n ---------- *)

let top_bits n = n mod 64 (* 0: the top limb is full *)
let low_mask r = Int64.pred (Int64.shift_left 1L r)

(* clears the bits above n *)
let mask cx n (l : value array) =
  let r = top_bits n in
  if r = 0 then l
  else
    let l = Array.copy l in
    let k = Array.length l - 1 in
    l.(k) <- ib cx And l.(k) (c64 cx (low_mask r));
    l

(* the top limb sign-extended from bit n-1 (breaks the invariant: for
   signed operations, mask the result) *)
let sx_top cx n (l : value array) =
  let r = top_bits n in
  if r = 0 then l
  else
    let l = Array.copy l in
    let k = Array.length l - 1 in
    let s = c64 cx (Int64.of_int (64 - r)) in
    l.(k) <- ib cx Ashr (ib cx Shl l.(k) s) s;
    l

(* i1: bit n-1 of a normalized value *)
let sign cx n l =
  let s = sx_top cx n l in
  icmp cx Slt s.(Array.length s - 1) (zero64 cx)

(* all-ones or zero, from a sign-extended top limb *)
let fill_of cx (sx : value array) = ib cx Ashr sx.(Array.length sx - 1) (c64 cx 63L)

let zeros cx k = Array.make k (zero64 cx)

let const_limbs cx n (limbs : int64 array) =
  Array.map (c64 cx) (normalize_limbs n limbs)

let all_ones cx n = mask cx n (Array.make (nlimbs n) (c64 cx (-1L)))

(* MIN and MAX of the signed width n *)
let smin_limbs cx n =
  let k = nlimbs n in
  const_limbs cx n (Array.init k (fun i -> if i = (n - 1) / 64 then Int64.shift_left 1L ((n - 1) mod 64) else 0L))

let smax_limbs cx n =
  let k = nlimbs n in
  const_limbs cx n
    (Array.init k (fun i ->
         if i < (n - 1) / 64 then -1L
         else if i = (n - 1) / 64 then low_mask ((n - 1) mod 64)
         else 0L))

(* a + b and a - b over all limbs, unmasked; also the carry/borrow out *)
let add_c cx a b =
  let k = Array.length a in
  let out = Array.make k (zero64 cx) in
  let carry = ref None in
  for i = 0 to k - 1 do
    let t = ib cx Add a.(i) b.(i) in
    let c1 = icmp cx Ult t a.(i) in
    match !carry with
    | None ->
        out.(i) <- t;
        carry := Some c1
    | Some c ->
        let s = ib cx Add t (zext64 cx c) in
        let c2 = icmp cx Ult s t in
        out.(i) <- s;
        carry := Some (or1 cx c1 c2)
  done;
  (out, Option.get !carry)

let sub_c cx a b =
  let k = Array.length a in
  let out = Array.make k (zero64 cx) in
  let borrow = ref None in
  for i = 0 to k - 1 do
    let t = ib cx Sub a.(i) b.(i) in
    let w1 = icmp cx Ult a.(i) b.(i) in
    match !borrow with
    | None ->
        out.(i) <- t;
        borrow := Some w1
    | Some w ->
        let wv = zext64 cx w in
        let d = ib cx Sub t wv in
        let w2 = icmp cx Ult t wv in
        out.(i) <- d;
        borrow := Some (or1 cx w1 w2)
  done;
  (out, Option.get !borrow)

let add cx a b = fst (add_c cx a b)
let sub cx a b = fst (sub_c cx a b)
let neg cx a = sub cx (zeros cx (Array.length a)) a
let bitwise cx k a b = Array.map2 (fun x y -> ib cx k x y) a b
let select_l cx c a b = Array.map2 (fun x y -> sel cx c x y i64) a b

(* comparisons of normalized values *)
let eq cx a b =
  let k = Array.length a in
  let acc = ref (ib cx Xor a.(0) b.(0)) in
  for i = 1 to k - 1 do
    acc := ib cx Or !acc (ib cx Xor a.(i) b.(i))
  done;
  icmp cx Eq !acc (zero64 cx)

(* a < b, unsigned; the top limb compared with [top] (Ult or Slt) *)
let less cx ~top a b =
  let k = Array.length a in
  let lt = ref (icmp cx (if k = 1 then top else Ult) a.(0) b.(0)) in
  for i = 1 to k - 1 do
    let c = if i = k - 1 then top else Ult in
    let li = icmp cx c a.(i) b.(i) in
    let ei = icmp cx Eq a.(i) b.(i) in
    lt := or1 cx li (and1 cx ei !lt)
  done;
  !lt

let ult cx a b = less cx ~top:Ult a b
let slt cx n a b = less cx ~top:Slt (sx_top cx n a) (sx_top cx n b)

(* resize to [k] limbs: drop, or extend with [fill] *)
let resize (l : value array) k fill = Array.init k (fun i -> if i < Array.length l then l.(i) else fill)

(* shifts by a constant, on [k] limbs of a value whose bits above the
   shift's width are already what they should be ([fill] for right shifts:
   zero or the sign) *)
let shl_const cx (l : value array) s =
  let k = Array.length l in
  let q = s / 64 and r = s mod 64 in
  Array.init k (fun i ->
      let src j = if j >= 0 && j < k then Some l.(j) else None in
      match (src (i - q), if r = 0 then None else src (i - q - 1)) with
      | None, None -> zero64 cx
      | Some a, None -> if r = 0 then a else ib cx Shl a (c64 cx (Int64.of_int r))
      | None, Some b -> ib cx Lshr b (c64 cx (Int64.of_int (64 - r)))
      | Some a, Some b -> e cx (Fsh (true, a, b, c64 cx (Int64.of_int r))) i64)

let shr_const cx (l : value array) s fill =
  let k = Array.length l in
  let q = s / 64 and r = s mod 64 in
  Array.init k (fun i ->
      let src j = if j < k then l.(j) else fill in
      if i + q >= k then fill
      else if r = 0 then src (i + q)
      else e cx (Fsh (false, src (i + q + 1), src (i + q), c64 cx (Int64.of_int r))) i64)

(* shifts by a variable amount [amt] (an i64, below the width; anything
   else gives some value). Up to four limbs: per output limb, a select over
   the limb offsets q = amt / 64 of funnel shifts by amt % 64. Wider: the
   limbs go to a stack buffer next to a run of [fill] limbs, and each output
   limb funnels two loads at a variable offset. *)
let shift_var cx ~left (l : value array) amt fill =
  let k = Array.length l in
  let q = ib cx Lshr amt (c64 cx 6L) in
  let r = ib cx And amt (c64 cx 63L) in
  if k <= 4 then
    Array.init k (fun i ->
        let acc = ref fill in
        if left then
          for j = i downto 0 do
            let hi = l.(i - j) and lo = if i - j - 1 >= 0 then l.(i - j - 1) else zero64 cx in
            let v = e cx (Fsh (true, hi, lo, r)) i64 in
            acc := sel cx (icmp cx Eq q (c64 cx (Int64.of_int j))) v !acc i64
          done
        else
          for j = k - 1 - i downto 0 do
            let lo = l.(i + j) and hi = if i + j + 1 < k then l.(i + j + 1) else fill in
            let v = e cx (Fsh (false, hi, lo, r)) i64 in
            acc := sel cx (icmp cx Eq q (c64 cx (Int64.of_int j))) v !acc i64
          done;
        !acc)
  else begin
    let buf = slot cx (16 * k) 8 in
    let at p i = e cx (Ptradd (0, p, zero64 cx, 1, 8 * i)) ptr in
    let st v p = e0 cx (Store (plain_mem 8, v, p)) in
    let ld p = e cx (Load (plain_mem 8, p)) i64 in
    let qc = e cx (Ibin (Umin, 0, q, c64 cx (Int64.of_int (k - 1)))) i64 in
    if left then begin
      (* buf = [0 x k][l]; P = buf + 8(k - q); out_i = fshl(P[i], P[i-1]) *)
      for i = 0 to k - 1 do
        st (zero64 cx) (at buf i);
        st l.(i) (at buf (k + i))
      done;
      let p = e cx (Ptradd (0, buf, ib cx Sub (c64 cx (Int64.of_int k)) qc, 8, 0)) ptr in
      Array.init k (fun i -> e cx (Fsh (true, ld (at p i), ld (at p (i - 1)), r)) i64)
    end
    else begin
      (* buf = [l][fill x k]; P = buf + 8q; out_i = fshr(P[i+1], P[i]) *)
      for i = 0 to k - 1 do
        st l.(i) (at buf i);
        st fill (at buf (k + i))
      done;
      let p = e cx (Ptradd (0, buf, qc, 8, 0)) ptr in
      Array.init k (fun i -> e cx (Fsh (false, ld (at p (i + 1)), ld (at p i), r)) i64)
    end
  end

(* a * b mod 2^(64k), k = the limb count of both: schoolbook, a row per
   limb of a; inline, so about k^2 instructions *)
let mul_inline cx a b =
  let k = Array.length a in
  let acc = ref (zeros cx k) in
  for i = 0 to k - 1 do
    (* row = a_i * b, shifted by i limbs, truncated to k limbs *)
    let row = Array.make k (zero64 cx) in
    let prev_hi = ref None and carry = ref None in
    for j = 0 to k - 1 - i do
      let lo = ib cx Mul a.(i) b.(j) in
      let v =
        match !prev_hi with
        | None -> lo
        | Some h ->
            let s = ib cx Add lo h in
            let c1 = icmp cx Ult s lo in
            let s, c =
              match !carry with
              | None -> (s, c1)
              | Some c ->
                  let s2 = ib cx Add s (zext64 cx c) in
                  (s2, or1 cx c1 (icmp cx Ult s2 s))
            in
            carry := Some c;
            s
      in
      row.(i + j) <- v;
      if i + j + 1 < k then prev_hi := Some (ib cx Umulh a.(i) b.(j))
    done;
    acc := if i = 0 then row else add cx !acc row
  done;
  !acc

(* products of more than this many limbs call __fir_mul (a loop) instead
   of growing as k^2 inline: _BitInt(65535) is 1024 limbs *)
let mul_inline_limbs = 8

let mul cx a b =
  let k = Array.length a in
  if k <= mul_inline_limbs then mul_inline cx a b
  else begin
    let limb_ptr cx p i = if i = 0 then p else e cx (Ptradd (0, p, zero64 cx, 1, 8 * i)) ptr in
    let store l =
      let p = slot cx (8 * k) 8 in
      Array.iteri (fun i x -> e0 cx (Store (plain_mem 8, x, limb_ptr cx p i))) l;
      p
    in
    let pa = store a and pb = store b and pr = slot cx (8 * k) 8 in
    ignore (call_helper cx "__fir_mul" [ ptr; ptr; ptr; i64 ] [] [| pr; pa; pb; c64 cx (Int64.of_int k) |]);
    Array.init k (fun i -> e cx (Load (plain_mem 8, limb_ptr cx pr i)) i64)
  end

(* the exact product of two n-bit values, signed or not, as 2k limbs *)
let mul_full cx ~signed n a b =
  let k = nlimbs n in
  let ext l = if signed then let s = sx_top cx n l in resize s (2 * k) (fill_of cx s) else resize l (2 * k) (zero64 cx) in
  mul cx (ext a) (ext b)

(* ---------- conversions between widths ---------- *)

(* limbs of a source value of any integer type, extended to [k] limbs as
   the n-bit value it is (n = its width): zero or sign fill *)
let extend cx ~signed n (l : value array) k =
  if signed then
    let s = sx_top cx n l in
    resize s k (fill_of cx s)
  else resize l k (zero64 cx)

(* ---------- memory ---------- *)

let limb_ptr cx p i = if i = 0 then p else e cx (Ptradd (0, p, zero64 cx, 1, 8 * i)) ptr

let load_limbs cx n (m : mem) p =
  let k = nlimbs n in
  if k = 1 then begin
    let c = container n in
    let x = e cx (Load (m, p)) (Int c) in
    let x = if c < 64 then zext64 cx x else x in
    mask cx n [| x |]
  end
  else begin
    if m.order <> Plain then unsupported "atomic access to i%d" n;
    let m = { m with align = min m.align 8 } in
    mask cx n (Array.init k (fun i -> e cx (Load (m, limb_ptr cx p i)) i64))
  end

let store_limbs cx n (m : mem) (l : value array) p =
  let k = nlimbs n in
  if k = 1 then begin
    let c = container n in
    let x = if c < 64 then conv cx Trunc l.(0) (Int c) else l.(0) in
    e0 cx (Store (m, x, p))
  end
  else begin
    if m.order <> Plain then unsupported "atomic access to i%d" n;
    let m = { m with align = min m.align 8 } in
    Array.iteri (fun i x -> e0 cx (Store (m, x, limb_ptr cx p i))) l
  end

(* a wide value as an agg (signature) operand: a slot holding the limbs *)
let to_agg cx n l =
  let k = nlimbs n in
  let p = slot cx (8 * k) (if k = 2 then 16 else 8) in
  store_limbs cx n (plain_mem 8) l p;
  p

(* ---------- values ---------- *)

let src_ty cx v = ty_of cx.f v

let rec get cx (v : value) : value array =
  let v = resolve cx.f v in
  match Hashtbl.find_opt cx.map v with
  | Some l -> l
  | None ->
      let l =
        match def_of cx.f v with
        | Const c -> (
            let t = ty_of cx.f v in
            match (c, t) with
            | Cint (_, limbs), Int n when not (legal_width n) -> const_limbs cx n limbs
            | (Czero _ | Cpoison _), Int n when not (legal_width n) -> zeros cx (nlimbs n)
            | Cfloat (_, b), F16 -> [| iconst cx.g (Int 16) b.(0) |]
            | (Czero _ | Cpoison _), F16 -> [| iconst cx.g (Int 16) 0L |]
            | _, t when mentions_odd t -> unsupported "constant of type %s" (ty_to_string t)
            | Cvec (vt, lanes), _ -> [| const cx.g (Cvec (vt, Array.map (get1 cx) lanes)) t |]
            | c, _ -> [| const cx.g c t |])
        | _ -> unsupported "value %d used before its definition" v
      in
      Hashtbl.replace cx.map v l;
      l

and get1 cx v =
  match get cx v with [| x |] -> x | _ -> unsupported "a wide integer where one value is needed"

(* the low 64 bits of an integer operand of any width, as an i64 *)
let low64 cx v =
  match src_ty cx v with
  | Int n when not (legal_width n) -> (get cx v).(0)
  | Int 64 -> get1 cx v
  | Int _ -> zext64 cx (get1 cx v)
  | t -> unsupported "integer operand of type %s" (ty_to_string t)

(* limbs of any scalar integer operand as an n-bit value: legal widths are
   zero-extended into one i64 (callers that need the sign use [extend]) *)
let limbs_of cx v =
  match src_ty cx v with
  | Int n when not (legal_width n) -> (n, get cx v)
  | Int n -> (n, [| (if n = 64 then get1 cx v else zext64 cx (get1 cx v)) |])
  | t -> unsupported "integer operand of type %s" (ty_to_string t)

let set cx v l = Hashtbl.replace cx.map v l

(* ---------- operations ---------- *)

let div_helper cx n a b =
  let k = nlimbs n in
  let pa = to_agg cx n a and pb = to_agg cx n b in
  let pq = slot cx (8 * k) 8 and pr = slot cx (8 * k) 8 in
  ignore (call_helper cx "__fir_udivmod" [ ptr; ptr; ptr; ptr; i64 ] [] [| pq; pr; pa; pb; c64 cx (Int64.of_int k) |]);
  (load_limbs cx n (plain_mem 8) pq, load_limbs cx n (plain_mem 8) pr)

(* unsigned and signed division of normalized n-bit values *)
let udivrem cx n a b ~rem =
  if nlimbs n = 1 then [| ib cx (if rem then Urem else Udiv) a.(0) b.(0) |]
  else
    let q, r = div_helper cx n a b in
    if rem then r else q

let sdivrem cx n a b ~rem =
  if nlimbs n = 1 then
    let sa = sx_top cx n a and sb = sx_top cx n b in
    mask cx n [| ib cx (if rem then Srem else Sdiv) sa.(0) sb.(0) |]
  else
    let na = sign cx n a and nb = sign cx n b in
    let abs_ x s = select_l cx s (mask cx n (neg cx x)) x in
    let q, r = div_helper cx n (abs_ a na) (abs_ b nb) in
    if rem then select_l cx na (mask cx n (neg cx r)) r
    else select_l cx (e cx (Ibin (Xor, 0, na, nb)) i1) (mask cx n (neg cx q)) q

(* the shift amount as an i64 (values at or above the width give poison in
   Fir, so the high limbs can be ignored) *)
let shift cx n k a amt =
  let l = Array.length a in
  ignore l;
  match k with
  | Shl ->
      if nlimbs n = 1 then mask cx n [| ib cx Shl a.(0) amt |]
      else mask cx n (shift_var cx ~left:true a amt (zero64 cx))
  | Lshr ->
      if nlimbs n = 1 then [| ib cx Lshr a.(0) amt |] else shift_var cx ~left:false a amt (zero64 cx)
  | Ashr ->
      let s = sx_top cx n a in
      if nlimbs n = 1 then mask cx n [| ib cx Ashr s.(0) amt |]
      else mask cx n (shift_var cx ~left:false s amt (fill_of cx s))
  | _ -> assert false

(* overflow-checked arithmetic: the wrapped value and the i1 flag *)
let ovf cx n k a b =
  match k with
  | Uadd_ovf ->
      let r = mask cx n (add cx a b) in
      (r, ult cx r a)
  | Usub_ovf -> (mask cx n (sub cx a b), ult cx a b)
  | Sadd_ovf | Ssub_ovf ->
      let r = mask cx n ((if k = Sadd_ovf then add else sub) cx a b) in
      let sa = sign cx n a and sb = sign cx n b and sr = sign cx n r in
      let same = icmp cx Eq (zext64 cx sa) (zext64 cx sb) in
      let operands = if k = Sadd_ovf then same else not1 cx same in
      let flipped = icmp cx Ne (zext64 cx sr) (zext64 cx sa) in
      (r, and1 cx operands flipped)
  | Umul_ovf ->
      let p = mul_full cx ~signed:false n a b in
      let r = mask cx n (Array.sub p 0 (nlimbs n)) in
      (r, not1 cx (eq cx p (resize r (Array.length p) (zero64 cx))))
  | Smul_ovf ->
      let p = mul_full cx ~signed:true n a b in
      let r = mask cx n (Array.sub p 0 (nlimbs n)) in
      (r, not1 cx (eq cx p (extend cx ~signed:true n r (Array.length p))))

(* high half of the 2n-bit product: bits n .. 2n-1 *)
let mulh cx n ~signed a b =
  let p = mul_full cx ~signed n a b in
  let k = nlimbs n in
  let h =
    if signed then shr_const cx p n (ib cx Ashr p.(Array.length p - 1) (c64 cx 63L))
    else shr_const cx p n (zero64 cx)
  in
  mask cx n (Array.sub h 0 k)

let ibin cx n k a b =
  let k1 = nlimbs n = 1 in
  match k with
  | Add -> mask cx n (add cx a b)
  | Sub -> mask cx n (sub cx a b)
  | Mul -> if k1 then mask cx n [| ib cx Mul a.(0) b.(0) |] else mask cx n (mul cx a b)
  | Udiv -> udivrem cx n a b ~rem:false
  | Urem -> udivrem cx n a b ~rem:true
  | Sdiv -> sdivrem cx n a b ~rem:false
  | Srem -> sdivrem cx n a b ~rem:true
  | Shl | Lshr | Ashr -> shift cx n k a b.(0)
  | And -> bitwise cx And a b
  | Or -> bitwise cx Or a b
  | Xor -> bitwise cx Xor a b
  | Umin -> select_l cx (ult cx a b) a b
  | Umax -> select_l cx (ult cx b a) a b
  | Smin -> select_l cx (slt cx n a b) a b
  | Smax -> select_l cx (slt cx n b a) a b
  | Umulh -> mulh cx n ~signed:false a b
  | Smulh -> mulh cx n ~signed:true a b
  | Uadd_sat ->
      let r, o = ovf cx n Uadd_ovf a b in
      select_l cx o (all_ones cx n) r
  | Usub_sat ->
      let r, o = ovf cx n Usub_ovf a b in
      select_l cx o (zeros cx (nlimbs n)) r
  | Sadd_sat | Ssub_sat ->
      let r, o = ovf cx n (if k = Sadd_sat then Sadd_ovf else Ssub_ovf) a b in
      let bound = select_l cx (sign cx n a) (smin_limbs cx n) (smax_limbs cx n) in
      select_l cx o bound r

(* ctz/clz/popcnt give an iN count, bswap/bitrev/abs an iN value *)
let iun cx n k a =
  let kk = nlimbs n in
  let result_count x = resize [| x |] kk (zero64 cx) in
  let unused = (64 * kk) - n in
  match k with
  | Popcnt ->
      let s = ref (e cx (Iun (Popcnt, 0, a.(0))) i64) in
      for i = 1 to kk - 1 do
        s := ib cx Add !s (e cx (Iun (Popcnt, 0, a.(i))) i64)
      done;
      result_count !s
  | Ctz ->
      (* the lowest nonzero limb decides; all zero gives n *)
      let acc = ref (c64 cx (Int64.of_int n)) in
      for i = kk - 1 downto 0 do
        let c = ib cx Add (e cx (Iun (Ctz, 0, a.(i))) i64) (c64 cx (Int64.of_int (64 * i))) in
        acc := sel cx (icmp cx Ne a.(i) (zero64 cx)) c !acc i64
      done;
      result_count !acc
  | Clz ->
      (* the highest nonzero limb decides; the top limb's unused bits are
         counted by clz and taken off again *)
      let acc = ref (c64 cx (Int64.of_int n)) in
      for i = 0 to kk - 1 do
        let c =
          ib cx Add (e cx (Iun (Clz, 0, a.(i))) i64) (c64 cx (Int64.of_int ((64 * (kk - 1 - i)) - unused)))
        in
        acc := sel cx (icmp cx Ne a.(i) (zero64 cx)) c !acc i64
      done;
      result_count !acc
  | Bswap | Bitrev ->
      (* reverse all 64k bits (or bytes), then shift the unused bits out *)
      if k = Bswap && n mod 16 <> 0 then unsupported "bswap of i%d" n;
      let r = Array.init kk (fun i -> e cx (Iun (k, 0, a.(kk - 1 - i))) i64) in
      if unused = 0 then r else shr_const cx r unused (zero64 cx)
  | Abs -> select_l cx (sign cx n a) (mask cx n (neg cx a)) a

(* fshl/fshr: the 2n-bit concatenation a:b shifted by amt mod n *)
let fsh cx n ~left a b c =
  let kk = nlimbs n in
  let s =
    if kk = 1 then ib cx Urem c.(0) (c64 cx (Int64.of_int n))
    else if n land (n - 1) = 0 then ib cx And c.(0) (c64 cx (Int64.of_int (n - 1)))
    else (udivrem cx n c (const_limbs cx n [| Int64.of_int n |]) ~rem:true).(0)
  in
  let k2 = nlimbs (2 * n) in
  let cat = bitwise cx Or (shl_const cx (resize a k2 (zero64 cx)) n) (resize b k2 (zero64 cx)) in
  if left then
    let t = shift_var cx ~left:true cat s (zero64 cx) in
    mask cx n (Array.sub (shr_const cx t n (zero64 cx)) 0 kk)
  else
    let t = shift_var cx ~left:false cat s (zero64 cx) in
    mask cx n (Array.sub t 0 kk)

(* ---------- f16 ---------- *)

let i16 = Int 16

(* f16 bits -> f32 (exact) *)
let h2f cx x = (call_helper cx "__fir_h2f" [ i16 ] [ F32 ] [| x |]).(0)

(* an f32/f64/f80 value -> f16 bits, one rounding *)
let f_to_h cx t x =
  let name = match t with F32 -> "__fir_f32toh" | F64 -> "__fir_f64toh" | F80 -> "__fir_f80toh" | t -> unsupported "fptrunc from %s" (ty_to_string t) in
  (call_helper cx name [ t ] [ i16 ] [| x |]).(0)

let ib16 cx k a b = e cx (Ibin (k, 0, a, b)) i16
let c16 cx x = iconst cx.g i16 x

(* 2^e as an f64 constant (+inf beyond the range) *)
let pow2_f64 cx e =
  fconst_bits cx.g F64 [| (if e > 1023 then 0x7ff0000000000000L else Int64.shift_left (Int64.of_int (e + 1023)) 52) |]

(* 2^e as an f80 constant (+inf beyond the range) *)
let pow2_f80 cx e = fconst_bits cx.g F80 [| Int64.min_int; Int64.of_int (if e > 16383 then 0x7fff else e + 16383) |]

let float_to_int cx n kind ~src xv =
  (* f32 widens to f64 (exact); f80 stays itself: f64 would round it *)
  let ft = match src with F80 -> F80 | _ -> F64 in
  let x64 = match src with F64 | F80 -> xv | F32 -> conv cx Fpext xv F64 | t -> unsupported "conversion from %s" (ty_to_string t) in
  let pow2 e = if ft = F80 then pow2_f80 cx e else pow2_f64 cx e in
  let kk = nlimbs n in
  let signed = kind = Fptosi || kind = Fptosi_sat in
  let sat = kind = Fptosi_sat || kind = Fptoui_sat in
  if kk = 1 then begin
    (* through i64; the saturating forms clamp to the n-bit range *)
    let r = conv cx (if sat then (if signed then Fptosi_sat else Fptoui_sat) else (if signed then Fptosi else Fptoui)) x64 i64 in
    let r =
      (* n = 64 is the i64 conversion itself (and low_mask 64 would be 0) *)
      if not sat || n = 64 then r
      else if signed then
        let lo = c64 cx (Int64.neg (Int64.shift_left 1L (n - 1))) and hi = c64 cx (low_mask (n - 1)) in
        e cx (Ibin (Smin, 0, e cx (Ibin (Smax, 0, r, lo)) i64, hi)) i64
      else e cx (Ibin (Umin, 0, r, c64 cx (low_mask n))) i64
    in
    mask cx n [| r |]
  end
  else begin
    let zero_f = fconst_bits cx.g ft (if ft = F80 then [| 0L; 0L |] else [| 0L |]) in
    let negative = e cx (Fcmp (Folt, 0, x64, zero_f)) i1 in
    let ax = e cx (Fun (Fabs, 0, x64)) ft in
    let out = slot cx (8 * kk) 8 in
    let helper = if ft = F80 then "__fir_f80tou" else "__fir_f64tou" in
    ignore (call_helper cx helper [ ptr; ft; i64 ] [] [| out; ax; c64 cx (Int64.of_int kk) |]);
    let mag = load_limbs cx n (plain_mem 8) out in
    let v = if signed then select_l cx negative (mask cx n (neg cx mag)) mag else mag in
    if not sat then v
    else begin
      let nan = e cx (Fcmp (Funo, 0, x64, x64)) i1 in
      let r =
        if signed then
          let too_big = e cx (Fcmp (Foge, 0, x64, pow2 (n - 1))) i1 in
          let too_small = e cx (Fcmp (Folt, 0, x64, e cx (Fun (Fneg, 0, pow2 (n - 1))) ft)) i1 in
          select_l cx too_big (smax_limbs cx n) (select_l cx too_small (smin_limbs cx n) v)
        else
          let too_big = e cx (Fcmp (Foge, 0, x64, pow2 n)) i1 in
          let positive = e cx (Fcmp (Fogt, 0, x64, zero_f)) i1 in
          select_l cx too_big (all_ones cx n) (select_l cx positive v (zeros cx kk))
      in
      select_l cx nan (zeros cx kk) r
    end
  end

let int_to_float cx n ~signed a ty =
  let kk = nlimbs n in
  if ty <> F32 && ty <> F64 && ty <> F80 then unsupported "conversion to %s" (ty_to_string ty);
  if kk = 1 then
    let x = if signed then (sx_top cx n a).(0) else a.(0) in
    conv cx (if signed then Sitofp else Uitofp) x ty
  else begin
    let negative = if signed then Some (sign cx n a) else None in
    let mag = match negative with Some s -> select_l cx s (mask cx n (neg cx a)) a | None -> a in
    let p = to_agg cx n mag in
    let name = match ty with F64 -> "__fir_utof64" | F80 -> "__fir_utof80" | _ -> "__fir_utof32" in
    let f = (call_helper cx name [ ptr; i64 ] [ ty ] [| p; c64 cx (Int64.of_int kk) |]).(0) in
    match negative with Some s -> sel cx s (e cx (Fun (Fneg, 0, f)) ty) f ty | None -> f
  end

(* a bitcast with an odd integer on one side: through a stack slot *)
let bitcast_mem cx x dst_ty =
  let st = src_ty cx x in
  let size = max (size_of st) (size_of dst_ty) in
  let p = slot cx ((size + 15) / 16 * 16) 16 in
  (match st with
  | Int n when not (legal_width n) -> store_limbs cx n (plain_mem 16) (get cx x) p
  | _ -> e0 cx (Store (plain_mem 16, get1 cx x, p)));
  match dst_ty with
  | Int n when not (legal_width n) -> load_limbs cx n (plain_mem 16) p
  | t -> [| e cx (Load (plain_mem 16, p)) t |]

let convert cx kind x dst =
  let st = src_ty cx x in
  match (kind, st, dst) with
  | Bitcast, F16, Int 16 | Bitcast, Int 16, F16 -> [| get1 cx x |]
  | Fpext, F16, F32 -> [| h2f cx (get1 cx x) |]
  | Fpext, F16, _ -> [| conv cx Fpext (h2f cx (get1 cx x)) dst |]
  | Fptrunc, _, F16 -> [| f_to_h cx st (get1 cx x) |]
  | Bitcast, _, _ -> bitcast_mem cx x dst
  | (Trunc | Zext | Sext), Int sn, Int dn ->
      let _, l = limbs_of cx x in
      let signed = kind = Sext in
      let l = if legal_width sn && sn < 64 && signed then [| conv cx Sext (get1 cx x) i64 |] else l in
      let ext = extend cx ~signed:(signed && not (legal_width sn && sn <= 64)) sn l (nlimbs dn) in
      let ext = if signed && legal_width sn && sn <= 64 then resize l (nlimbs dn) (ib cx Ashr l.(0) (c64 cx 63L)) else ext in
      if legal_width dn then
        let x0 = ext.(0) in
        [| (if dn = 64 then x0 else conv cx Trunc x0 (Int dn)) |]
      else mask cx dn ext
  | Ptrtoint, _, Int dn -> mask cx dn (resize [| conv cx Ptrtoint (get1 cx x) i64 |] (nlimbs dn) (zero64 cx))
  | Inttoptr, Int _, _ -> [| conv cx Inttoptr (low64 cx x) dst |]
  | (Fptosi | Fptoui | Fptosi_sat | Fptoui_sat), _, Int dn ->
      (* f16 converts exactly to f32 first *)
      let src, xv = if st = F16 then (F32, h2f cx (get1 cx x)) else (st, get1 cx x) in
      let r = float_to_int cx dn kind ~src xv in
      (* a legal destination (from f16) is one value of its own type *)
      if legal_width dn && dn < 64 then [| conv cx Trunc r.(0) (Int dn) |] else r
  | (Sitofp | Uitofp), Int sn, F16 ->
      let _, a = limbs_of cx x in
      [| f_to_h cx F32 (int_to_float cx sn ~signed:(kind = Sitofp) a F32) |]
  | (Sitofp | Uitofp), Int sn, _ ->
      let _, a = limbs_of cx x in
      [| int_to_float cx sn ~signed:(kind = Sitofp) a dst |]
  | _ -> unsupported "conversion %s from %s to %s" (Fir_print.conv_name kind) (ty_to_string st) (ty_to_string dst)

(* ---------- instructions ---------- *)

let copy_inst cx (ir : inst_rec) =
  let op = map_op_uses (get1 cx) ir.op in
  let tys = Array.map (fun r -> ty_of cx.f r) ir.results in
  let rs = emit ~loc:cx.loc cx.g cx.b op tys in
  Array.iteri (fun k r -> set cx r [| rs.(k) |]) ir.results

let odd_value cx v = mentions_odd (src_ty cx v)

(* a value handed over through a signature slot with the (source) ABI
   [abi], or [None] for a variadic argument. One limb goes as its container,
   sign-extended from bit n-1 for [ext(iN, sx)] (C's signed _BitInt(N)
   arrives sign-extended; the slot's own extension then widens the
   container), or as an i64 through [...]; more limbs as an agg slot. *)
let to_abi cx (abi : abi option) (v : value) =
  match src_ty cx v with
  | Int n when not (legal_width n) ->
      let l = get cx v in
      if nlimbs n = 1 then
        let c = if abi = None then 64 else container n in
        let x = match abi with Some (Ext (_, Sx)) when c < 64 -> (sx_top cx n l).(0) | _ -> l.(0) in
        if c < 64 then conv cx Trunc x (Int c) else x
      else to_agg cx n l
  | F16 -> conv cx Bitcast (get1 cx v) F16
  | t when mentions_odd t -> unsupported "vector of %s in a signature" (ty_to_string t)
  | _ -> get1 cx v

(* call arguments by the (source) signature; variadic arguments of more
   than one limb are listed as agg(...) arguments *)
let call_args cx (s : signature) (args : value array) =
  let nfixed = List.length s.param_abis in
  let va_aggs = ref [] in
  let out =
    Array.mapi
      (fun i x ->
        (match src_ty cx x with
        | Int n when (not (legal_width n)) && nlimbs n > 1 && i >= nfixed -> va_aggs := (i, wide_agg n) :: !va_aggs
        | _ -> ());
        to_abi cx (if i < nfixed then Some (List.nth s.param_abis i) else None) x)
      args
  in
  (out, List.rev !va_aggs)

(* a value received through an ABI slot (parameter, call result) as limbs *)
let from_abi cx t (x : value) =
  match t with
  | Int n when not (legal_width n) ->
      if nlimbs n = 1 then
        let c = container n in
        mask cx n [| (if c < 64 then zext64 cx x else x) |]
      else load_limbs cx n (plain_mem 8) x
  | F16 -> [| conv cx Bitcast x i16 |]
  | _ -> [| x |]

let convert_inst cx (i : inst) =
  let ir = inst cx.f i in
  cx.loc <- ir.loc;
  let r k = ir.results.(k) in
  let rty k = ty_of cx.f (r k) in
  let any_odd =
    Array.exists (fun r -> mentions_odd (ty_of cx.f r)) ir.results
    ||
    let found = ref false in
    iter_op_uses (fun v -> if odd_value cx v then found := true) ir.op;
    !found
  in
  let scalar_odd t = match t with Vec _ when mentions_odd t -> unsupported "vector of %s" (ty_to_string t) | _ -> () in
  Array.iter (fun r -> scalar_odd (ty_of cx.f r)) ir.results;
  match ir.op with
  | Call c ->
      let sig_ = sig_of c.sig_ in
      let args, va_aggs = call_args cx c.sig_ c.args in
      let callee = match c.callee with Indirect p -> Indirect (get1 cx p) | d -> d in
      let tys = Array.of_list (List.map (fun a -> match a with Scalar t | Ext (t, _) -> t | Agg _ -> ptr) sig_.result_abis) in
      let rs = emit ~loc:cx.loc cx.g cx.b (Call { c with sig_; callee; args; va_aggs = c.va_aggs @ va_aggs }) tys in
      Array.iteri (fun k res -> set cx res (from_abi cx (ty_of cx.f res) rs.(k))) ir.results
  (* f80 fma: a correctly rounded software helper (no x87 instruction) *)
  | Fma (_, a, b, c) when ty_of cx.f ir.results.(0) = F80 ->
      set cx (r 0) (call_helper cx "__fir_fma80" [ F80; F80; F80 ] [ F80 ] [| get1 cx a; get1 cx b; get1 cx c |])
  | _ when not any_odd -> copy_inst cx ir
  | Nop | Dbg_value _ -> ()
  (* f16: see the header *)
  | Fbin (Copysign, _, a, b) ->
      let mag = ib16 cx And (get1 cx a) (c16 cx 0x7fffL) and sgn = ib16 cx And (get1 cx b) (c16 cx 0x8000L) in
      set cx (r 0) [| ib16 cx Or mag sgn |]
  | Fbin (k, fl, a, b) ->
      let v = e cx (Fbin (k, fl, h2f cx (get1 cx a), h2f cx (get1 cx b))) F32 in
      set cx (r 0) [| f_to_h cx F32 v |]
  | Fun (Fneg, _, a) -> set cx (r 0) [| ib16 cx Xor (get1 cx a) (c16 cx 0x8000L) |]
  | Fun (Fabs, _, a) -> set cx (r 0) [| ib16 cx And (get1 cx a) (c16 cx 0x7fffL) |]
  | Fun (k, fl, a) ->
      let v = e cx (Fun (k, fl, h2f cx (get1 cx a))) F32 in
      set cx (r 0) [| f_to_h cx F32 v |]
  | Fcmp (p, fl, a, b) -> set cx (r 0) [| e cx (Fcmp (p, fl, h2f cx (get1 cx a), h2f cx (get1 cx b))) i1 |]
  | Fma (_, a, b, c) ->
      set cx (r 0) (call_helper cx "__fir_fmah" [ i16; i16; i16 ] [ i16 ] [| get1 cx a; get1 cx b; get1 cx c |])
  | Select (c, a, b) when rty 0 = F16 -> set cx (r 0) [| sel cx (get1 cx c) (get1 cx a) (get1 cx b) i16 |]
  | Freeze a when rty 0 = F16 -> set cx (r 0) [| e cx (Freeze (get1 cx a)) i16 |]
  | Load (m, p) when rty 0 = F16 -> set cx (r 0) [| e cx (Load (m, get1 cx p)) i16 |]
  | Store (m, v, p) when src_ty cx v = F16 -> e0 cx (Store (m, get1 cx v, get1 cx p))
  | Va_arg p when rty 0 = F16 ->
      (* an SSE slot, as f32: the low 16 bits are the f16 *)
      let f = e cx (Va_arg (get1 cx p)) F32 in
      set cx (r 0) [| conv cx Trunc (conv cx Bitcast f (Int 32)) i16 |]
  | Rmw (Xchg, m, p, v) when src_ty cx v = F16 -> set cx (r 0) [| e cx (Rmw (Xchg, m, get1 cx p, get1 cx v)) i16 |]
  | Rmw (((Rfadd | Rfsub) as k), _, p, v) when src_ty cx v = F16 ->
      let sub = c64 cx (if k = Rfsub then 1L else 0L) in
      set cx (r 0) (call_helper cx "__fir_rmwh" [ ptr; i16; i64 ] [ i16 ] [| get1 cx p; get1 cx v; sub |])
  | Cmpxchg c when src_ty cx c.expected = F16 ->
      let rs = emit ~loc:cx.loc cx.g cx.b (Cmpxchg { c with p = get1 cx c.p; expected = get1 cx c.expected; desired = get1 cx c.desired }) [| i16; i1 |] in
      set cx (r 0) [| rs.(0) |];
      set cx (r 1) [| rs.(1) |]
  | Ibin (k, _, a, b) ->
      let n = match src_ty cx a with Int n -> n | t -> unsupported "%s" (ty_to_string t) in
      set cx (r 0) (ibin cx n k (get cx a) (get cx b))
  | Iun (k, _, a) ->
      let n = match src_ty cx a with Int n -> n | t -> unsupported "%s" (ty_to_string t) in
      set cx (r 0) (iun cx n k (get cx a))
  | Fsh (left, a, b, c) ->
      let n = match src_ty cx a with Int n -> n | t -> unsupported "%s" (ty_to_string t) in
      set cx (r 0) (fsh cx n ~left (get cx a) (get cx b) (get cx c))
  | Ovf (k, a, b) ->
      let n = match src_ty cx a with Int n -> n | t -> unsupported "%s" (ty_to_string t) in
      let v, o = ovf cx n k (get cx a) (get cx b) in
      set cx (r 0) v;
      set cx (r 1) [| o |]
  | Icmp (k, a, b) ->
      let n = match src_ty cx a with Int n -> n | t -> unsupported "icmp on %s" (ty_to_string t) in
      let x = get cx a and y = get cx b in
      let c =
        match k with
        | Eq -> eq cx x y
        | Ne -> not1 cx (eq cx x y)
        | Ult -> ult cx x y
        | Ugt -> ult cx y x
        | Ule -> not1 cx (ult cx y x)
        | Uge -> not1 cx (ult cx x y)
        | Slt -> slt cx n x y
        | Sgt -> slt cx n y x
        | Sle -> not1 cx (slt cx n y x)
        | Sge -> not1 cx (slt cx n x y)
      in
      set cx (r 0) [| c |]
  | Select (c, a, b) -> set cx (r 0) (select_l cx (get1 cx c) (get cx a) (get cx b))
  | Freeze a -> set cx (r 0) (Array.map (fun x -> e cx (Freeze x) i64) (get cx a))
  | Conv (k, a) -> set cx (r 0) (convert cx k a (rty 0))
  | Load (m, p) -> (
      match rty 0 with
      | Int n -> set cx (r 0) (load_limbs cx n m (get1 cx p))
      | t -> unsupported "load of %s" (ty_to_string t))
  | Store (m, v, p) -> (
      match src_ty cx v with
      | Int n -> store_limbs cx n m (get cx v) (get1 cx p)
      | t -> unsupported "store of %s" (ty_to_string t))
  | Alloca (sz, al) -> set cx (r 0) [| e cx (Alloca (low64 cx sz, al)) ptr |]
  | Ptradd (fl, p, idx, sc, d) -> set cx (r 0) [| e cx (Ptradd (fl, get1 cx p, low64 cx idx, sc, d)) ptr |]
  | Memcpy (vol, a, b, n) -> e0 cx (Memcpy (vol, get1 cx a, get1 cx b, low64 cx n))
  | Memmove (vol, a, b, n) -> e0 cx (Memmove (vol, get1 cx a, get1 cx b, low64 cx n))
  | Memset (vol, a, b, n) -> e0 cx (Memset (vol, get1 cx a, get1 cx b, low64 cx n))
  | Extract (vec, idx) when not (odd_value cx vec) ->
      set cx (r 0) [| e cx (Extract (get1 cx vec, low64 cx idx)) (rty 0) |]
  | Insert (vec, x, idx) when not (odd_value cx vec) ->
      set cx (r 0) [| e cx (Insert (get1 cx vec, get1 cx x, low64 cx idx)) (rty 0) |]
  | Syscall args -> set cx (r 0) [| e cx (Syscall (Array.map (low64 cx) args)) i64 |]
  | Rmw (((Xchg | Rand | Ror | Rxor) as k), m, p, v) -> (
      match src_ty cx v with
      | Int n when nlimbs n = 1 ->
          (* the container in memory; and/or/xor/xchg of normalized values
             keep its bits above n zero *)
          let c = container n in
          let x = (get cx v).(0) in
          let x = if c < 64 then conv cx Trunc x (Int c) else x in
          let old = e cx (Rmw (k, m, get1 cx p, x)) (Int c) in
          set cx (r 0) (mask cx n [| (if c < 64 then zext64 cx old else old) |])
      | t -> unsupported "atomic_rmw on %s" (ty_to_string t))
  | Cmpxchg c -> (
      match src_ty cx c.expected with
      | Int n when nlimbs n = 1 ->
          let ct = Int (container n) in
          let narrow x = if container n < 64 then conv cx Trunc (get cx x).(0) ct else (get cx x).(0) in
          let rs =
            emit ~loc:cx.loc cx.g cx.b
              (Cmpxchg { c with p = get1 cx c.p; expected = narrow c.expected; desired = narrow c.desired })
              [| ct; i1 |]
          in
          let old = if container n < 64 then zext64 cx rs.(0) else rs.(0) in
          set cx (r 0) (mask cx n [| old |]);
          set cx (r 1) [| rs.(1) |]
      | t -> unsupported "cmpxchg on %s" (ty_to_string t))
  | Va_arg p -> (
      match rty 0 with
      | Int n when nlimbs n = 1 -> set cx (r 0) (mask cx n [| e cx (Va_arg (get1 cx p)) i64 |])
      | Int n ->
          let q = e cx (Va_arg_agg (wide_agg n, get1 cx p)) ptr in
          set cx (r 0) (load_limbs cx n (plain_mem 8) q)
      | t -> unsupported "va_arg of %s" (ty_to_string t))
  | _ ->
      (* the op's mnemonic: the first word of its text *)
      let text = Fir_print.op_text cx.f (Fir_print.assign_names cx.f) ir in
      unsupported "%s on a wide or odd-width integer" (List.hd (String.split_on_char ' ' text))

(* ---------- terminators ---------- *)

let target_args cx (t : target) = { t with args = Array.concat (List.map (get cx) (Array.to_list t.args)) }

let rebuild_term cx (b : block) =
  let br = block cx.f b in
  cx.loc <- br.term_loc;
  let set_term t = Fir.set_term ~loc:br.term_loc cx.g cx.b t in
  match br.term with
  | Ret vs ->
      let abis = cx.f.sig_.result_abis in
      set_term
        (Ret
           (Array.mapi
              (fun k v -> to_abi cx (Some (List.nth abis k)) v)
              vs))
  | Jump t -> set_term (Jump (target_args cx t))
  | Br (c, t, f, w) -> set_term (Br (get1 cx c, target_args cx t, target_args cx f, w))
  | Indirect_br (v, bs) -> set_term (Indirect_br (get1 cx v, bs))
  | Asm_goto g -> set_term (Asm_goto { g with aargs = Array.map (get1 cx) g.aargs })
  | Unreachable -> set_term Unreachable
  | Switch (v, d, cases) when not (odd_value cx v) ->
      set_term (Switch (get1 cx v, target_args cx d, Array.map (fun (key, t) -> (key, target_args cx t)) cases))
  | Switch (v, d, cases) ->
      let n = match src_ty cx v with Int n -> n | _ -> assert false in
      let x = get cx v in
      let d = target_args cx d in
      let cases = Array.map (fun (key, t) -> (key, target_args cx t)) cases in
      if nlimbs n = 1 then set_term (Switch (x.(0), d, Array.map (fun (key, t) -> ([| key.(0) |], t)) cases))
      else begin
        (* a chain of compares, one new block per case after the first *)
        let nc = Array.length cases in
        if nc = 0 then set_term (Jump d)
        else
          Array.iteri
            (fun i (key, t) ->
              let hit = eq cx x (const_limbs cx n key) in
              if i = nc - 1 then set_term (Br (hit, t, d, -1))
              else begin
                let next = add_block cx.g in
                set_term (Br (hit, t, target next, -1));
                cx.b <- next
              end)
            cases
      end

(* ---------- functions ---------- *)

let func_needs (f : func) =
  sig_changes f.sig_
  || (f.defined
     &&
     let found = ref false in
     for v = 0 to num_values f - 1 do
       if mentions_odd (ty_of f v) then found := true
     done;
     Vec.iter
       (fun (ir : inst_rec) ->
         match ir.op with
         | Call c when sig_changes c.sig_ -> found := true
         | Fma _ when ty_of f ir.results.(0) = F80 -> found := true
         | _ -> ())
       f.insts;
     !found)

let rebuild_func m helpers (f : func) : func =
  let g = new_func ~linkage:f.linkage ~defined:f.defined f.name (sig_of f.sig_) in
  g.visibility <- f.visibility;
  g.fattrs <- f.fattrs;
  g.display <- f.display;
  g.decl_loc <- f.decl_loc;
  if not f.defined then g
  else begin
    resolve_all f;
    let cx = { m; f; g; map = Hashtbl.create 256; b = entry; loc = 0; helpers } in
    let nb = num_blocks f in
    for b = 0 to nb - 1 do
      ignore (add_block ~name:(block f b).bname g)
    done;
    (* parameters: the entry block's follow the new signature; the others
       get one parameter per limb *)
    let entry_vals =
      List.map2
        (fun (a : abi) p ->
          let t = match a with Scalar t | Ext (t, _) -> t | Agg _ -> ptr in
          (p, add_param g entry t))
        (sig_of f.sig_).param_abis
        (Array.to_list (block f entry).params)
    in
    for b = 1 to nb - 1 do
      Array.iter
        (fun p ->
          match ty_of f p with
          | Int n when not (legal_width n) -> set cx p (Array.init (nlimbs n) (fun _ -> add_param g b i64))
          | F16 -> set cx p [| add_param g b i16 |]
          | t when mentions_odd t -> unsupported "block parameter of type %s" (ty_to_string t)
          | t -> set cx p [| add_param g b t |])
        (block f b).params
    done;
    cx.b <- entry;
    List.iter2
      (fun (a : abi) (p, np) ->
        match a with
        | Agg _ -> set cx p [| np |]
        | _ -> set cx p (from_abi cx (ty_of f p) np))
      f.sig_.param_abis entry_vals;
    (* blocks in reverse postorder, so definitions come before uses;
       unreachable blocks are left empty *)
    let rpo = Fir_dom.reverse_postorder (Fir_dom.cfg f) in
    Array.iter
      (fun b ->
        cx.b <- b;
        Vec.iter (fun i -> convert_inst cx i) (block f b).insts;
        rebuild_term cx b)
      rpo;
    g
  end

(* ---------- module ---------- *)

let add_helpers (m : modul) helpers =
  (* each helper with the ones it calls *)
  let want = Hashtbl.create 8 in
  let rec need n =
    if not (Hashtbl.mem want n) then begin
      Hashtbl.replace want n ();
      List.iter need (snd (List.assoc n Fir_wide_runtime.all))
    end
  in
  Hashtbl.iter (fun n () -> need n) helpers;
  let present = Hashtbl.create 64 in
  Vec.iter (fun (f : func) -> Hashtbl.replace present f.name ()) m.funcs;
  let text =
    String.concat "\n"
      (List.filter_map
         (fun (n, (t, _)) -> if Hashtbl.mem want n && not (Hashtbl.mem present n) then Some t else None)
         Fir_wide_runtime.all)
  in
  if text <> "" then begin
    let rt = Fir_parse.parse_module ("triple \"x86_64-unknown-linux-gnu\"\n" ^ text) in
    Vec.iter (fun f -> add_func m f) rt.funcs
  end

(* Rewrites [m] in place. Afterwards no value, signature or memory access
   uses an integer width other than 1, 8, 16, 32 or 64. *)
let run (m : modul) =
  (* odd-width, wide and f80 atomics first: on containers or libatomic *)
  Fir_atomic.run m;
  let helpers = Hashtbl.create 8 in
  let n = Vec.length m.funcs in
  for i = 0 to n - 1 do
    let f = Vec.get m.funcs i in
    if func_needs f then Vec.set m.funcs i (rebuild_func m helpers f)
  done;
  if Hashtbl.length helpers > 0 then add_helpers m helpers
