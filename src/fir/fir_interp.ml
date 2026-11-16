(* Fir_interp: runs verified Fir with exactly the semantics of SPEC.md.

   It has two jobs (survey D2, roadmap v0.4):
   - compile-time evaluation for Fas (`const assert`, constants from function
     calls, `const { }`): it runs the same Fir the backends compile, so a
     constant means what the compiled code would compute;
   - a test oracle: native, LLVM and the interpreter must agree.

   The model:
   - every value is an array of lanes (a scalar has one); a lane is poison
     or its bits as little-endian 64-bit limbs (integers normalized to their
     width, floats as their IEEE bit pattern, pointers as the address);
   - memory is byte-addressed: each object (an alloca, a global, an
     aggregate copy) has a synthetic address, and an access must lie inside
     one live object. Pointers are plain integers, as SPEC §8 says, so
     ptrtoint/inttoptr round trips and pointer bytes in memory just work;
   - poison in memory is per byte: storing poison marks the stored bytes,
     and a load that reads any poison byte gives poison;
   - fresh memory (allocas) reads as zero: SPEC §8 allows any fixed value,
     and zero keeps runs deterministic.

   What it refuses, with [Error]:
   - undefined behaviour it can see: division by zero (without `trap`),
     branching on poison, poison addresses, an access outside every live
     object or breaking its `align`, writing a readonly global or an
     aggregate parameter, overlapping memcpy, reaching `unreachable`, a
     call with the wrong signature;
   - what it cannot do: inline assembly, f80 arithmetic, a va_list whose
     register save area is in use, and anything the host refuses (calls to
     functions with no body, syscalls);
   - running longer than the step limit or nesting calls too deeply.
   The one UB it does not check is accessing a non-escaped object through
   an unrelated pointer: such a pointer must have been computed from the
   object's address, which the interpreter cannot tell from a guess.

   `trap` and trapping divisions end the run with [Trapped]. *)

open Fir

exception Error of string

let error fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

type lane = Poison | Bits of int64 array
type v = lane array
type trap_kind = Trap_insn | Trap_div

exception Trapped_exn of trap_kind
exception Exit_exn of int

(* ---------- wide integers ----------
   Limbs are little-endian; [w] is the width in bits. Every function takes
   normalized operands of [limbs_for w] limbs and returns normalized limbs. *)
module W = struct
  let nl = limbs_for
  let norm = normalize_limbs
  let zero w = Array.make (nl w) 0L
  let one w = norm w [| 1L |]
  let of_int64 = limbs_of_int64
  let all_ones w = norm w (Array.make (nl w) (-1L))
  let is_zero a = Array.for_all (fun x -> x = 0L) a
  let equal (a : int64 array) b = a = b

  let bit a i =
    let k = i / 64 in
    k < Array.length a
    && Int64.logand (Int64.shift_right_logical a.(k) (i mod 64)) 1L <> 0L

  let neg_bit w a = bit a (w - 1)

  let ult a b =
    let r = ref 0 in
    let i = ref (Array.length a - 1) in
    while !r = 0 && !i >= 0 do
      r := Int64.unsigned_compare a.(!i) b.(!i);
      decr i
    done;
    !r < 0

  let slt w a b =
    let sa = neg_bit w a and sb = neg_bit w b in
    if sa <> sb then sa else ult a b

  let add w a b =
    let n = nl w in
    let r = Array.make n 0L in
    let carry = ref 0L in
    for i = 0 to n - 1 do
      let s1 = Int64.add a.(i) b.(i) in
      let c1 = Int64.unsigned_compare s1 a.(i) < 0 in
      let s2 = Int64.add s1 !carry in
      let c2 = Int64.unsigned_compare s2 s1 < 0 in
      r.(i) <- s2;
      carry := if c1 || c2 then 1L else 0L
    done;
    norm w r

  let lognot w a = norm w (Array.map Int64.lognot a)
  let neg w a = add w (lognot w a) (one w)
  let sub w a b = add w a (neg w b)
  let logand a b = Array.map2 Int64.logand a b
  let logor a b = Array.map2 Int64.logor a b
  let logxor a b = Array.map2 Int64.logxor a b

  (* shifts by 0 <= k < w *)
  let shl w a k =
    let n = nl w in
    let q = k / 64 and s = k mod 64 in
    let get i = if i < 0 || i >= n then 0L else a.(i) in
    norm w
      (Array.init n (fun i ->
           let hi = Int64.shift_left (get (i - q)) s in
           if s = 0 then hi
           else Int64.logor hi (Int64.shift_right_logical (get (i - q - 1)) (64 - s))))

  let lshr_fill w a k fill =
    let n = nl w in
    (* view [a] as extended with [fill] limbs above its width *)
    let a =
      if fill = 0L || w mod 64 = 0 then a
      else begin
        let a = Array.copy a in
        let top = w mod 64 in
        a.(n - 1) <- Int64.logor a.(n - 1) (Int64.shift_left (-1L) top);
        a
      end
    in
    let q = k / 64 and s = k mod 64 in
    let get i = if i >= n then fill else a.(i) in
    norm w
      (Array.init n (fun i ->
           let lo = Int64.shift_right_logical (get (i + q)) s in
           if s = 0 then lo
           else Int64.logor lo (Int64.shift_left (get (i + q + 1)) (64 - s))))

  let lshr w a k = lshr_fill w a k 0L
  let ashr w a k = lshr_fill w a k (if neg_bit w a then -1L else 0L)

  (* the 128-bit product of two unsigned limbs, as (low, high) *)
  let umul64 a b =
    let m = 0xFFFFFFFFL in
    let a0 = Int64.logand a m and a1 = Int64.shift_right_logical a 32 in
    let b0 = Int64.logand b m and b1 = Int64.shift_right_logical b 32 in
    let p00 = Int64.mul a0 b0 and p01 = Int64.mul a0 b1 in
    let p10 = Int64.mul a1 b0 and p11 = Int64.mul a1 b1 in
    let mid =
      Int64.add
        (Int64.shift_right_logical p00 32)
        (Int64.add (Int64.logand p01 m) (Int64.logand p10 m))
    in
    let lo = Int64.logor (Int64.logand p00 m) (Int64.shift_left mid 32) in
    let hi =
      Int64.add p11
        (Int64.add
           (Int64.shift_right_logical p01 32)
           (Int64.add
              (Int64.shift_right_logical p10 32)
              (Int64.shift_right_logical mid 32)))
    in
    (lo, hi)

  (* the full unsigned product: length a + length b limbs *)
  let mul_full a b =
    let na = Array.length a and nb = Array.length b in
    let r = Array.make (na + nb) 0L in
    for i = 0 to na - 1 do
      let carry = ref 0L in
      for j = 0 to nb - 1 do
        let lo, hi = umul64 a.(i) b.(j) in
        let s1 = Int64.add r.(i + j) lo in
        let c1 = if Int64.unsigned_compare s1 lo < 0 then 1L else 0L in
        let s2 = Int64.add s1 !carry in
        let c2 = if Int64.unsigned_compare s2 !carry < 0 then 1L else 0L in
        r.(i + j) <- s2;
        (* the row's running total stays below 2^128, so this cannot wrap *)
        carry := Int64.add hi (Int64.add c1 c2)
      done;
      r.(i + nb) <- !carry
    done;
    r

  let mul w a b = norm w (Array.sub (mul_full a b) 0 (nl w))

  (* width changes *)
  let zext w' a = norm w' a
  let trunc w' a = norm w' a

  let sext w w' a =
    if not (neg_bit w a) then norm w' a
    else
      let n' = nl w' in
      let r = Array.make n' (-1L) in
      Array.blit a 0 r 0 (Array.length a);
      (* the bits of the top source limb above [w] become ones *)
      let k = Array.length a - 1 in
      if w mod 64 <> 0 then
        r.(k) <- Int64.logor a.(k) (Int64.shift_left (-1L) (w mod 64));
      norm w' r

  (* unsigned division, a / b with b <> 0 *)
  let udivmod w a b =
    if w <= 64 then
      ([| Int64.unsigned_div a.(0) b.(0) |], [| Int64.unsigned_rem a.(0) b.(0) |])
    else begin
      let q = ref (zero w) and r = ref (zero w) in
      for i = w - 1 downto 0 do
        r := shl w !r 1;
        if bit a i then !r.(0) <- Int64.logor !r.(0) 1L;
        if not (ult !r b) then begin
          r := sub w !r b;
          let k = i / 64 in
          !q.(k) <- Int64.logor !q.(k) (Int64.shift_left 1L (i mod 64))
        end
      done;
      (!q, !r)
    end

  let abs w a = if neg_bit w a then neg w a else a

  (* signed division, truncating toward zero; the caller excludes b = 0 *)
  let sdivmod w a b =
    let q, r = udivmod w (abs w a) (abs w b) in
    let q = if neg_bit w a <> neg_bit w b then neg w q else q in
    let r = if neg_bit w a then neg w r else r in
    (q, r)

  let min_signed w = shl w (one w) (w - 1)
  let max_signed w = lognot w (min_signed w)

  let popcount a =
    Array.fold_left
      (fun acc x ->
        let c = ref 0 and x = ref x in
        while !x <> 0L do
          x := Int64.logand !x (Int64.pred !x);
          incr c
        done;
        acc + !c)
      0 a

  let ctz w a =
    let r = ref w in
    (try
       for i = 0 to w - 1 do
         if bit a i then begin
           r := i;
           raise Exit
         end
       done
     with Exit -> ());
    !r

  (* index of the highest set bit, -1 for zero *)
  let top_bit a =
    let r = ref (-1) in
    let i = ref (Array.length a - 1) in
    while !r < 0 && !i >= 0 do
      let x = a.(!i) in
      if x <> 0L then begin
        let b = ref 63 in
        while Int64.logand (Int64.shift_right_logical x !b) 1L = 0L do
          decr b
        done;
        r := (!i * 64) + !b
      end;
      decr i
    done;
    !r

  let clz w a = w - 1 - top_bit a

  let set_bit a i =
    let k = i / 64 in
    a.(k) <- Int64.logor a.(k) (Int64.shift_left 1L (i mod 64))

  let bitrev w a =
    let r = zero w in
    for i = 0 to w - 1 do
      if bit a i then set_bit r (w - 1 - i)
    done;
    r

  let byte a k =
    let i = k / 8 in
    if i >= Array.length a then 0
    else
      Int64.to_int
        (Int64.logand (Int64.shift_right_logical a.(i) (8 * (k mod 8))) 0xffL)

  let of_bytes w (get : int -> int) nbytes =
    let r = Array.make (nl w) 0L in
    for k = 0 to nbytes - 1 do
      let i = k / 8 in
      if i < Array.length r then
        r.(i) <-
          Int64.logor r.(i) (Int64.shift_left (Int64.of_int (get k)) (8 * (k mod 8)))
    done;
    norm w r

  let bswap w a =
    let n = w / 8 in
    of_bytes w (fun k -> byte a (n - 1 - k)) n

  (* [len] <= 63 bits starting at bit [lo], as a non-negative int64 *)
  let bits_at a lo len =
    let r = ref 0L in
    for i = len - 1 downto 0 do
      r := Int64.logor (Int64.shift_left !r 1) (if bit a (lo + i) then 1L else 0L)
    done;
    !r

  let any_below a n =
    let r = ref false in
    for i = 0 to n - 1 do
      if bit a i then r := true
    done;
    !r

  (* a small non-negative value, if it fits an OCaml int *)
  let to_int_opt a =
    let rest_zero = ref true in
    for i = 1 to Array.length a - 1 do
      if a.(i) <> 0L then rest_zero := false
    done;
    if
      !rest_zero
      && Int64.compare a.(0) 0L >= 0
      && Int64.compare a.(0) (Int64.of_int max_int) <= 0
    then Some (Int64.to_int a.(0))
    else None

  (* the value as a signed int64 (low 64 bits, sign-extended from w) *)
  let to_int64_signed w a =
    let x = a.(0) in
    if w >= 64 then x else Int64.shift_right (Int64.shift_left x (64 - w)) (64 - w)
end

(* ---------- floats ---------- *)

let roundeven x =
  if Float.is_nan x || Float.is_integer x then x
  else
    let f = Float.floor x in
    let d = x -. f in
    let r =
      if d < 0.5 then f
      else if d > 0.5 then f +. 1.
      else if Float.rem f 2. = 0. then f
      else f +. 1.
    in
    Float.copy_sign r x

let f16_to_float (b : int) =
  let sign = if b land 0x8000 <> 0 then -1. else 1. in
  let e = (b lsr 10) land 0x1f and m = b land 0x3ff in
  if e = 0x1f then if m = 0 then sign *. infinity else Float.copy_sign Float.nan sign
  else if e = 0 then sign *. Float.ldexp (float_of_int m) (-24)
  else sign *. Float.ldexp (float_of_int (m lor 0x400)) (e - 25)

(* round to nearest, ties to even *)
let f16_of_float x =
  let sign = if Float.sign_bit x then 0x8000 else 0 in
  if Float.is_nan x then sign lor 0x7e00
  else
    let a = Float.abs x in
    if a = Float.infinity then sign lor 0x7c00
    else if a = 0. then sign
    else
      let _, e = Float.frexp a in
      let exp = e - 1 in
      if exp > 15 then sign lor 0x7c00
      else
        let q = if exp < -14 then -24 else exp - 10 in
        let r = int_of_float (roundeven (Float.ldexp a (-q))) in
        if q = -24 then sign lor r
        else
          let bits = ((exp + 15) lsl 10) + (r - 1024) in
          if bits >= 0x7c00 then sign lor 0x7c00 else sign lor bits

let f32_to_float (b : int64) = Int32.float_of_bits (Int64.to_int32 b)
let f32_bits x = Int64.logand (Int64.of_int32 (Int32.bits_of_float x)) 0xffffffffL

(* precision in bits (with the hidden one) of a format *)
let precision = function F16 -> 11 | F32 -> 24 | F64 -> 53 | _ -> 64
let no_f80 () = error "f80 arithmetic is not supported by the interpreter"

let to_float ty (l : int64 array) =
  match ty with
  | F16 -> f16_to_float (Int64.to_int l.(0))
  | F32 -> f32_to_float l.(0)
  | F64 -> Int64.float_of_bits l.(0)
  | _ -> no_f80 ()

(* rounds an f64 to the format (exact for values the format holds) *)
let of_float ty x =
  match ty with
  | F16 -> [| Int64.of_int (f16_of_float x) |]
  | F32 -> [| f32_bits x |]
  | F64 -> [| Int64.bits_of_float x |]
  | _ -> no_f80 ()

(* a·b+c rounded once to a format of at most 24 bits. The product is exact
   in f64; the sum is rounded to odd (rounded to nearest, then nudged to the
   odd neighbour when inexact), which makes the final rounding correct *)
let fma_narrow ty a b c =
  let p = a *. b in
  let s = p +. c in
  let s =
    if Float.is_finite s && s <> 0. then
      let bb = s -. p in
      let err = p -. (s -. bb) +. (c -. bb) in
      if err = 0. then s
      else
        let bits = Int64.bits_of_float s in
        if Int64.logand bits 1L = 1L then s
        else if err > 0. = (s > 0.) then Int64.float_of_bits (Int64.succ bits)
        else Int64.float_of_bits (Int64.pred bits)
    else s
  in
  of_float ty s

(* IEEE minimumNumber / maximumNumber *)
let fmin_num a b =
  if Float.is_nan a then b
  else if Float.is_nan b then a
  else if a = 0. && b = 0. then if Float.sign_bit a then a else b
  else Float.min a b

let fmax_num a b =
  if Float.is_nan a then b
  else if Float.is_nan b then a
  else if a = 0. && b = 0. then if Float.sign_bit a then b else a
  else Float.max a b

let sign_bit_of ty =
  match ty with F16 -> 15 | F32 -> 31 | F64 -> 63 | F80 -> 79 | _ -> assert false

let flip_bit (l : int64 array) i set =
  let l = Array.copy l in
  let k = i / 64 and m = Int64.shift_left 1L (i mod 64) in
  l.(k) <-
    (match set with
    | `Flip -> Int64.logxor l.(k) m
    | `Clear -> Int64.logand l.(k) (Int64.lognot m)
    | `Set -> Int64.logor l.(k) m);
  l

(* an integer of width w, read signed or unsigned, rounded to the format *)
let float_of_int_limbs ty ~signed w a =
  let neg = signed && W.neg_bit w a in
  let mag = if neg then W.neg w a else a in
  let top = W.top_bit mag in
  let x =
    if top < 0 then 0.
    else
      let p = precision ty in
      let m, e =
        if top < p then (W.bits_at mag 0 (top + 1), 0)
        else
          let lo = top - p + 1 in
          let m = W.bits_at mag lo p in
          let round = W.bit mag (lo - 1) and sticky = W.any_below mag (lo - 1) in
          let m =
            if round && (sticky || Int64.logand m 1L = 1L) then Int64.succ m else m
          in
          (m, lo)
      in
      let x = Float.ldexp (Int64.to_float m) e in
      match ty with
      | F16 when x >= 65536. -> Float.infinity
      | F32 when x >= Float.ldexp 1. 128 -> Float.infinity
      | _ -> x
  in
  of_float ty (if neg then -.x else x)

(* an integral float as w-bit limbs (two's complement when negative) *)
let limbs_of_integral w t =
  if t = 0. then W.zero w
  else
    let ww = w + 128 in
    let m, e = Float.frexp (Float.abs t) in
    let mi = Int64.of_float (Float.ldexp m 53) in
    let sh = e - 53 in
    let mag = W.of_int64 ww mi in
    let mag = if sh >= 0 then W.shl ww mag sh else W.lshr ww mag (-sh) in
    let r = if t < 0. then W.neg ww mag else mag in
    W.trunc w r

(* fptosi/fptoui: None when out of range (poison) *)
let int_of_float_opt ~signed w x =
  if Float.is_nan x || (Float.is_integer x = false && not (Float.is_finite x)) then None
  else
    let t = Float.trunc x in
    let ok =
      if signed then t >= -.Float.ldexp 1. (w - 1) && t < Float.ldexp 1. (w - 1)
      else (t >= 0. && t < Float.ldexp 1. w) || t = 0.
    in
    if ok then Some (limbs_of_integral w t) else None

let int_of_float_sat ~signed w x =
  if Float.is_nan x then W.zero w
  else
    match int_of_float_opt ~signed w x with
    | Some l -> l
    | None ->
        if signed then if x < 0. then W.min_signed w else W.max_signed w
        else if x < 0. then W.zero w
        else W.all_ones w

(* ---------- memory ---------- *)

type obj = {
  base : int64;
  osize : int;
  data : Bytes.t;
  mutable pois : Bytes.t option;  (** one byte per data byte: 1 = poison *)
  writable : bool;
  what : string;  (** for messages: "alloca", "global `@x`", ... *)
  var_global : bool;  (** a writable global: Fas `var` *)
  mutable ro : (int * int) list;
      (** read-only (offset, length) ranges inside a writable object *)
}

module Amap = Map.Make (Int64)

(* ---------- programs ---------- *)

type finfo = {
  mi : int;  (** module index *)
  fn : func;
  cvals : v option array;  (** constants, computed once *)
  faddr : int64;
}

type sym =
  | Fn of finfo
  | Ext_fn of string * int64
  | Obj of obj
  | Part of obj * int  (** a global inside a section object, at this offset *)
  | Ext_gl of string * int64
  | Addr of int64

type frame = {
  fi : finfo;
  vals : v array;
  mutable allocas : obj list;  (** newest first *)
  mutable nallocas : int;
  marks : (int64, int) Hashtbl.t;  (** stacksave token -> allocas then *)
  varargs : int64;  (** the overflow area of the variadic arguments, 0 when none *)
}

type state = {
  modules : modul array;
  mutable objs : obj Amap.t;
  mutable next_addr : int64;
  globals_tbl : (string, sym) Hashtbl.t;  (** global and weak symbols *)
  locals_tbl : (int * string, sym) Hashtbl.t;  (** per-module local symbols *)
  by_addr : (int64, sym) Hashtbl.t;  (** function addresses *)
  blockaddrs : (int64, string * int) Hashtbl.t;
  blockaddr_of : (string * int, int64) Hashtbl.t;
  mutable steps : int;
  mutable depth : int;
  cfg : config;
  mutable token : int64;
}

and config = {
  step_limit : int;
  max_depth : int;
  allow_var_globals : bool;
      (** false: touching a writable global is an error (Fas `var`) *)
  syscall : state -> int64 array -> int64;
  extern_call : state -> string -> signature -> v array -> v array;
}

let default_step_limit = 10_000_000
let default_max_depth = 10_000

let refuse_syscall _ (a : int64 array) =
  error "makes a system call (number %Ld), which compile-time evaluation cannot do"
    a.(0)

let refuse_extern _ name _ _ =
  error "calls `%s`, which has no Fir body (a C function)" name

let const_config =
  {
    step_limit = default_step_limit;
    max_depth = default_max_depth;
    allow_var_globals = false;
    syscall = refuse_syscall;
    extern_call = refuse_extern;
  }

let align_up (a : int64) n =
  let n = Int64.of_int n in
  Int64.mul (Int64.div (Int64.add a (Int64.pred n)) n) n

let new_obj st ~size ~align ~writable ~what ~var_global =
  let base = align_up st.next_addr (max align 16) in
  (* a gap after each object: a one-past-the-end pointer is never the next
     object's first byte, and empty objects get distinct addresses *)
  st.next_addr <- Int64.add base (Int64.of_int (size + 16));
  let o =
    {
      base;
      osize = size;
      data = Bytes.make size '\000';
      pois = None;
      writable;
      what;
      var_global;
      ro = [];
    }
  in
  st.objs <- Amap.add base o st.objs;
  o

let free_obj st o = st.objs <- Amap.remove o.base st.objs

(* the object holding [len] bytes at [addr], and the offset in it *)
let locate st addr len =
  match Amap.find_last_opt (fun b -> Int64.compare b addr <= 0) st.objs with
  | Some (_, o) ->
      let off = Int64.sub addr o.base in
      if
        Int64.compare off 0L >= 0
        && Int64.compare (Int64.add off (Int64.of_int len)) (Int64.of_int o.osize) <= 0
      then (o, Int64.to_int off)
      else error "accesses %d bytes at 0x%Lx, outside every live object" len addr
  | None -> error "accesses %d bytes at 0x%Lx, outside every live object" len addr

let check_var st o =
  if o.var_global && not st.cfg.allow_var_globals then
    error "touches the variable %s" o.what

let check_align addr align =
  if align > 1 && Int64.rem addr (Int64.of_int align) <> 0L then
    error "accesses 0x%Lx, which breaks its promise of align %d" addr align

(* raw bytes; None marks a poison byte *)
let read_raw st addr len =
  if len = 0 then [||]
  else
    let o, off = locate st addr len in
    check_var st o;
    Array.init len (fun k ->
        match o.pois with
        | Some p when Bytes.get p (off + k) <> '\000' -> None
        | _ -> Some (Char.code (Bytes.get o.data (off + k))))

let write_raw st addr (bytes : int option array) =
  let len = Array.length bytes in
  if len > 0 then begin
    let o, off = locate st addr len in
    check_var st o;
    if not o.writable then error "writes %s, which is read-only" o.what;
    List.iter
      (fun (s, n) ->
        if off < s + n && s < off + len then
          error "writes a read-only global in %s" o.what)
      o.ro;
    Array.iteri
      (fun k b ->
        match b with
        | Some x ->
            Bytes.set o.data (off + k) (Char.chr x);
            Option.iter (fun p -> Bytes.set p (off + k) '\000') o.pois
        | None ->
            let p =
              match o.pois with
              | Some p -> p
              | None ->
                  let p = Bytes.make o.osize '\000' in
                  o.pois <- Some p;
                  p
            in
            Bytes.set p (off + k) '\001')
      bytes
  end

(* for hosts: plain bytes (poison reads as zero) *)
let read_bytes st addr len =
  String.init len (fun k ->
      match (read_raw st addr len).(k) with Some x -> Char.chr x | None -> '\000')

let read_string st addr len =
  if len = 0 then ""
  else
    let raw = read_raw st addr len in
    String.init len (fun k ->
        match raw.(k) with Some x -> Char.chr x | None -> '\000')

let _ = read_bytes

(* ---------- encoding values in memory ---------- *)

(* bytes a scalar of this type occupies when stored; f80 writes its 10 *)
let store_size = function F80 -> 10 | t -> size_of t

let lane_width = function
  | Int n -> n
  | F16 -> 16
  | F32 -> 32
  | F64 | Ptr _ -> 64
  | F80 -> 80
  | Vec _ -> assert false

let encode ty (x : v) : int option array =
  match ty with
  | Vec (n, Int 1) ->
      let out = Array.make ((n + 7) / 8) (Some 0) in
      Array.iteri
        (fun i l ->
          let k = i / 8 in
          match (l, out.(k)) with
          | Poison, _ -> out.(k) <- None
          | Bits b, Some acc ->
              if b.(0) <> 0L then out.(k) <- Some (acc lor (1 lsl (i mod 8)))
          | Bits _, None -> ())
        x;
      out
  | _ ->
      let e = elem ty in
      let stride = size_of e and ss = store_size e in
      let out = Array.make (((lanes ty - 1) * stride) + ss) (Some 0) in
      Array.iteri
        (fun i l ->
          for k = 0 to ss - 1 do
            out.((i * stride) + k) <-
              (match l with Poison -> None | Bits b -> Some (W.byte b k))
          done)
        x;
      out

(* the gaps between lanes (vectors of f80) are not written *)
let write_value st ty addr x =
  let bytes = encode ty x in
  match ty with
  | Vec (_, F80) ->
      Array.iteri
        (fun i _ ->
          write_raw st
            (Int64.add addr (Int64.of_int (i * 16)))
            (Array.sub bytes (i * 16) 10))
        x
  | _ -> write_raw st addr bytes

let decode ty (raw : int option array) : v =
  (* a load that reads any poison byte gives poison (SPEC §8) *)
  if Array.exists Option.is_none raw then Array.make (lanes ty) Poison
  else
    let byte k = Option.get raw.(k) in
    match ty with
    | Vec (n, Int 1) ->
        Array.init n (fun i ->
            Bits [| (if byte (i / 8) land (1 lsl (i mod 8)) <> 0 then 1L else 0L) |])
    | _ ->
        let e = elem ty in
        let stride = size_of e and ss = store_size e in
        let w = lane_width e in
        Array.init (lanes ty) (fun i ->
            Bits (W.of_bytes w (fun k -> byte ((i * stride) + k)) ss))

let read_value st ty addr =
  match ty with
  | Vec (n, F80) ->
      Array.init n (fun i ->
          (decode F80 (read_raw st (Int64.add addr (Int64.of_int (i * 16))) 10)).(0))
  | _ ->
      let len =
        match ty with
        | Vec (n, Int 1) -> (n + 7) / 8
        | _ -> ((lanes ty - 1) * size_of (elem ty)) + store_size (elem ty)
      in
      decode ty (read_raw st addr len)

(* ---------- symbols ---------- *)

let fn_region = 0x1_0000_0000L
let block_region = 0x2_0000_0000L

let lookup st mi name =
  match Hashtbl.find_opt st.locals_tbl (mi, name) with
  | Some s -> s
  | None -> (
      match Hashtbl.find_opt st.globals_tbl name with
      | Some s -> s
      | None -> error "refers to `%s`, which is defined nowhere" name)

let sym_addr = function
  | Fn fi -> fi.faddr
  | Ext_fn (_, a) | Ext_gl (_, a) | Addr a -> a
  | Obj o -> o.base
  | Part (o, off) -> Int64.add o.base (Int64.of_int off)

let blockaddr st fname b =
  match Hashtbl.find_opt st.blockaddr_of (fname, b) with
  | Some a -> a
  | None ->
      let a =
        Int64.add block_region (Int64.of_int (16 * Hashtbl.length st.blockaddr_of))
      in
      Hashtbl.replace st.blockaddr_of (fname, b) a;
      Hashtbl.replace st.blockaddrs a (fname, b);
      a

let finfo mi fn faddr = { mi; fn; cvals = Array.make (num_values fn) None; faddr }

(* lays out every module: functions get addresses, globals objects *)
let create cfg (ms : modul list) =
  let st =
    {
      modules = Array.of_list ms;
      objs = Amap.empty;
      next_addr = 0x10_0000L;
      globals_tbl = Hashtbl.create 64;
      locals_tbl = Hashtbl.create 64;
      by_addr = Hashtbl.create 64;
      blockaddrs = Hashtbl.create 8;
      blockaddr_of = Hashtbl.create 8;
      steps = 0;
      depth = 0;
      cfg;
      token = 0x3_0000_0000L;
    }
  in
  let next_fn = ref fn_region in
  let fresh_fn_addr () =
    let a = !next_fn in
    next_fn := Int64.add a 16L;
    a
  in
  let define mi linkage name s =
    match linkage with
    | Local -> Hashtbl.replace st.locals_tbl (mi, name) s
    | Global -> Hashtbl.replace st.globals_tbl name s
    | Weak ->
        if not (Hashtbl.mem st.globals_tbl name) then
          Hashtbl.replace st.globals_tbl name s
  in
  (* strong definitions first, so a weak one never shadows them *)
  let sections = ref [] in
  let sectioned (g : global) =
    match g.section with
    | Some s when not g.thread_local ->
        s <> ""
        && String.for_all
             (fun c ->
               c = '_'
               || ('a' <= c && c <= 'z')
               || ('A' <= c && c <= 'Z')
               || ('0' <= c && c <= '9'))
             s
    | _ -> false
  in
  let pass weak =
    Array.iteri
      (fun mi (m : modul) ->
        Vec.iter
          (fun (f : func) ->
            if f.defined && f.linkage = Weak = weak then begin
              let fi = finfo mi f (fresh_fn_addr ()) in
              Hashtbl.replace st.by_addr fi.faddr (Fn fi);
              define mi f.linkage f.name (Fn fi)
            end)
          m.funcs;
        Vec.iter
          (fun (g : global) ->
            if g.gdefined && g.glinkage = Weak = weak && sectioned g then
              sections := (mi, g) :: !sections
            else if g.gdefined && g.glinkage = Weak = weak then begin
              let what = "global @" ^ g.gname in
              let o =
                new_obj st ~size:g.size ~align:g.galign ~writable:(not g.readonly) ~what
                  ~var_global:(not g.readonly)
              in
              define mi g.glinkage g.gname (Obj o)
            end)
          m.globals)
      st.modules
  in
  pass false;
  pass true;
  (* the globals of a section named like a C identifier share one object,
     laid out in order, which __start_s and __stop_s bound (SPEC §3) *)
  let order = ref [] in
  List.iter
    (fun (_, (g : global)) ->
      let s = Option.get g.section in
      if not (List.mem s !order) then order := s :: !order)
    (List.rev !sections);
  List.iter
    (fun s ->
      let members =
        List.filter (fun (_, (g : global)) -> g.section = Some s) (List.rev !sections)
      in
      let size, offs =
        List.fold_left
          (fun (pos, acc) (_, (g : global)) ->
            let at = (pos + g.galign - 1) / g.galign * g.galign in
            (at + g.size, at :: acc))
          (0, []) members
      in
      let offs = List.rev offs in
      let align =
        List.fold_left (fun a (_, (g : global)) -> max a g.galign) 1 members
      in
      let o =
        new_obj st ~size ~align ~writable:true
          ~what:("section \"" ^ s ^ "\"")
          ~var_global:(List.exists (fun (_, (g : global)) -> not g.readonly) members)
      in
      List.iter2
        (fun (mi, (g : global)) off ->
          if g.readonly then o.ro <- (off, g.size) :: o.ro;
          define mi g.glinkage g.gname (Part (o, off)))
        members offs;
      List.iter
        (fun (name, a) ->
          if not (Hashtbl.mem st.globals_tbl name) then
            Hashtbl.replace st.globals_tbl name (Addr a))
        [
          ("__start_" ^ s, o.base); ("__stop_" ^ s, Int64.add o.base (Int64.of_int size));
        ])
    (List.rev !order);
  (* declarations nothing defines *)
  Array.iteri
    (fun mi (m : modul) ->
      Vec.iter
        (fun (f : func) ->
          if (not f.defined) && not (Hashtbl.mem st.globals_tbl f.name) then
            if f.linkage = Weak then
              Hashtbl.replace st.locals_tbl (mi, f.name) (Addr 0L)
            else begin
              let a = fresh_fn_addr () in
              let s = Ext_fn (f.name, a) in
              Hashtbl.replace st.by_addr a s;
              Hashtbl.replace st.globals_tbl f.name s
            end)
        m.funcs;
      Vec.iter
        (fun (g : global) ->
          if (not g.gdefined) && not (Hashtbl.mem st.globals_tbl g.gname) then
            if g.glinkage = Weak then
              Hashtbl.replace st.locals_tbl (mi, g.gname) (Addr 0L)
            else
              Hashtbl.replace st.globals_tbl g.gname
                (Ext_gl (g.gname, fresh_fn_addr ())))
        m.globals)
    st.modules;
  (* aliases name their target, plus an addend *)
  Array.iteri
    (fun mi (m : modul) ->
      Vec.iter
        (fun (a : alias) ->
          if a.aname <> "" then
            let s =
              match lookup st mi a.target with
              | Fn fi -> Fn fi
              | s -> Addr (Int64.add (sym_addr s) a.addend)
            in
            define mi a.alinkage a.aname s)
        m.aliases)
    st.modules;
  (* initializers, now that every address is known *)
  Array.iteri
    (fun mi (m : modul) ->
      Vec.iter
        (fun (g : global) ->
          if g.gdefined then
            match lookup st mi g.gname with
            | (Obj _ | Part _) as s -> (
                let o, start =
                  match s with
                  | Part (o, off) -> (o, off)
                  | Obj o -> (o, 0)
                  | _ -> assert false
                in
                match g.init with
                | None -> ()
                | Some pieces ->
                    let pos = ref start in
                    let put_addr a width =
                      for k = 0 to width - 1 do
                        Bytes.set o.data (!pos + k) (Char.chr (W.byte [| a |] k))
                      done;
                      pos := !pos + width
                    in
                    List.iter
                      (function
                        | Bytes s ->
                            Bytes.blit_string s 0 o.data !pos (String.length s);
                            pos := !pos + String.length s
                        | Zeros k -> pos := !pos + k
                        | Reloc { sym; addend; width; pcrel } ->
                            let a = Int64.add (sym_addr (lookup st mi sym)) addend in
                            (* pcrel: relative to these bytes' own address *)
                            let here =
                              Int64.add (sym_addr s) (Int64.of_int (!pos - start))
                            in
                            put_addr (if pcrel then Int64.sub a here else a) width
                        | Blockref { fn; block } -> put_addr (blockaddr st fn block) 8)
                      pieces)
            | _ -> ())
        m.globals)
    st.modules;
  st

(* ---------- evaluation ---------- *)

let bool_lane b = Bits [| (if b then 1L else 0L) |]
let ptr_of = function Bits b -> b.(0) | Poison -> error "uses poison as an address"

let const_value st mi f c : v =
  let rec go c =
    match c with
    | Cint (ty, l) -> Array.make (lanes ty) (Bits l)
    | Cfloat (ty, l) -> Array.make (lanes ty) (Bits l)
    | Cnull ty -> Array.make (lanes ty) (Bits [| 0L |])
    | Cpoison ty -> Array.make (lanes ty) Poison
    | Czero ty ->
        let e = elem ty in
        Array.make (lanes ty)
          (Bits
             (Array.make (match e with F80 -> 2 | _ -> limbs_for (lane_width e)) 0L))
    | Cvec (_, ls) ->
        Array.map
          (fun l ->
            match def_of f (resolve f l) with
            | Const c -> (go c).(0)
            | _ -> error "a vector constant lane is not a constant")
          ls
    | Csym (name, addend) ->
        [| Bits [| Int64.add (sym_addr (lookup st mi name)) addend |] |]
    | Cblockaddr (fn, b) -> [| Bits [| blockaddr st fn b |] |]
  in
  go c

let get st fr v =
  let f = fr.fi.fn in
  let v = resolve f v in
  match def_of f v with
  | Const c -> (
      match fr.fi.cvals.(v) with
      | Some x -> x
      | None ->
          let x = const_value st fr.fi.mi f c in
          fr.fi.cvals.(v) <- Some x;
          x)
  | _ ->
      let x = fr.vals.(v) in
      if Array.length x = 0 then error "reads %%%d before it is defined" v;
      x

let get1 st fr v = (get st fr v).(0)

(* a lane-wise map; a scalar operand broadcasts *)
let lane (x : v) i = if Array.length x = 1 then x.(0) else x.(i)
let map2 n f a b = Array.init n (fun i -> f (lane a i) (lane b i))
let poison_if b l = if b then Poison else l

let ibin_lane k flags w a b : lane =
  match (a, b) with
  | _, Poison when match k with Udiv | Sdiv | Urem | Srem -> true | _ -> false ->
      error "divides by poison"
  | Poison, _ | _, Poison -> Poison
  | Bits a, Bits b -> (
      let nsw = has flags f_nsw and nuw = has flags f_nuw in
      let sshift () =
        match W.to_int_opt b with Some s when s < w -> Some s | _ -> None
      in
      match k with
      | Add ->
          let r = W.add w a b in
          let so = W.neg_bit w a = W.neg_bit w b && W.neg_bit w r <> W.neg_bit w a in
          poison_if ((nsw && so) || (nuw && W.ult r a)) (Bits r)
      | Sub ->
          let r = W.sub w a b in
          let so = W.neg_bit w a <> W.neg_bit w b && W.neg_bit w r <> W.neg_bit w a in
          poison_if ((nsw && so) || (nuw && W.ult a b)) (Bits r)
      | Mul ->
          let r = W.mul w a b in
          let ww = 2 * w in
          let sfull = W.mul ww (W.sext w ww a) (W.sext w ww b) in
          let ufull = W.mul ww (W.zext ww a) (W.zext ww b) in
          let so = not (W.equal sfull (W.sext w ww r))
          and uo = not (W.equal ufull (W.zext ww r)) in
          poison_if ((nsw && so) || (nuw && uo)) (Bits r)
      | Udiv | Urem | Sdiv | Srem ->
          let trap = has flags f_trap in
          if W.is_zero b then
            if trap then raise (Trapped_exn Trap_div) else error "divides by zero";
          let signed = k = Sdiv || k = Srem in
          if signed && W.equal a (W.min_signed w) && W.equal b (W.all_ones w) then
            if trap then raise (Trapped_exn Trap_div)
            else error "divides the minimum value by -1";
          let q, r = if signed then W.sdivmod w a b else W.udivmod w a b in
          if k = Udiv || k = Sdiv then
            poison_if (has flags f_exact && not (W.is_zero r)) (Bits q)
          else Bits r
      | Shl -> (
          match sshift () with
          | None -> Poison
          | Some s ->
              let r = W.shl w a s in
              poison_if
                ((nsw && not (W.equal (W.ashr w r s) a))
                || (nuw && not (W.equal (W.lshr w r s) a)))
                (Bits r))
      | Lshr | Ashr -> (
          match sshift () with
          | None -> Poison
          | Some s ->
              let r = if k = Lshr then W.lshr w a s else W.ashr w a s in
              poison_if (has flags f_exact && W.any_below a s) (Bits r))
      | And -> Bits (W.logand a b)
      | Or ->
          poison_if
            (has flags f_disjoint && not (W.is_zero (W.logand a b)))
            (Bits (W.logor a b))
      | Xor -> Bits (W.logxor a b)
      | Smin -> Bits (if W.slt w a b then a else b)
      | Smax -> Bits (if W.slt w a b then b else a)
      | Umin -> Bits (if W.ult a b then a else b)
      | Umax -> Bits (if W.ult a b then b else a)
      | Smulh | Umulh ->
          let ww = 2 * w in
          let full =
            if k = Smulh then W.mul ww (W.sext w ww a) (W.sext w ww b)
            else W.mul ww (W.zext ww a) (W.zext ww b)
          in
          Bits (W.trunc w (W.lshr ww full w))
      | Sadd_sat ->
          let r = W.add w a b in
          if W.neg_bit w a = W.neg_bit w b && W.neg_bit w r <> W.neg_bit w a then
            Bits (if W.neg_bit w a then W.min_signed w else W.max_signed w)
          else Bits r
      | Ssub_sat ->
          let r = W.sub w a b in
          if W.neg_bit w a <> W.neg_bit w b && W.neg_bit w r <> W.neg_bit w a then
            Bits (if W.neg_bit w a then W.min_signed w else W.max_signed w)
          else Bits r
      | Uadd_sat ->
          let r = W.add w a b in
          Bits (if W.ult r a then W.all_ones w else r)
      | Usub_sat -> Bits (if W.ult a b then W.zero w else W.sub w a b))

let iun_lane k flags w a =
  match a with
  | Poison -> Poison
  | Bits a -> (
      match k with
      | Ctz ->
          poison_if
            (has flags f_zero_poison && W.is_zero a)
            (Bits (W.of_int64 w (Int64.of_int (W.ctz w a))))
      | Clz ->
          poison_if
            (has flags f_zero_poison && W.is_zero a)
            (Bits (W.of_int64 w (Int64.of_int (W.clz w a))))
      | Popcnt -> Bits (W.of_int64 w (Int64.of_int (W.popcount a)))
      | Bswap -> Bits (W.bswap w a)
      | Bitrev -> Bits (W.bitrev w a)
      | Abs ->
          if W.equal a (W.min_signed w) && w > 1 then
            poison_if (has flags f_poison_min) (Bits a)
          else Bits (W.abs w a))

let fbin_lane ty k a b =
  match (a, b) with
  | Poison, _ | _, Poison -> Poison
  | Bits a, Bits b -> (
      match k with
      | Copysign ->
          let s = sign_bit_of ty in
          Bits (flip_bit a s (if W.bit b s then `Set else `Clear))
      | _ ->
          let x = to_float ty a and y = to_float ty b in
          let r =
            match k with
            | Fadd -> x +. y
            | Fsub -> x -. y
            | Fmul -> x *. y
            | Fdiv -> x /. y
            | Frem -> Float.rem x y
            | Fmin -> fmin_num x y
            | Fmax -> fmax_num x y
            | Copysign -> assert false
          in
          Bits (of_float ty r))

let fun_lane ty k a =
  match a with
  | Poison -> Poison
  | Bits a -> (
      let s = sign_bit_of ty in
      match k with
      | Fneg -> Bits (flip_bit a s `Flip)
      | Fabs -> Bits (flip_bit a s `Clear)
      | _ ->
          let x = to_float ty a in
          let r =
            match k with
            | Sqrt -> Float.sqrt x
            | Floor -> Float.floor x
            | Ceil -> Float.ceil x
            | Ftrunc -> Float.trunc x
            | Roundeven -> roundeven x
            | Fneg | Fabs -> assert false
          in
          Bits (of_float ty r))

let fcmp_lane ty k a b =
  match (a, b) with
  | Poison, _ | _, Poison -> Poison
  | Bits a, Bits b ->
      let x = to_float ty a and y = to_float ty b in
      let uno = Float.is_nan x || Float.is_nan y in
      let r =
        match k with
        | Ffalse -> false
        | Ftrue -> true
        | Foeq -> (not uno) && x = y
        | Fogt -> (not uno) && x > y
        | Foge -> (not uno) && x >= y
        | Folt -> (not uno) && x < y
        | Fole -> (not uno) && x <= y
        | Fone -> (not uno) && x <> y
        | Ford -> not uno
        | Fueq -> uno || x = y
        | Fugt -> uno || x > y
        | Fuge -> uno || x >= y
        | Fult -> uno || x < y
        | Fule -> uno || x <= y
        | Fune -> uno || x <> y
        | Funo -> uno
      in
      bool_lane r

let icmp_lane w k a b =
  match (a, b) with
  | Poison, _ | _, Poison -> Poison
  | Bits a, Bits b ->
      let r =
        match k with
        | Eq -> W.equal a b
        | Ne -> not (W.equal a b)
        | Slt -> W.slt w a b
        | Sle -> not (W.slt w b a)
        | Sgt -> W.slt w b a
        | Sge -> not (W.slt w a b)
        | Ult -> W.ult a b
        | Ule -> not (W.ult b a)
        | Ugt -> W.ult b a
        | Uge -> not (W.ult a b)
      in
      bool_lane r

(* bitcast: lanes concatenated from bit 0 (lane 0 lowest) *)
let bitcast st ty sty (x : v) : v =
  ignore st;
  let sw = lane_width (elem sty) and dw = lane_width (elem ty) in
  let total = sw * lanes sty in
  let acc = ref (W.zero total) in
  let pois = Array.make total false in
  Array.iteri
    (fun i l ->
      match l with
      | Poison -> Array.fill pois (i * sw) sw true
      | Bits b ->
          let piece =
            W.shl total
              (W.zext total (if Array.length b > limbs_for sw then W.trunc sw b else b))
              (i * sw)
          in
          acc := W.logor !acc piece)
    x;
  Array.init (lanes ty) (fun i ->
      if Array.exists Fun.id (Array.sub pois (i * dw) dw) then Poison
      else
        let r = W.trunc dw (W.lshr total !acc (i * dw)) in
        Bits
          (if elem ty = F80 then
             Array.init 2 (fun k -> if k < Array.length r then r.(k) else 0L)
           else r))

let conv_lane k sty ty a =
  let se = elem sty and de = elem ty in
  match a with
  | Poison -> Poison
  | Bits a -> (
      match k with
      | Trunc -> Bits (W.trunc (lane_width de) a)
      | Zext -> Bits (W.zext (lane_width de) a)
      | Sext -> Bits (W.sext (lane_width se) (lane_width de) a)
      | Fptrunc | Fpext -> Bits (of_float de (to_float se a))
      | Fptosi | Fptoui -> (
          match
            int_of_float_opt ~signed:(k = Fptosi) (lane_width de) (to_float se a)
          with
          | Some l -> Bits l
          | None -> Poison)
      | Fptosi_sat | Fptoui_sat ->
          Bits
            (int_of_float_sat ~signed:(k = Fptosi_sat) (lane_width de) (to_float se a))
      | Sitofp | Uitofp ->
          Bits (float_of_int_limbs de ~signed:(k = Sitofp) (lane_width se) a)
      | Ptrtoint -> Bits (W.zext (lane_width de) (W.trunc (lane_width de) a))
      | Inttoptr -> Bits (W.zext 64 (W.trunc 64 a))
      | Bitcast -> assert false)

let int_lane_of_int w n = Bits (W.of_int64 w (Int64.of_int n))
let lane_index = function Poison -> None | Bits b -> W.to_int_opt b

let rmw_value op ty old v =
  match (old, v) with
  | Poison, _ | _, Poison -> Poison
  | Bits o, Bits x -> (
      let w = lane_width ty in
      match op with
      | Xchg -> Bits x
      | Radd -> Bits (W.add w o x)
      | Rsub -> Bits (W.sub w o x)
      | Rand -> Bits (W.logand o x)
      | Ror -> Bits (W.logor o x)
      | Rxor -> Bits (W.logxor o x)
      | Rmin -> Bits (if W.slt w x o then x else o)
      | Rmax -> Bits (if W.slt w o x then x else o)
      | Rumin -> Bits (if W.ult x o then x else o)
      | Rumax -> Bits (if W.ult o x then x else o)
      | Rfadd -> Bits (of_float ty (to_float ty o +. to_float ty x))
      | Rfsub -> Bits (of_float ty (to_float ty o -. to_float ty x)))

(* ---------- varargs (SysV, everything in the overflow area) ---------- *)

let pow2_at_least n =
  let r = ref 1 in
  while !r < n do
    r := 2 * !r
  done;
  !r

(* (slot size, alignment) of a variadic argument *)
let vararg_slot = function
  | `Ty (Int n) when n > 64 && n <= 128 -> (16, 16)
  | `Ty (Int n) when n > 128 -> ((size_of (Int n) + 7) / 8 * 8, 8)
  | `Ty F80 -> (16, 16)
  | `Ty (Vec _ as t) ->
      let c = pow2_at_least (size_of t) in
      ((c + 7) / 8 * 8, if c > 8 then max 16 c else 8)
  | `Ty _ -> (8, 8)
  | `Agg (size, align) -> ((size + 7) / 8 * 8, if align > 8 then max 16 align else 8)

(* ---------- calls ---------- *)

let abi_ty = function Scalar t | Ext (t, _) -> t | Agg _ -> ptr

let sig_matches (a : signature) (b : signature) =
  let same x y =
    abi_ty x = abi_ty y
    && match (x, y) with Agg p, Agg q -> p.size = q.size | _ -> true
  in
  List.length a.param_abis = List.length b.param_abis
  && List.for_all2 same a.param_abis b.param_abis
  && List.length a.result_abis = List.length b.result_abis
  && List.for_all2 same a.result_abis b.result_abis
  && a.variadic = b.variadic

let tick st =
  st.steps <- st.steps + 1;
  if st.steps > st.cfg.step_limit then
    error "runs for more than %d steps" st.cfg.step_limit

let new_alloca st fr ~size ~align ~what =
  let o = new_obj st ~size ~align ~writable:true ~what ~var_global:false in
  fr.allocas <- o :: fr.allocas;
  fr.nallocas <- fr.nallocas + 1;
  o

(* copies [size] bytes from [src] into a new object owned by [owner] *)
let copy_bytes st owner src size ~writable ~what =
  let raw = read_raw st src size in
  let o = new_obj st ~size ~align:16 ~writable:true ~what ~var_global:false in
  write_raw st o.base raw;
  let o = if writable then o else { o with writable = false } in
  if not writable then st.objs <- Amap.add o.base o st.objs;
  owner.allocas <- o :: owner.allocas;
  owner.nallocas <- owner.nallocas + 1;
  o

let rec call st (caller : frame option) (target : sym) (sig_ : signature)
    (args : v array) va_aggs : v array =
  match target with
  | Fn fi ->
      let f = fi.fn in
      if not (sig_matches sig_ f.sig_) then
        error "calls @%s with a signature other than the one it is defined with" f.name;
      if st.depth >= st.cfg.max_depth then
        error "nests calls more than %d deep" st.cfg.max_depth;
      st.depth <- st.depth + 1;
      let fixed = List.length f.sig_.param_abis in
      let fr0 =
        {
          fi;
          vals = Array.make (num_values f) [||];
          allocas = [];
          nallocas = 0;
          marks = Hashtbl.create 1;
          varargs = 0L;
        }
      in
      (* the variadic arguments, laid out as SysV's overflow area *)
      let varargs =
        if Array.length args <= fixed then 0L
        else begin
          let items =
            Array.to_list
              (Array.mapi
                 (fun k x ->
                   if k < fixed then None
                   else
                     match List.assoc_opt k va_aggs with
                     | Some (Agg { size; align; _ }) -> Some (`Agg (size, align), x)
                     | _ -> Some (`Ty (arg_ty caller sig_ k x), x))
                 args)
            |> List.filter_map Fun.id
          in
          let size =
            List.fold_left
              (fun pos (s, _) ->
                let n, a = vararg_slot s in
                ((pos + a - 1) / a * a) + n)
              0 items
          in
          let o = new_alloca st fr0 ~size ~align:16 ~what:"variadic arguments" in
          ignore
            (List.fold_left
               (fun pos (s, x) ->
                 let n, a = vararg_slot s in
                 let pos = (pos + a - 1) / a * a in
                 let at = Int64.add o.base (Int64.of_int pos) in
                 (match s with
                 | `Agg (size, _) -> write_raw st at (read_raw st (ptr_of x.(0)) size)
                 | `Ty t -> write_value st t at x);
                 pos + n)
               0 items);
          o.base
        end
      in
      let fr = { fr0 with varargs } in
      let params = (block f entry).params in
      List.iteri
        (fun k abi ->
          let x = args.(k) in
          let x =
            match abi with
            | Agg { size; _ } ->
                (* the callee reads its own copy and must not write it *)
                let o =
                  copy_bytes st fr
                    (ptr_of x.(0))
                    size ~writable:false ~what:"an aggregate parameter"
                in
                [| Bits [| o.base |] |]
            | _ -> x
          in
          fr.vals.(params.(k)) <- x)
        f.sig_.param_abis;
      let results = run_block st caller fr entry in
      List.iter (free_obj st) fr.allocas;
      st.depth <- st.depth - 1;
      results
  | Ext_fn (name, _) -> st.cfg.extern_call st name sig_ args
  | Ext_gl (name, _) -> error "calls the address of the global @%s" name
  | Obj o | Part (o, _) -> error "calls the address of %s" o.what
  | Addr a -> error "calls address 0x%Lx, which holds no function" a

and arg_ty caller sig_ k x =
  ignore sig_;
  match caller with
  | Some _ -> (
      match x with
      | _ -> (
          (* the type travels with the call site: see [exec] *)
          match !current_arg_tys with
          | Some tys when k < Array.length tys -> tys.(k)
          | _ -> error "a variadic argument with no type"))
  | None -> error "a variadic argument with no type"

and current_arg_tys : ty array option ref = ref None

and run_block st caller fr b : v array =
  let f = fr.fi.fn in
  let br = block f b in
  Vec.iter
    (fun i ->
      tick st;
      exec st fr i)
    br.insts;
  tick st;
  let goto (t : target) =
    let args = Array.map (get st fr) t.args in
    let ps = (block f t.dest).params in
    Array.iteri (fun k p -> fr.vals.(p) <- args.(k)) ps;
    run_block st caller fr t.dest
  in
  let branch_value v =
    match get1 st fr v with Poison -> error "branches on poison" | Bits b -> b
  in
  match br.term with
  | Jump t -> goto t
  | Br (c, t, e, _) -> if (branch_value c).(0) <> 0L then goto t else goto e
  | Switch (v, d, cases, _) ->
      let x = branch_value v in
      let w = int_width (ty_of f v) in
      let rec find k =
        if k >= Array.length cases then goto d
        else
          let key, t = cases.(k) in
          if W.equal (normalize_limbs w key) x then goto t else find (k + 1)
      in
      find 0
  | Indirect_br (v, targets) -> (
      let a = (branch_value v).(0) in
      match Hashtbl.find_opt st.blockaddrs a with
      | Some (fname, blk) when fname = f.name && Array.mem blk targets ->
          run_block st caller fr blk
      | _ -> error "indirect_br to 0x%Lx, which is not one of its listed blocks" a)
  | Asm_goto _ ->
      error "runs inline assembly (asm goto), which the interpreter cannot do"
  | Unreachable -> error "reaches `unreachable` in @%s" f.name
  | Ret vs ->
      let rs = Array.map (get st fr) vs in
      (* an aggregate result is copied out before this frame goes away *)
      List.iteri
        (fun k abi ->
          match (abi, caller) with
          | Agg { size; _ }, Some owner ->
              let o =
                copy_bytes st owner
                  (ptr_of rs.(k).(0))
                  size ~writable:true ~what:"an aggregate result"
              in
              rs.(k) <- [| Bits [| o.base |] |]
          | Agg { size; _ }, None ->
              (* no caller frame (the entry call): keep the bytes in a
                 standalone object *)
              let raw = read_raw st (ptr_of rs.(k).(0)) size in
              let o =
                new_obj st ~size ~align:16 ~writable:true ~what:"an aggregate result"
                  ~var_global:false
              in
              write_raw st o.base raw;
              rs.(k) <- [| Bits [| o.base |] |]
          | _ -> ())
        f.sig_.result_abis;
      rs

and exec st fr i =
  let f = fr.fi.fn in
  let ir = inst f i in
  let rty k = ty_of f ir.results.(k) in
  let set k x = fr.vals.(ir.results.(k)) <- x in
  let g v = get st fr v in
  let n () = lanes (rty 0) in
  let vty v = ty_of f v in
  match ir.op with
  | Nop | Fence _ | Lifetime _ | Dbg_value _ -> ()
  | Ibin (k, flags, a, b) ->
      let w = int_width (rty 0) in
      set 0 (map2 (n ()) (ibin_lane k flags w) (g a) (g b))
  | Iun (k, flags, a) ->
      let w = int_width (rty 0) in
      set 0 (Array.map (iun_lane k flags w) (g a))
  | Fbin (k, _, a, b) -> set 0 (map2 (n ()) (fbin_lane (elem (rty 0)) k) (g a) (g b))
  | Fun (k, _, a) -> set 0 (Array.map (fun_lane (elem (rty 0)) k) (g a))
  | Fma (_, a, b, c) ->
      let t = elem (rty 0) in
      let x = g a and y = g b and z = g c in
      set 0
        (Array.init (n ()) (fun i ->
             match (lane x i, lane y i, lane z i) with
             | Bits a, Bits b, Bits c ->
                 let a = to_float t a and b = to_float t b and c = to_float t c in
                 Bits
                   (if t = F64 then of_float t (Float.fma a b c) else fma_narrow t a b c)
             | _ -> Poison))
  | Fsh (left, a, b, amt) ->
      let w = int_width (rty 0) in
      let x = g a and y = g b and z = g amt in
      set 0
        (Array.init (n ()) (fun i ->
             match (lane x i, lane y i, lane z i) with
             | Bits a, Bits b, Bits s ->
                 let s =
                   Int64.to_int
                     (Int64.unsigned_rem
                        (W.udivmod w s (W.of_int64 w (Int64.of_int w)) |> snd).(0)
                        (Int64.of_int w))
                 in
                 if s = 0 then Bits (if left then a else b)
                 else if left then Bits (W.logor (W.shl w a s) (W.lshr w b (w - s)))
                 else Bits (W.logor (W.lshr w b s) (W.shl w a (w - s)))
             | _ -> Poison))
  | Ovf (k, a, b) ->
      let w = int_width (rty 0) in
      let x = g a and y = g b in
      let r = Array.make (n ()) Poison and o = Array.make (n ()) Poison in
      for i = 0 to n () - 1 do
        match (lane x i, lane y i) with
        | Bits a, Bits b ->
            let s, ov =
              match k with
              | Sadd_ovf ->
                  let s = W.add w a b in
                  (s, W.neg_bit w a = W.neg_bit w b && W.neg_bit w s <> W.neg_bit w a)
              | Uadd_ovf ->
                  let s = W.add w a b in
                  (s, W.ult s a)
              | Ssub_ovf ->
                  let s = W.sub w a b in
                  (s, W.neg_bit w a <> W.neg_bit w b && W.neg_bit w s <> W.neg_bit w a)
              | Usub_ovf -> (W.sub w a b, W.ult a b)
              | Smul_ovf ->
                  let ww = 2 * w in
                  let full = W.mul ww (W.sext w ww a) (W.sext w ww b) in
                  let s = W.trunc w full in
                  (s, not (W.equal full (W.sext w ww s)))
              | Umul_ovf ->
                  let ww = 2 * w in
                  let full = W.mul ww (W.zext ww a) (W.zext ww b) in
                  let s = W.trunc w full in
                  (s, not (W.equal full (W.zext ww s)))
            in
            r.(i) <- Bits s;
            o.(i) <- bool_lane ov
        | _ -> ()
      done;
      set 0 r;
      set 1 o
  | Icmp (k, a, b) ->
      let w = match elem (vty a) with Ptr _ -> 64 | t -> lane_width t in
      set 0 (map2 (n ()) (icmp_lane w k) (g a) (g b))
  | Fcmp (k, _, a, b) -> set 0 (map2 (n ()) (fcmp_lane (elem (vty a)) k) (g a) (g b))
  | Select (c, a, b) ->
      let cv = g c and x = g a and y = g b in
      set 0
        (Array.init (n ()) (fun i ->
             match lane cv i with
             | Poison -> Poison
             | Bits c -> if c.(0) <> 0L then lane x i else lane y i))
  (* a constant-time mark changes nothing, poison included *)
  | Mark (_, a) -> set 0 (g a)
  | Freeze a ->
      let t = elem (rty 0) in
      set 0
        (Array.map
           (function
             | Poison ->
                 Bits
                   (Array.make
                      (match t with F80 -> 2 | _ -> limbs_for (lane_width t))
                      0L)
             | l -> l)
           (g a))
  | Conv (Bitcast, a) -> set 0 (bitcast st (rty 0) (vty a) (g a))
  | Conv (k, a) -> set 0 (Array.map (conv_lane k (vty a) (rty 0)) (g a))
  | Alloca (size, align) -> (
      match get1 st fr size with
      | Poison -> error "allocates a poison number of bytes"
      | Bits s -> (
          match W.to_int_opt s with
          | Some size when size <= 1 lsl 30 ->
              let o = new_alloca st fr ~size ~align ~what:"a stack slot" in
              set 0 [| Bits [| o.base |] |]
          | _ -> error "allocates more stack than the interpreter allows (1 GiB)"))
  | Load (m, p) ->
      let a = ptr_of (get1 st fr p) in
      check_align a m.align;
      set 0 (read_value st (rty 0) a)
  | Store (m, v, p) ->
      let a = ptr_of (get1 st fr p) in
      check_align a m.align;
      write_value st (vty v) a (g v)
  | Ptradd (flags, p, idx, scale, disp) ->
      let pv = g p and iv = g idx in
      let iw = int_width (vty idx) in
      set 0
        (Array.init (n ()) (fun i ->
             match (lane pv i, lane iv i) with
             | Bits p, Bits x ->
                 let ww = max iw 64 + 130 in
                 let exact =
                   W.add ww
                     (W.add ww (W.zext ww p)
                        (W.mul ww (W.sext iw ww x) (W.of_int64 ww (Int64.of_int scale))))
                     (W.of_int64 ww (Int64.of_int disp))
                 in
                 let r = W.trunc 64 exact in
                 if has flags f_inbounds && not (W.equal exact (W.zext ww r)) then
                   Poison
                 else Bits r
             | _ -> Poison))
  | Memcpy (_, d, s, len) | Memmove (_, d, s, len) -> (
      match get1 st fr len with
      | Poison -> error "copies a poison number of bytes"
      | Bits l -> (
          match W.to_int_opt l with
          | Some 0 -> ()
          | Some len ->
              let da = ptr_of (get1 st fr d) and sa = ptr_of (get1 st fr s) in
              (match ir.op with
              | Memcpy _ ->
                  let lo = Int64.sub da sa in
                  if
                    Int64.compare (Int64.abs lo) (Int64.of_int len) < 0
                    && lo <> Int64.min_int
                  then error "memcpy between overlapping regions"
              | _ -> ());
              write_raw st da (read_raw st sa len)
          | None -> error "copies more bytes than any object holds"))
  | Memset (_, d, byte, len) -> (
      match (get1 st fr len, get1 st fr byte) with
      | Poison, _ -> error "fills a poison number of bytes"
      | Bits l, b -> (
          match W.to_int_opt l with
          | Some 0 -> ()
          | Some len ->
              let x =
                match b with Poison -> None | Bits b -> Some (Int64.to_int b.(0))
              in
              write_raw st (ptr_of (get1 st fr d)) (Array.make len x)
          | None -> error "fills more bytes than any object holds"))
  | Rmw (op, m, p, v) ->
      let a = ptr_of (get1 st fr p) in
      check_align a m.align;
      let t = vty v in
      let old = read_value st t a in
      (* lane-wise on a vector *)
      write_value st t a (Array.map2 (rmw_value op (elem t)) old (g v));
      set 0 old
  | Cmpxchg { mem; p; expected; desired; _ } ->
      let a = ptr_of (get1 st fr p) in
      check_align a mem.align;
      let t = vty desired in
      let old = read_value st t a in
      (* the whole value, bit for bit (every lane of a vector) *)
      let ok =
        let x = g expected in
        let acc = ref (Some true) in
        Array.iteri
          (fun k o ->
            acc :=
              match (!acc, o, x.(k)) with
              | Some b, Bits o, Bits x -> Some (b && W.equal o x)
              | _ -> None)
          old;
        !acc
      in
      (match ok with Some true -> write_value st t a (g desired) | _ -> ());
      set 0 old;
      set 1 [| (match ok with Some b -> bool_lane b | None -> Poison) |]
  | Cmpxchg2 { mem; p; elo; ehi; dlo; dhi } ->
      let a = ptr_of (get1 st fr p) in
      check_align a mem.align;
      let ah = Int64.add a 8L and t = Int 64 in
      let ol = read_value st t a and oh = read_value st t ah in
      let eq o x =
        match (o.(0), x.(0)) with Bits o, Bits x -> Some (W.equal o x) | _ -> None
      in
      let ok =
        match (eq ol (g elo), eq oh (g ehi)) with
        | Some a, Some b -> Some (a && b)
        | _ -> None
      in
      if ok = Some true then begin
        write_value st t a (g dlo);
        write_value st t ah (g dhi)
      end;
      set 0 ol;
      set 1 oh;
      set 2 [| (match ok with Some b -> bool_lane b | None -> Poison) |]
  | Extract (v, idx) ->
      let x = g v in
      set 0
        [|
          (match lane_index (get1 st fr idx) with
          | Some k when k < Array.length x -> x.(k)
          | _ -> Poison);
        |]
  | Insert (v, x, idx) ->
      let vv = Array.copy (g v) in
      (match lane_index (get1 st fr idx) with
      | Some k when k < Array.length vv -> vv.(k) <- get1 st fr x
      | _ -> Array.fill vv 0 (Array.length vv) Poison);
      set 0 vv
  | Shuffle (a, b, mask) ->
      let x = g a and y = g b in
      let na = Array.length x in
      set 0
        (Array.map
           (fun k -> if k < 0 then Poison else if k < na then x.(k) else y.(k - na))
           mask)
  | Splat a -> set 0 (Array.make (n ()) (get1 st fr a))
  | Reduce (op, v) ->
      let x = g v in
      let t = elem (vty v) in
      let r =
        if Array.exists (( = ) Poison) x then Poison
        else
          let bits = Array.map (function Bits b -> b | Poison -> assert false) x in
          let w = lane_width t in
          let fold f =
            Bits (Array.fold_left f bits.(0) (Array.sub bits 1 (Array.length bits - 1)))
          in
          match op with
          | Red_add -> fold (W.add w)
          | Red_mul -> fold (W.mul w)
          | Red_and -> fold W.logand
          | Red_or -> fold W.logor
          | Red_xor -> fold W.logxor
          | Red_smin -> fold (fun a b -> if W.slt w b a then b else a)
          | Red_smax -> fold (fun a b -> if W.slt w a b then b else a)
          | Red_umin -> fold (fun a b -> if W.ult b a then b else a)
          | Red_umax -> fold (fun a b -> if W.ult a b then b else a)
          | Red_fmin ->
              fold (fun a b -> of_float t (fmin_num (to_float t a) (to_float t b)))
          | Red_fmax ->
              fold (fun a b -> of_float t (fmax_num (to_float t a) (to_float t b)))
          | Red_fadd_ordered ->
              fold (fun a b -> of_float t (to_float t a +. to_float t b))
      in
      set 0 [| r |]
  | Masked_load (align, p, mask, pass) ->
      let base = ptr_of (get1 st fr p) in
      let t = elem (rty 0) in
      let m = g mask and pv = g pass in
      let sel i =
        match m.(i) with
        | Poison -> error "masked_load with a poison mask lane"
        | Bits b -> b.(0) <> 0L
      in
      if t = i1 then
        (* bit packed: lane i is bit i mod 8 of byte i / 8, and only the
           bytes holding a selected lane are read *)
        set 0
          (Array.init (n ()) (fun i ->
               if not (sel i) then lane pv i
               else
                 match
                   (read_value st i8 (Int64.add base (Int64.of_int (i / 8)))).(0)
                 with
                 | Poison -> Poison
                 | Bits b ->
                     Bits
                       [| Int64.logand (Int64.shift_right_logical b.(0) (i mod 8)) 1L |]))
      else
        set 0
          (Array.init (n ()) (fun i ->
               match m.(i) with
               | Poison -> error "masked_load with a poison mask lane"
               | Bits b when b.(0) <> 0L ->
                   let a = Int64.add base (Int64.of_int (i * size_of t)) in
                   if i = 0 then check_align a align;
                   (read_value st t a).(0)
               | Bits _ -> lane pv i))
  | Masked_store (_, v, p, mask) ->
      let base = ptr_of (get1 st fr p) in
      let t = elem (vty v) in
      let x = g v and m = g mask in
      if t = i1 then begin
        (* a read-modify-write of each byte holding a selected lane *)
        let sel i =
          match m.(i) with
          | Poison -> error "masked_store with a poison mask lane"
          | Bits b -> b.(0) <> 0L
        in
        for j = 0 to ((Array.length m + 7) / 8) - 1 do
          let lanes =
            List.init (min 8 (Array.length m - (8 * j))) (fun k -> (8 * j) + k)
          in
          if List.exists sel lanes then begin
            let a = Int64.add base (Int64.of_int j) in
            let nw =
              List.fold_left
                (fun acc i ->
                  match (acc, x.(i)) with
                  | Poison, _ -> Poison
                  | _, _ when not (sel i) -> acc
                  | _, Poison -> Poison
                  | Bits o, Bits b ->
                      let bit = Int64.shift_left 1L (i mod 8) in
                      let o = Int64.logand o.(0) (Int64.lognot bit) in
                      Bits
                        [|
                          (if Int64.logand b.(0) 1L <> 0L then Int64.logor o bit else o);
                        |])
                (read_value st i8 a).(0)
                lanes
            in
            write_value st i8 a [| nw |]
          end
        done
      end
      else
        Array.iteri
          (fun i ml ->
            match ml with
            | Poison -> error "masked_store with a poison mask lane"
            | Bits b when b.(0) <> 0L ->
                write_value st t
                  (Int64.add base (Int64.of_int (i * size_of t)))
                  [| x.(i) |]
            | Bits _ -> ())
          m
  | Permute (v, idx) ->
      (* lane k is V[IDX[k]]; an index past the last lane gives zero, a
         poison index a poison lane *)
      let x = g v in
      let t = elem (rty 0) in
      let zero =
        Bits (Array.make (match t with F80 -> 2 | _ -> limbs_for (lane_width t)) 0L)
      in
      set 0
        (Array.map
           (function
             | Poison -> Poison
             | Bits b -> (
                 match W.to_int_opt b with
                 | Some k when k < Array.length x -> x.(k)
                 | _ -> zero))
           (g idx))
  | Compress (v, mask) | Expand (v, mask) ->
      let x = g v and m = g mask in
      let t = elem (rty 0) in
      let zero =
        Bits (Array.make (match t with F80 -> 2 | _ -> limbs_for (lane_width t)) 0L)
      in
      let sel i =
        match m.(i) with Poison -> error "a poison mask lane" | Bits b -> b.(0) <> 0L
      in
      let out = Array.make (n ()) zero in
      let k = ref 0 in
      (match ir.op with
      | Compress _ ->
          Array.iteri
            (fun i l ->
              if sel i then begin
                out.(!k) <- l;
                incr k
              end)
            x
      | _ ->
          Array.iteri
            (fun i _ ->
              if sel i then begin
                out.(i) <- x.(!k);
                incr k
              end)
            x);
      set 0 out
  | Call { sig_; callee; args; attrs; va_aggs } ->
      let target =
        match callee with
        | Direct name -> lookup st fr.fi.mi name
        | Indirect v -> (
            let a = ptr_of (get1 st fr v) in
            match Hashtbl.find_opt st.by_addr a with Some s -> s | None -> Addr a)
      in
      let argv = Array.map g args in
      current_arg_tys := Some (Array.map vty args);
      let rs = call st (Some fr) target sig_ argv va_aggs in
      if has attrs a_noreturn then error "returns from a call marked noreturn";
      Array.iteri (fun k x -> set k x) rs
  | Syscall args ->
      let a =
        Array.map
          (fun v ->
            match get1 st fr v with
            | Poison -> error "passes poison to a system call"
            | Bits b -> b.(0))
          args
      in
      set 0 [| Bits [| st.cfg.syscall st a |] |]
  | Va_start p ->
      if fr.varargs = 0L && not fr.fi.fn.sig_.variadic then
        error "va_start in a function that is not variadic";
      let a = ptr_of (get1 st fr p) in
      let w32 x = [| Bits [| Int64.of_int x |] |] in
      write_value st i32 a (w32 48);
      write_value st i32 (Int64.add a 4L) (w32 176);
      write_value st ptr (Int64.add a 8L) [| Bits [| fr.varargs |] |];
      write_value st ptr (Int64.add a 16L) [| Bits [| 0L |] |]
  | Va_arg p ->
      let t = rty 0 in
      let at = va_next st (ptr_of (get1 st fr p)) (`Ty t) in
      set 0 (read_value st t at)
  | Va_arg_agg (abi, p) -> (
      match abi with
      | Agg { size; align; _ } ->
          let at = va_next st (ptr_of (get1 st fr p)) (`Agg (size, align)) in
          let o = copy_bytes st fr at size ~writable:true ~what:"a va_arg aggregate" in
          set 0 [| Bits [| o.base |] |]
      | _ -> error "va_arg agg without an aggregate ABI")
  | Stack_save ->
      st.token <- Int64.add st.token 16L;
      Hashtbl.replace fr.marks st.token fr.nallocas;
      set 0 [| Bits [| st.token |] |]
  | Stack_restore s -> (
      let tok = ptr_of (get1 st fr s) in
      match Hashtbl.find_opt fr.marks tok with
      | None ->
          error "stackrestore with a value that no stacksave of this function made"
      | Some k ->
          while fr.nallocas > k do
            match fr.allocas with
            | o :: rest ->
                free_obj st o;
                fr.allocas <- rest;
                fr.nallocas <- fr.nallocas - 1
            | [] -> fr.nallocas <- k
          done)
  | Asm _ -> error "runs inline assembly, which the interpreter cannot do"
  | Trap -> raise (Trapped_exn Trap_insn)

(* the address of the next variadic argument; advances the va_list *)
and va_next st va slot =
  let gp = (read_value st i32 va).(0)
  and fp = (read_value st i32 (Int64.add va 4L)).(0) in
  (match (gp, fp) with
  | Bits g, Bits f when Int64.compare g.(0) 48L >= 0 && Int64.compare f.(0) 176L >= 0 ->
      ()
  | _ ->
      error
        "va_arg through a va_list whose register save area is in use (not supported by \
         the interpreter)");
  let area = ptr_of (read_value st ptr (Int64.add va 8L)).(0) in
  let n, a = vararg_slot slot in
  let at = align_up area a in
  write_value st ptr (Int64.add va 8L) [| Bits [| Int64.add at (Int64.of_int n) |] |];
  at

(* ---------- entry points ---------- *)

type outcome = Returned of v array | Trapped of trap_kind | Exited of int

let run_call st (target : sym) (sig_ : signature) (args : v array) =
  try Returned (call st None target sig_ args []) with
  | Trapped_exn k -> Trapped k
  | Exit_exn c -> Exited c

(* Calls the function [name] of module [mi] *)
let run_function st ?(mi = 0) name (args : v array) =
  match lookup st mi name with
  | Fn fi as s -> run_call st s fi.fn.sig_ args
  | _ -> error "@%s has no body" name

let int_arg w x = [| Bits (W.of_int64 w x) |]

(* ---------- whole programs (the test oracle) ---------- *)

type program_result = {
  status : int;
  stdout : string;
  stderr : string;
  trapped : trap_kind option;
}

(* Runs a program as fas-fir links it (SPEC §10): exit_group(main(argc,
   argv)). write(1|2) and exit/exit_group are the only syscalls; calls to
   functions without a body are errors. *)
let run_program ?(step_limit = 1_000_000_000) ?(argv = [ "prog" ]) (ms : modul list) =
  let out = Buffer.create 256 and err = Buffer.create 64 in
  let syscall st (a : int64 array) =
    let arg k = if k < Array.length a then a.(k) else 0L in
    match a.(0) with
    | 1L ->
        let len = Int64.to_int (arg 3) in
        let s = read_string st (arg 2) len in
        (match arg 1 with
        | 1L -> Buffer.add_string out s
        | 2L -> Buffer.add_string err s
        | fd ->
            error "writes to file descriptor %Ld, which the interpreter does not have"
              fd);
        Int64.of_int len
    | 60L | 231L -> raise (Exit_exn (Int64.to_int (Int64.logand (arg 1) 0xffL)))
    | nr -> error "makes system call %Ld, which the interpreter does not support" nr
  in
  let cfg = { const_config with step_limit; allow_var_globals = true; syscall } in
  let st = create cfg ms in
  let main = lookup st 0 "main" in
  let main_fn = match main with Fn fi -> fi.fn | _ -> error "no @main is defined" in
  let args =
    match main_fn.sig_.param_abis with
    | [] -> [||]
    | [ _; _ ] ->
        (* argv: pointers to NUL-terminated strings, then a null *)
        let ptrs =
          List.map
            (fun s ->
              let o =
                new_obj st
                  ~size:(String.length s + 1)
                  ~align:1 ~writable:true ~what:"argv" ~var_global:false
              in
              Bytes.blit_string s 0 o.data 0 (String.length s);
              o.base)
            argv
        in
        let n = List.length ptrs in
        let o =
          new_obj st
            ~size:(8 * (n + 1))
            ~align:8 ~writable:true ~what:"argv" ~var_global:false
        in
        List.iteri
          (fun k p ->
            write_value st ptr
              (Int64.add o.base (Int64.of_int (8 * k)))
              [| Bits [| p |] |])
          ptrs;
        [| int_arg 32 (Int64.of_int n); [| Bits [| o.base |] |] |]
    | _ -> error "@main has a signature other than () or (i32, ptr)"
  in
  let status, trapped =
    match run_call st main main_fn.sig_ args with
    | Returned [| [| Bits b |] |] -> (Int64.to_int (Int64.logand b.(0) 0xffL), None)
    | Returned _ -> error "@main returned poison"
    | Exited c -> (c, None)
    | Trapped k -> ((match k with Trap_insn -> 132 | Trap_div -> 136), Some k)
  in
  { status; stdout = Buffer.contents out; stderr = Buffer.contents err; trapped }

(* ---------- compile-time evaluation (Fas constants from calls) ---------- *)

type const_result =
  | Const_value of v array
  (* the evaluation trapped; the text is what it wrote to stderr first, which
     for a failed Fas `assert` is "file:line: assertion failed: message" *)
  | Const_trapped of trap_kind * string
  | Const_error of string

(* Runs the argument-less function [name] under [const_config]: no C calls, no
   `var` globals, no system calls. A write to stderr is recorded rather than
   refused straight away, so a failing `assert` (write, then trap) reports its
   message; an evaluation that writes and then returns normally still made a
   system call and is an error. *)
let eval_const ?(step_limit = default_step_limit) (ms : modul list) name =
  let err = Buffer.create 64 and wrote = ref false in
  let syscall st (a : int64 array) =
    let arg k = if k < Array.length a then a.(k) else 0L in
    match (a.(0), arg 1) with
    | 1L, 2L ->
        let len = Int64.to_int (arg 3) in
        Buffer.add_string err (read_string st (arg 2) len);
        wrote := true;
        Int64.of_int len
    | _ -> refuse_syscall st a
  in
  let cfg = { const_config with step_limit; syscall } in
  match
    let st = create cfg ms in
    run_function st name [||]
  with
  | Returned _ when !wrote ->
      Const_error
        "makes a system call (number 1), which compile-time evaluation cannot do"
  | Returned values -> Const_value values
  | Trapped kind -> Const_trapped (kind, Buffer.contents err)
  | Exited _ -> Const_error "exits the program"
  | exception Error message -> Const_error message

(* the outcome of a `const { }` fill (Fas D2): the bytes of the filled global,
   None marking a poison byte, or why the evaluation failed *)
type fill_result = Fill_bytes of int option array | Fill_failed of const_result

(* Runs the argument-less function [name] as [eval_const] does, except that
   the global [fill] (of the first module) may be read and written although it
   is writable; every other `var` global stays refused. Returns that global's
   bytes when the function returns. *)
let eval_fill ?(step_limit = default_step_limit) (ms : modul list) ~fill name =
  let err = Buffer.create 64 and wrote = ref false in
  let syscall st (a : int64 array) =
    let arg k = if k < Array.length a then a.(k) else 0L in
    match (a.(0), arg 1) with
    | 1L, 2L ->
        let len = Int64.to_int (arg 3) in
        Buffer.add_string err (read_string st (arg 2) len);
        wrote := true;
        Int64.of_int len
    | _ -> refuse_syscall st a
  in
  let cfg = { const_config with step_limit; syscall } in
  match
    let st = create cfg ms in
    let target =
      match lookup st 0 fill with
      | Obj o ->
          (* the same bytes, no longer counted as a `var` *)
          let o' = { o with var_global = false } in
          st.objs <- Amap.add o.base o' st.objs;
          if Hashtbl.mem st.locals_tbl (0, fill) then
            Hashtbl.replace st.locals_tbl (0, fill) (Obj o')
          else Hashtbl.replace st.globals_tbl fill (Obj o');
          o'
      | _ -> error "fills `%s`, which is not a plain global" fill
    in
    (run_function st name [||], target)
  with
  | Returned _, _ when !wrote ->
      Fill_failed
        (Const_error
           "makes a system call (number 1), which compile-time evaluation cannot do")
  | Returned _, o ->
      Fill_bytes
        (Array.init o.osize (fun k ->
             match o.pois with
             | Some p when Bytes.get p k <> '\000' -> None
             | _ -> Some (Char.code (Bytes.get o.data k))))
  | Trapped kind, _ -> Fill_failed (Const_trapped (kind, Buffer.contents err))
  | Exited _, _ -> Fill_failed (Const_error "exits the program")
  | exception Error message -> Fill_failed (Const_error message)
