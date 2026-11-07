(* Mir_isel: instruction selection, Fir (after Fir_wide) -> Mir.

   One Fir function at a time, straight from its tables: every Fir value of
   scalar type gets one virtual register, constants are materialized again
   at each use (no constant pool, no hoisting: this is the correctness
   baseline, not the fast path). Fir block b is Mir block b; blocks added
   here (critical-edge copies, switch chains, internal control flow of
   memmove and atomic loops) come after them.

   Value conventions (see mir.ml): integers and pointers are zero-extended
   to 64 bits in a GPR; f32/f64 sit in the low lane of an xmm register.
   Every rule below computes into fresh virtual registers and then copies
   the result into the Fir value's register, so a rule can never clobber an
   operand it still needs.

   Flags: a rule that compares and then reads the flags (setcc, cmov, jcc)
   materializes every operand before the compare, and between the two emits
   only copies, [Movi] (a real mov) and loads. *)

open Mir
module F = Fir

exception Unsupported of string

let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

type ctx = {
  fn : F.func;
  mf : Mir.func;
  pic : bool;
  local_syms : (string, unit) Hashtbl.t;
      (** symbols defined here that no other module can preempt *)
  label_funcs : (string, unit) Hashtbl.t;
      (** functions defined here and called directly (not weak) *)
  tls_syms : (string, unit) Hashtbl.t;  (** thread-local variables *)
  vals : (int, reg) Hashtbl.t;
  mutable cur : int;  (** the Mir block instructions go to *)
  mutable fir_block : int;
  mutable sret : reg option;  (** the hidden result pointer, in a MEMORY-class function *)
  mutable va : (int * int * int * int) option;
      (** in a variadic function: GPRs and xmm registers taken by named
          parameters, their stack bytes, and the register save area's slot *)
  blk_refs : (string * int, unit) Hashtbl.t;  (** blockaddr targets seen *)
  frame : (int, int) Hashtbl.t;
      (** static allocas (entry block, constant size, align <= 16): value ->
          frame slot. Their address is a constant rbp offset, so every use
          computes it afresh ([use]) or folds it into the memory operand
          ([addr_of]) instead of keeping one register busy for the whole
          function. *)
  cells : (int, reg * bool) Hashtbl.t;
      (** promoted allocas ([promotable_allocas]): value -> the virtual
          register holding the variable itself, and whether it starts at 0 *)
}

(* the local text symbol at block [b] of function [fn], for blockaddr *)
let block_symbol fn b = Printf.sprintf ".Lfas.blk.%s.%d" fn b

let emit c i =
  let b = Dynarray.get c.mf.blocks c.cur in
  b.code <- i :: b.code

let set_term c t = (Dynarray.get c.mf.blocks c.cur).term <- t
let fresh c k = new_vreg c.mf k
let cls_of_reg c = function V n -> Dynarray.get c.mf.vcls n | r -> class_of_phys r

let cls_of_ty = function
  | F.Int n when n <= 64 -> Gpr
  | F.Ptr _ -> Gpr
  | F.F32 | F.F64 -> Xmm
  | t -> unsupported "values of type %s" (F.ty_to_string t)

let width = function
  | F.Int n when n <= 64 -> n
  | F.Ptr _ -> 64
  | t -> unsupported "integer operation on %s" (F.ty_to_string t)

let is_dbl = function
  | F.F64 -> true
  | F.F32 -> false
  | t -> unsupported "float operation on %s" (F.ty_to_string t)

let ty c v = F.ty_of c.fn v
let fits32 (k : int64) = Int64.compare k (-0x8000_0000L) >= 0 && Int64.compare k 0x7fff_ffffL <= 0
let align_up x a = (x + a - 1) / a * a

let vreg c v =
  let v = F.resolve c.fn v in
  match Hashtbl.find_opt c.vals v with
  | Some r -> r
  | None ->
      let r = fresh c (cls_of_ty (ty c v)) in
      Hashtbl.add c.vals v r;
      r

let copy c r =
  let t = fresh c (cls_of_reg c r) in
  emit c (Copy (t, r));
  t

let movi c k =
  let t = fresh c Gpr in
  emit c (Movi (t, k));
  t

let slot_addr ?(disp = 0) s = { (at (Bslot s)) with disp }
let with_disp a d = { a with disp = a.disp + d }

(* a float constant from its bit pattern (f32: the low 32 bits) *)
let fbits c dbl (bits : int64) =
  let g = movi c (if dbl then bits else Int64.logand bits 0xffff_ffffL) in
  let x = fresh c Xmm in
  emit c (Movgx (dbl, x, g));
  x

let fconst c dbl (x : float) =
  fbits c dbl (if dbl then Int64.bits_of_float x else Int64.of_int32 (Int32.bits_of_float x))

(* The address of a symbol plus an addend: through the GOT when another
   module may define (or interpose) it, else rip-relative; a thread-local
   variable's is the thread pointer plus its offset. Only lea after
   the GOT load, so materializing it never touches the flags. *)
let sym_addr c name (addend : int64) =
  let t = fresh c Gpr in
  let add_addend () =
    if addend <> 0L then
      if fits32 addend then emit c (Lea (t, { (at_reg t) with disp = Int64.to_int addend }))
      else
        let k = movi c addend in
        emit c (Lea (t, { base = Breg t; index = Some k; scale = 1; disp = 0 }))
  in
  if Hashtbl.mem c.tls_syms name then begin
    (* a thread-local variable: thread pointer + its offset *)
    let tp = fresh c Gpr and off = fresh c Gpr in
    emit c (Tp tp);
    emit c (Tlsoff (off, name));
    emit c (Lea (t, { base = Breg tp; index = Some off; scale = 1; disp = 0 }));
    add_addend ()
  end
  else if c.pic && not (Hashtbl.mem c.local_syms name) then begin
    emit c (Got (t, name));
    add_addend ()
  end
  else if fits32 addend then emit c (Lea (t, { (at (Bsym name)) with disp = Int64.to_int addend }))
  else begin
    emit c (Lea (t, at (Bsym name)));
    add_addend ()
  end;
  t

(* the low [w] bits of [k], zero-extended (the canonical register form) *)
let mask w (k : int64) = if w >= 64 then k else Int64.logand k (Int64.pred (Int64.shift_left 1L w))

let zero_of c = function
  | Gpr -> movi c 0L
  | Xmm -> fbits c true 0L

let materialize c (k : F.const) =
  match k with
  | Cint (t, l) -> movi c (mask (width t) l.(0))
  | Czero t | Cnull t | Cpoison t -> zero_of c (cls_of_ty t)
  | Cfloat (t, bits) -> fbits c (is_dbl t) bits.(0)
  | Csym (name, addend) -> sym_addr c name addend
  | Cvec _ -> unsupported "vector constants"
  | Cblockaddr (fn, b) ->
      Hashtbl.replace c.blk_refs (fn, b) ();
      let name = block_symbol fn b in
      Hashtbl.replace c.local_syms name ();
      sym_addr c name 0L

(* the register holding Fir value [v] (a fresh one for a constant) *)
let use c v =
  match F.const_of c.fn v with
  | Some k -> materialize c k
  | None -> (
      match Hashtbl.find_opt c.frame (F.resolve c.fn v) with
      | Some s ->
          let t = fresh c Gpr in
          emit c (Lea (t, slot_addr s));
          t
      | None -> vreg c v)

(* the memory operand for an access through pointer [p] *)
let addr_of c p =
  match Hashtbl.find_opt c.frame (F.resolve c.fn p) with
  | Some s -> slot_addr s
  | None -> at_reg (use c p)

let def c v r = emit c (Copy (vreg c v, r))

(* ---------- integer helpers ---------- *)

(* zero-extend the low [w] bits of [r] in place (may clobber the flags) *)
let norm c w r =
  match w with
  | 64 -> ()
  | 8 | 16 | 32 -> emit c (Ext (w, false, r, r))
  | 1 -> emit c (Alui (X64.And, r, 1))
  | _ -> unsupported "i%d arithmetic" w

(* a fresh register with [r] sign-extended from [w] bits to 64 *)
let sx c w r =
  let t = fresh c Gpr in
  (match w with
  | 64 -> emit c (Copy (t, r))
  | 8 | 16 | 32 -> emit c (Ext (w, true, t, r))
  | 1 ->
      emit c (Copy (t, r));
      emit c (Neg t)
  | _ -> unsupported "i%d arithmetic" w);
  t

let smax w = if w = 64 then Int64.max_int else Int64.pred (Int64.shift_left 1L (w - 1))
let smin w = if w = 64 then Int64.min_int else Int64.neg (Int64.shift_left 1L (w - 1))
let umax w = if w = 64 then -1L else Int64.pred (Int64.shift_left 1L w)

let setcc c cond =
  let r = fresh c Gpr in
  emit c (Setcc (cond, r));
  r

(* ---------- integer operations ---------- *)

let ibin c (op : F.ibin) w a b =
  let ra = use c a and rb = use c b in
  let two alu ~wraps =
    let t = copy c ra in
    emit c (Alu (alu, t, rb));
    if wraps then norm c w t;
    t
  in
  let div signed rem =
    let x, y = if signed then (sx c w ra, sx c w rb) else (ra, rb) in
    emit c (Copy (G Rax, x));
    if signed then emit c Cqo else emit c (Movi (G Rdx, 0L));
    emit c (Div (signed, y));
    let t = copy c (G (if rem then Rdx else Rax)) in
    if signed then norm c w t;
    t
  in
  let shift sh =
    let t = if sh = X64.Sar then sx c w ra else copy c ra in
    emit c (Copy (G Rcx, rb));
    emit c (Shift (sh, t));
    if sh <> X64.Shr then norm c w t;
    t
  in
  let minmax signed cond =
    let r = copy c ra in
    if signed && w < 64 then begin
      let x = sx c w ra and y = sx c w rb in
      emit c (Alu (Cmp, x, y))
    end
    else emit c (Alu (Cmp, ra, rb));
    emit c (Cmov (cond, r, rb));
    r
  in
  match op with
  | Add -> two Add ~wraps:true
  | Sub -> two Sub ~wraps:true
  | And -> two And ~wraps:false
  | Or -> two Or ~wraps:false
  | Xor -> two Xor ~wraps:false
  | Mul ->
      let t = copy c ra in
      emit c (Imul (t, rb));
      norm c w t;
      t
  | Udiv -> div false false
  | Urem -> div false true
  | Sdiv -> div true false
  | Srem -> div true true
  | Shl -> shift Shl
  | Lshr -> shift Shr
  | Ashr -> shift Sar
  | Smin -> minmax true G
  | Smax -> minmax true L
  | Umin -> minmax false A
  | Umax -> minmax false B
  | Smulh | Umulh ->
      let signed = op = Smulh in
      if w = 64 then begin
        emit c (Copy (G Rax, ra));
        emit c (Mul1 (signed, rb));
        copy c (G Rdx)
      end
      else
        (* both operands fit in 32 bits: the 64-bit product is exact *)
        let x = if signed then sx c w ra else copy c ra in
        let y = if signed then sx c w rb else rb in
        emit c (Imul (x, y));
        emit c (Shifti ((if signed then Sar else Shr), x, w));
        norm c w x;
        x
  | Uadd_sat ->
      let t = copy c ra in
      if w = 64 then begin
        let m = movi c (-1L) in
        emit c (Alu (Add, t, rb));
        emit c (Cmov (B, t, m))
      end
      else begin
        let hi = movi c (umax w) in
        emit c (Alu (Add, t, rb));
        emit c (Alu (Cmp, t, hi));
        emit c (Cmov (A, t, hi))
      end;
      t
  | Usub_sat ->
      let t = copy c ra in
      let z = movi c 0L in
      emit c (Alu (Sub, t, rb));
      emit c (Cmov (B, t, z));
      t
  | Sadd_sat | Ssub_sat ->
      let alu = if op = Sadd_sat then X64.Add else X64.Sub in
      if w = 64 then begin
        (* on overflow the result saturates toward the sign of a *)
        let s = copy c ra in
        emit c (Shifti (Sar, s, 63));
        let k = movi c Int64.max_int in
        emit c (Alu (Xor, s, k));
        let t = copy c ra in
        emit c (Alu (alu, t, rb));
        emit c (Cmov (O, t, s));
        t
      end
      else begin
        let x = sx c w ra and y = sx c w rb in
        emit c (Alu (alu, x, y));
        let hi = movi c (smax w) and lo = movi c (smin w) in
        emit c (Alu (Cmp, x, hi));
        emit c (Cmov (G, x, hi));
        emit c (Alu (Cmp, x, lo));
        emit c (Cmov (L, x, lo));
        norm c w x;
        x
      end

let ovf c (op : F.ovf) w a b =
  let ra = use c a and rb = use c b in
  let signed = match op with Sadd_ovf | Ssub_ovf | Smul_ovf -> true | _ -> false in
  let apply t y =
    match op with
    | Sadd_ovf | Uadd_ovf -> emit c (Alu (Add, t, y))
    | Ssub_ovf | Usub_ovf -> emit c (Alu (Sub, t, y))
    | Smul_ovf | Umul_ovf -> emit c (Imul (t, y))
  in
  if w = 64 then
    match op with
    | Umul_ovf ->
        emit c (Copy (G Rax, ra));
        emit c (Mul1 (false, rb));
        let v = copy c (G Rax) in
        (v, setcc c O)
    | _ ->
        let t = copy c ra in
        apply t rb;
        (t, setcc c (if signed then O else B))
  else begin
    (* the exact result in 64 bits; overflow iff it differs from the wrapped
       value, re-extended *)
    let e, y = if signed then (sx c w ra, sx c w rb) else (copy c ra, rb) in
    apply e y;
    let v = copy c e in
    norm c w v;
    let back = if signed then sx c w v else v in
    emit c (Alu (Cmp, back, e));
    (v, setcc c Ne)
  end

let icmp_cond : F.icmp -> X64.cond = function
  | Eq -> E
  | Ne -> Ne
  | Slt -> L
  | Sle -> Le
  | Sgt -> G
  | Sge -> Ge
  | Ult -> B
  | Ule -> Be
  | Ugt -> A
  | Uge -> Ae

let icmp c (p : F.icmp) a b =
  let w = width (ty c a) in
  let ra = use c a and rb = use c b in
  let signed = match p with Slt | Sle | Sgt | Sge -> true | _ -> false in
  let x, y = if signed && w < 64 then (sx c w ra, sx c w rb) else (ra, rb) in
  emit c (Alu (Cmp, x, y));
  setcc c (icmp_cond p)

let iun c (op : F.iun) w a =
  let ra = use c a in
  match op with
  | Popcnt ->
      let t = fresh c Gpr in
      emit c (Popcnt (t, ra));
      t
  | Bswap ->
      let t = copy c ra in
      if w >= 16 then begin
        emit c (Bswap t);
        if w < 64 then emit c (Shifti (Shr, t, 64 - w))
      end;
      t
  | Ctz ->
      if w < 64 then begin
        (* a sentinel bit at w makes the input nonzero: ctz 0 = w *)
        let t = copy c ra in
        let m = movi c (Int64.shift_left 1L w) in
        emit c (Alu (Or, t, m));
        let r = fresh c Gpr in
        emit c (Bsf (r, t));
        r
      end
      else begin
        let r = fresh c Gpr in
        let k = movi c 64L in
        emit c (Bsf (r, ra));
        emit c (Cmov (E, r, k));
        r
      end
  | Clz ->
      (* (w - 1) - bsr, with bsr 0 taken as -1 *)
      let r = fresh c Gpr in
      let k = movi c (-1L) in
      emit c (Bsr (r, ra));
      emit c (Cmov (E, r, k));
      let res = movi c (Int64.of_int (w - 1)) in
      emit c (Alu (Sub, res, r));
      res
  | Bitrev ->
      let t = copy c ra in
      if w > 1 then begin
        List.iter
          (fun (s, m) ->
            let hi = copy c t in
            emit c (Shifti (Shr, hi, s));
            let mk = movi c m in
            emit c (Alu (And, hi, mk));
            emit c (Alu (And, t, mk));
            emit c (Shifti (Shl, t, s));
            emit c (Alu (Or, t, hi)))
          [ (1, 0x5555_5555_5555_5555L); (2, 0x3333_3333_3333_3333L); (4, 0x0f0f_0f0f_0f0f_0f0fL) ];
        emit c (Bswap t);
        if w < 64 then emit c (Shifti (Shr, t, 64 - w))
      end;
      t
  | Abs ->
      let s = if w < 64 then sx c w ra else ra in
      let r = copy c s in
      emit c (Neg r);
      emit c (Cmov (S, r, s));
      norm c w r;
      r

let funnel c left w a b amt =
  let ra = use c a and rb = use c b and rc = use c amt in
  if w = 64 then begin
    let t = copy c (if left then ra else rb) in
    emit c (Copy (G Rcx, rc));
    emit c (Shd (left, t, if left then rb else ra));
    t
  end
  else begin
    (* x = a:b in 2w <= 64 bits; fshl = x >> (w - k), fshr = x >> k *)
    let x = copy c ra in
    emit c (Shifti (Shl, x, w));
    emit c (Alu (Or, x, rb));
    let k = copy c rc in
    emit c (Alui (And, k, w - 1));
    if left then begin
      let n = movi c (Int64.of_int w) in
      emit c (Alu (Sub, n, k));
      emit c (Copy (G Rcx, n))
    end
    else emit c (Copy (G Rcx, k));
    emit c (Shift (Shr, x));
    norm c w x;
    x
  end

(* ---------- float operations ---------- *)

let to_gpr c dbl x =
  let g = fresh c Gpr in
  emit c (Movxg (dbl, g, x));
  g

let to_xmm c dbl g =
  let x = fresh c Xmm in
  emit c (Movgx (dbl, x, g));
  x

let sign_bit dbl = if dbl then Int64.min_int else 0x8000_0000L
let mag_mask dbl = if dbl then Int64.max_int else 0x7fff_ffffL

let fbin c (op : F.fbin) dbl a b =
  let ra = use c a and rb = use c b in
  let arith fop =
    let t = copy c ra in
    emit c (Fop (fop, dbl, t, rb));
    t
  in
  match op with
  | Fadd -> arith Fadd
  | Fsub -> arith Fsub
  | Fmul -> arith Fmul
  | Fdiv -> arith Fdiv
  | Frem ->
      let s = new_slot c.mf ~size:16 ~align:8 in
      emit c (Fstore (dbl, slot_addr s, ra));
      emit c (Fstore (dbl, slot_addr ~disp:8 s, rb));
      emit c (Frem87 (dbl, s));
      let t = fresh c Xmm in
      emit c (Fload (dbl, t, slot_addr s));
      t
  | Fmin | Fmax ->
      (* minimumNumber/maximumNumber on the bit patterns: take the smaller
         (larger); when equal, OR (AND) the patterns so -0 < +0; a NaN loses
         to the other operand *)
      let min = op = Fmin in
      let ga = to_gpr c dbl ra and gb = to_gpr c dbl rb in
      let o = copy c ga in
      emit c (Alu ((if min then Or else And), o, gb));
      if min then emit c (Ucomi (dbl, ra, rb)) else emit c (Ucomi (dbl, rb, ra));
      let r = copy c ga in
      emit c (Cmov (A, r, gb));
      emit c (Cmov (E, r, o));
      emit c (Ucomi (dbl, ra, ra));
      emit c (Cmov (P, r, gb));
      emit c (Ucomi (dbl, rb, rb));
      emit c (Cmov (P, r, ga));
      to_xmm c dbl r
  | Copysign ->
      let ga = to_gpr c dbl ra and gb = to_gpr c dbl rb in
      let m = movi c (mag_mask dbl) and s = movi c (sign_bit dbl) in
      emit c (Alu (And, ga, m));
      emit c (Alu (And, gb, s));
      emit c (Alu (Or, ga, gb));
      to_xmm c dbl ga

let fun_ c (op : F.fun_) dbl a =
  let ra = use c a in
  let bits alu k =
    let g = to_gpr c dbl ra in
    let m = movi c k in
    emit c (Alu (alu, g, m));
    to_xmm c dbl g
  in
  let round mode =
    let t = fresh c Xmm in
    emit c (Round (dbl, t, ra, mode));
    t
  in
  match op with
  | Fneg -> bits Xor (sign_bit dbl)
  | Fabs -> bits And (mag_mask dbl)
  | Sqrt ->
      let t = fresh c Xmm in
      emit c (Fsqrt (dbl, t, ra));
      t
  | Floor -> round 1
  | Ceil -> round 2
  | Ftrunc -> round 3
  | Roundeven -> round 0

(* ucomi sets ZF PF CF all to 1 when unordered *)
let fcmp c (p : F.fcmp) a b =
  let dbl = is_dbl (ty c a) in
  let ra = use c a and rb = use c b in
  let cmp x y cond =
    emit c (Ucomi (dbl, x, y));
    setcc c cond
  in
  let both x y c1 c2 alu =
    emit c (Ucomi (dbl, x, y));
    let r1 = setcc c c1 in
    let r2 = setcc c c2 in
    emit c (Alu (alu, r1, r2));
    r1
  in
  match p with
  | Ffalse -> movi c 0L
  | Ftrue -> movi c 1L
  | Foeq -> both ra rb E Np And
  | Fune -> both ra rb Ne P Or
  | Fogt -> cmp ra rb A
  | Foge -> cmp ra rb Ae
  | Folt -> cmp rb ra A
  | Fole -> cmp rb ra Ae
  | Fone -> cmp ra rb Ne
  | Ford -> cmp ra rb Np
  | Fueq -> cmp ra rb E
  | Fugt -> cmp rb ra B
  | Fuge -> cmp rb ra Be
  | Fult -> cmp ra rb B
  | Fule -> cmp ra rb Be
  | Funo -> cmp ra rb P

(* ---------- conversions ---------- *)

(* f -> u64 for f in [0, 2^64): values from 2^63 up are reduced by 2^63
   and get the top bit back *)
let fptou64 c dbl ra =
  let k = fconst c dbl 9223372036854775808.0 in
  let r1 = fresh c Gpr in
  emit c (Cvttsi (dbl, r1, ra));
  let t = copy c ra in
  emit c (Fop (Fsub, dbl, t, k));
  let r2 = fresh c Gpr in
  emit c (Cvttsi (dbl, r2, t));
  let m = movi c Int64.min_int in
  emit c (Alu (Xor, r2, m));
  emit c (Ucomi (dbl, ra, k));
  emit c (Cmov (Ae, r1, r2));
  r1

let conv c (op : F.conv) rt a =
  let st = ty c a in
  let ra = use c a in
  match op with
  | Trunc ->
      let t = copy c ra in
      norm c (width rt) t;
      t
  | Zext | Inttoptr -> copy c ra
  | Ptrtoint ->
      let t = copy c ra in
      norm c (width rt) t;
      t
  | Sext ->
      let t = sx c (width st) ra in
      norm c (width rt) t;
      t
  | Fptrunc ->
      let t = fresh c Xmm in
      emit c (Cvtff (true, t, ra));
      t
  | Fpext ->
      let t = fresh c Xmm in
      emit c (Cvtff (false, t, ra));
      t
  | Fptosi ->
      let t = fresh c Gpr in
      emit c (Cvttsi (is_dbl st, t, ra));
      norm c (width rt) t;
      t
  | Fptoui ->
      let dbl = is_dbl st and w = width rt in
      if w = 64 then fptou64 c dbl ra
      else begin
        let t = fresh c Gpr in
        emit c (Cvttsi (dbl, t, ra));
        norm c w t;
        t
      end
  | Fptosi_sat ->
      let dbl = is_dbl st and w = width rt in
      let r = fresh c Gpr in
      emit c (Cvttsi (dbl, r, ra));
      let hi = movi c (smax w) in
      if w < 64 then begin
        let lo = movi c (smin w) in
        emit c (Alu (Cmp, r, hi));
        emit c (Cmov (G, r, hi));
        emit c (Alu (Cmp, r, lo));
        emit c (Cmov (L, r, lo))
      end;
      (* cvtt gives MIN for anything out of the 64-bit range and NaN *)
      let lim = fconst c dbl (Float.ldexp 1.0 (w - 1)) in
      let z = movi c 0L in
      emit c (Ucomi (dbl, ra, lim));
      emit c (Cmov (Ae, r, hi));
      emit c (Ucomi (dbl, ra, ra));
      emit c (Cmov (P, r, z));
      norm c w r;
      r
  | Fptoui_sat ->
      let dbl = is_dbl st and w = width rt in
      let z = movi c 0L and hi = movi c (umax w) in
      let r =
        if w = 64 then begin
          let r = fptou64 c dbl ra in
          let f0 = fconst c dbl 0.0 in
          emit c (Ucomi (dbl, f0, ra));
          emit c (Cmov (A, r, z));
          r
        end
        else begin
          let r = fresh c Gpr in
          emit c (Cvttsi (dbl, r, ra));
          emit c (Alu (Cmp, r, z));
          emit c (Cmov (L, r, z));
          emit c (Alu (Cmp, r, hi));
          emit c (Cmov (A, r, hi));
          r
        end
      in
      let lim = fconst c dbl (Float.ldexp 1.0 w) in
      emit c (Ucomi (dbl, ra, lim));
      emit c (Cmov (Ae, r, hi));
      emit c (Ucomi (dbl, ra, ra));
      emit c (Cmov (P, r, z));
      r
  | Sitofp ->
      let s = sx c (width st) ra in
      let t = fresh c Xmm in
      emit c (Cvtsi (is_dbl rt, t, s));
      t
  | Uitofp ->
      let dbl = is_dbl rt in
      if width st < 64 then begin
        let t = fresh c Xmm in
        emit c (Cvtsi (dbl, t, ra));
        t
      end
      else begin
        (* the top bit set: convert (x >> 1 | x & 1) and double it *)
        let f1 = fresh c Xmm in
        emit c (Cvtsi (dbl, f1, ra));
        let h = copy c ra and l = copy c ra in
        emit c (Shifti (Shr, h, 1));
        emit c (Alui (And, l, 1));
        emit c (Alu (Or, h, l));
        let f2 = fresh c Xmm in
        emit c (Cvtsi (dbl, f2, h));
        emit c (Fop (Fadd, dbl, f2, f2));
        let g1 = to_gpr c dbl f1 and g2 = to_gpr c dbl f2 in
        emit c (Test (ra, ra));
        emit c (Cmov (S, g1, g2));
        to_xmm c dbl g1
      end
  | Bitcast -> (
      match (cls_of_ty st, cls_of_ty rt) with
      | Gpr, Gpr | Xmm, Xmm -> copy c ra
      | Gpr, Xmm -> to_xmm c (is_dbl rt) ra
      | Xmm, Gpr -> to_gpr c (is_dbl st) ra)

(* ---------- memory ---------- *)

let mem_size t =
  match t with
  | F.Int _ | F.Ptr _ | F.F32 | F.F64 -> F.size_of t
  | t -> unsupported "memory access of type %s" (F.ty_to_string t)

(* copies [n] bytes between two addresses (registers or slots) *)
let copy_bytes c ~src ~dst n =
  if n > 128 then begin
    let s = fresh c Gpr and d = fresh c Gpr in
    emit c (Lea (s, src));
    emit c (Lea (d, dst));
    emit c (Copy (G Rsi, s));
    emit c (Copy (G Rdi, d));
    emit c (Movi (G Rcx, Int64.of_int n));
    emit c Rep_movsb
  end
  else
    let rec go off =
      if off < n then begin
        let k = if n - off >= 8 then 8 else if n - off >= 4 then 4 else if n - off >= 2 then 2 else 1 in
        let t = fresh c Gpr in
        emit c (Load (k, false, t, with_disp src off));
        emit c (Store (k, with_disp dst off, t));
        go (off + k)
      end
    in
    go 0

let ptradd c p idx scale disp =
  let rp = use c p in
  let t = fresh c Gpr in
  (match F.int_value c.fn idx with
  | Some k ->
      let off = Int64.add (Int64.mul k (Int64.of_int scale)) (Int64.of_int disp) in
      if fits32 off then emit c (Lea (t, { (at_reg rp) with disp = Int64.to_int off }))
      else
        let o = movi c off in
        emit c (Lea (t, { base = Breg rp; index = Some o; scale = 1; disp = 0 }))
  | None ->
      let i = sx c (width (ty c idx)) (use c idx) in
      if List.mem scale [ 1; 2; 4; 8 ] && fits32 (Int64.of_int disp) then
        emit c (Lea (t, { base = Breg rp; index = Some i; scale; disp }))
      else begin
        let k = movi c (Int64.of_int scale) in
        emit c (Imul (i, k));
        let d = movi c (Int64.of_int disp) in
        emit c (Alu (Add, i, d));
        emit c (Lea (t, { base = Breg rp; index = Some i; scale = 1; disp = 0 }))
      end);
  t

let memmove c d s n =
  let rd = use c d and rs = use c s and rn = use c n in
  let fwd = new_block c.mf and back = new_block c.mf and join = new_block c.mf in
  emit c (Alu (Cmp, rd, rs));
  set_term c (Jcc (A, back, fwd));
  c.cur <- fwd;
  emit c (Copy (G Rdi, rd));
  emit c (Copy (G Rsi, rs));
  emit c (Copy (G Rcx, rn));
  emit c Rep_movsb;
  set_term c (Jmp join);
  (* dst above src: copy downwards from the last byte *)
  c.cur <- back;
  let last r =
    let t = copy c r in
    emit c (Lea (t, { base = Breg t; index = Some rn; scale = 1; disp = -1 }));
    t
  in
  let ld = last rd and ls = last rs in
  emit c (Copy (G Rdi, ld));
  emit c (Copy (G Rsi, ls));
  emit c (Copy (G Rcx, rn));
  emit c Std;
  emit c Rep_movsb;
  emit c Cld;
  set_term c (Jmp join);
  c.cur <- join

let rmw c (op : F.rmw) t p v =
  let size = mem_size t in
  let rp = use c p and rv = use c v in
  let addr = at_reg rp in
  let float = F.is_float t in
  let dbl = float && is_dbl t in
  match op with
  | Xchg ->
      let x = if float then to_gpr c dbl rv else copy c rv in
      emit c (Xchg (size, addr, x));
      if float then to_xmm c dbl x else x
  | Radd ->
      let x = copy c rv in
      emit c (Xadd (size, addr, x));
      x
  | Rsub ->
      let x = copy c rv in
      emit c (Neg x);
      emit c (Xadd (size, addr, x));
      norm c (8 * size) x;
      x
  | _ ->
      (* a compare-and-swap loop; [old] is rewritten on each try *)
      let old = fresh c Gpr in
      emit c (Load (size, false, old, addr));
      let loop = new_block c.mf and exit = new_block c.mf in
      set_term c (Jmp loop);
      c.cur <- loop;
      let w = 8 * size in
      let nw =
        match op with
        | Rand | Ror | Rxor ->
            let n = copy c old in
            emit c (Alu ((match op with Rand -> And | Ror -> Or | _ -> Xor), n, rv));
            n
        | Rmin | Rmax ->
            let n = copy c old in
            let x = sx c w old and y = sx c w rv in
            emit c (Alu (Cmp, x, y));
            emit c (Cmov ((if op = Rmin then G else L), n, rv));
            n
        | Rumin | Rumax ->
            let n = copy c old in
            emit c (Alu (Cmp, old, rv));
            emit c (Cmov ((if op = Rumin then A else B), n, rv));
            n
        | Rfadd | Rfsub ->
            let x = to_xmm c dbl old in
            emit c (Fop ((if op = Rfadd then Fadd else Fsub), dbl, x, rv));
            to_gpr c dbl x
        | Xchg | Radd | Rsub -> assert false
      in
      emit c (Copy (G Rax, old));
      emit c (Cmpxchg (size, addr, nw));
      emit c (Copy (old, G Rax));
      set_term c (Jcc (Ne, loop, exit));
      c.cur <- exit;
      if float then to_xmm c dbl old else old

(* ---------- calls and the SysV ABI ---------- *)

type eightbyte = Eint | Esse
type agg_class = Regs of eightbyte list | Memory

let classify ~size ~align ~fields =
  if align > 16 then unsupported "agg with align %d" align;
  List.iter
    (fun (_, t) ->
      match t with
      | F.Int n when n <= 64 -> ()
      | F.Ptr _ | F.F32 | F.F64 -> ()
      | t -> unsupported "agg field of type %s" (F.ty_to_string t))
    fields;
  if size > 16 || List.exists (fun (o, t) -> o mod F.size_of t <> 0) fields then Memory
  else
    Regs
      (List.init ((size + 7) / 8) (fun i ->
           let fs = List.filter (fun (o, _) -> o / 8 = i) fields in
           if fs = [] then unsupported "agg eightbyte without fields";
           if List.for_all (fun (_, t) -> t = F.F32 || t = F.F64) fs then Esse else Eint))

type loc = Lreg of reg | Lstack of int

type plan =
  | Pscalar of F.ty * F.ext option * loc
  | Pregs of int * (eightbyte * reg) list  (** agg size, eightbytes in registers *)
  | Pstack of int * int  (** agg size, stack offset *)

(* Assigns registers and stack offsets to a list of ABI types; returns the
   plans, the stack bytes used and the number of xmm registers used *)
let plan_args ~sret abis =
  let gp = ref (if sret then 1 else 0) and xp = ref 0 and st = ref 0 in
  let stack size align =
    st := align_up !st align;
    let o = !st in
    st := o + align_up size 8;
    o
  in
  let plans =
    List.map
      (fun (abi : F.abi) ->
        match abi with
        | Scalar t | Ext (t, _) -> (
            let ext = match abi with Ext (_, e) -> Some e | _ -> None in
            (match t with F.Int n when n > 64 -> unsupported "i%d parameter" n | _ -> ());
            match cls_of_ty t with
            | Gpr ->
                if !gp < 6 then begin
                  incr gp;
                  Pscalar (t, ext, Lreg (G gpr_args.(!gp - 1)))
                end
                else Pscalar (t, ext, Lstack (stack 8 8))
            | Xmm ->
                if !xp < 8 then begin
                  incr xp;
                  Pscalar (t, ext, Lreg (X (!xp - 1)))
                end
                else Pscalar (t, ext, Lstack (stack 8 8)))
        | Agg { size; align; fields } -> (
            match classify ~size ~align ~fields with
            | Regs ebs ->
                let ng = List.length (List.filter (( = ) Eint) ebs) in
                let nx = List.length ebs - ng in
                if !gp + ng <= 6 && !xp + nx <= 8 then
                  Pregs
                    ( size,
                      List.map
                        (fun eb ->
                          match eb with
                          | Eint ->
                              incr gp;
                              (eb, G gpr_args.(!gp - 1))
                          | Esse ->
                              incr xp;
                              (eb, X (!xp - 1)))
                        ebs )
                else Pstack (size, stack size (max 8 align))
            | Memory -> Pstack (size, stack size (max 8 align))))
      abis
  in
  (plans, !st, !xp)

type ret_plan = Rnone | Rscalar of F.ty | Rregs of int * (eightbyte * reg) list | Rsret of int * int

let plan_ret (abis : F.abi list) =
  match abis with
  | [] -> Rnone
  | [ (Scalar t | Ext (t, _)) ] -> Rscalar t
  | [ Agg { size; align; fields } ] -> (
      match classify ~size ~align ~fields with
      | Memory -> Rsret (size, align)
      | Regs ebs ->
          let gi = ref 0 and xi = ref 0 in
          Rregs
            ( size,
              List.map
                (fun eb ->
                  match eb with
                  | Eint ->
                      incr gi;
                      (eb, G (if !gi = 1 then Rax else Rdx))
                  | Esse ->
                      incr xi;
                      (eb, X (!xi - 1)))
                ebs ))
  | _ -> unsupported "several results"

let ret_regs = function
  | Rnone -> []
  | Rscalar t -> [ (if cls_of_ty t = Xmm then X 0 else G Rax) ]
  | Rregs (_, l) -> List.map snd l
  | Rsret _ -> [ G Rax ]

(* stores the registers of an eightbyte list into a fresh slot *)
let spill_eightbytes c size l =
  let s = new_slot c.mf ~size:(max 8 (align_up size 8)) ~align:8 in
  List.iteri
    (fun i (eb, r) ->
      match eb with
      | Eint -> emit c (Store (8, slot_addr ~disp:(8 * i) s, r))
      | Esse -> emit c (Fstore (true, slot_addr ~disp:(8 * i) s, r)))
    l;
  let p = fresh c Gpr in
  emit c (Lea (p, slot_addr s));
  p

(* loads the eightbytes of the agg at [p] into fresh registers, through a
   padded copy so no load reads past the value *)
let load_eightbytes c size p l =
  let s = new_slot c.mf ~size:(max 8 (align_up size 8)) ~align:8 in
  copy_bytes c ~src:(at_reg p) ~dst:(slot_addr s) size;
  List.mapi
    (fun i (eb, r) ->
      match eb with
      | Eint ->
          let t = fresh c Gpr in
          emit c (Load (8, false, t, slot_addr ~disp:(8 * i) s));
          (r, t)
      | Esse ->
          let t = fresh c Xmm in
          emit c (Fload (true, t, slot_addr ~disp:(8 * i) s));
          (r, t))
    l

let call c ~(sig_ : F.signature) ~callee ~args ~va_aggs results =
  let nfixed = List.length sig_.param_abis in
  let abis =
    List.mapi
      (fun i a ->
        if i < nfixed then List.nth sig_.param_abis i
        else match List.assoc_opt i va_aggs with Some abi -> abi | None -> F.Scalar (ty c a))
      (Array.to_list args)
  in
  let rp = plan_ret sig_.result_abis in
  let sret = match rp with Rsret _ -> true | _ -> false in
  let plans, stack, nx = plan_args ~sret abis in
  c.mf.out_size <- max c.mf.out_size stack;
  let target =
    match callee with
    | F.Direct name -> if Hashtbl.mem c.label_funcs name then Tlabel name else Tsym name
    | F.Indirect v -> Treg (use c v)
  in
  (* memory first, then the register moves, so no temporary needs a
     register an argument already occupies *)
  let pending = ref [] in
  List.iteri
    (fun i plan ->
      let a = args.(i) in
      match plan with
      | Pscalar (t, ext, loc) -> (
          let r = use c a in
          let r = if ext = Some F.Sx then sx c (width t) r else r in
          match loc with
          | Lreg p -> pending := (p, r) :: !pending
          | Lstack o ->
              if cls_of_ty t = Xmm then emit c (Fstore (is_dbl t, { (at Bout) with disp = o }, r))
              else emit c (Store (8, { (at Bout) with disp = o }, r)))
      | Pregs (size, l) -> pending := List.rev_append (load_eightbytes c size (use c a) l) !pending
      | Pstack (size, o) -> copy_bytes c ~src:(at_reg (use c a)) ~dst:{ (at Bout) with disp = o } size)
    plans;
  let sret_slot =
    match rp with
    | Rsret (size, align) ->
        let s = new_slot c.mf ~size:(max 1 size) ~align:(max 8 align) in
        let p = fresh c Gpr in
        emit c (Lea (p, slot_addr s));
        pending := (G Rdi, p) :: !pending;
        Some s
    | _ -> None
  in
  let pending = List.rev !pending in
  List.iter (fun (p, r) -> emit c (Copy (p, r))) pending;
  let arg_regs = List.map fst pending in
  let arg_regs =
    if sig_.variadic then begin
      emit c (Movi (G Rax, Int64.of_int nx));
      G Rax :: arg_regs
    end
    else arg_regs
  in
  emit c (Call { target; args = arg_regs; rets = ret_regs rp });
  match (rp, results) with
  | Rnone, _ -> ()
  | Rscalar t, [| v |] ->
      if cls_of_ty t = Xmm then def c v (copy c (X 0))
      else begin
        let r = copy c (G Rax) in
        norm c (width t) r;
        def c v r
      end
  | Rregs (size, l), [| v |] -> def c v (spill_eightbytes c size l)
  | Rsret _, [| v |] ->
      let p = fresh c Gpr in
      emit c (Lea (p, slot_addr (Option.get sret_slot)));
      def c v p
  | _ -> unsupported "call result count"

let params c =
  let sig_ = c.fn.sig_ in
  let rp = plan_ret sig_.result_abis in
  let sret = match rp with Rsret _ -> true | _ -> false in
  if sret then c.sret <- Some (copy c (G Rdi));
  let plans, stack, _ = plan_args ~sret sig_.param_abis in
  if sig_.variadic then begin
    (* the register save area: rdi..r9 at 0..40, xmm0..7 at 48, 64, ... 160
       (8 bytes of each are enough: only f64/f32 are read back) *)
    let s = new_slot c.mf ~size:176 ~align:16 in
    Array.iteri (fun i r -> emit c (Store (8, slot_addr ~disp:(8 * i) s, G r))) gpr_args;
    for i = 0 to 7 do
      emit c (Fstore (true, slot_addr ~disp:(48 + (16 * i)) s, X i))
    done;
    let gp = List.length (List.filter (function Pscalar (_, _, Lreg (G _)) -> true | _ -> false) plans)
             + List.fold_left (fun n p -> match p with Pregs (_, l) -> n + List.length (List.filter (fun (e, _) -> e = Eint) l) | _ -> n) 0 plans
             + (if sret then 1 else 0)
    and fp = List.length (List.filter (function Pscalar (_, _, Lreg (X _)) -> true | _ -> false) plans)
             + List.fold_left (fun n p -> match p with Pregs (_, l) -> n + List.length (List.filter (fun (e, _) -> e = Esse) l) | _ -> n) 0 plans
    in
    c.va <- Some (gp, fp, stack, s)
  end;
  let ps = (F.block c.fn F.entry).params in
  if Array.length ps <> List.length plans then unsupported "entry parameters do not match the signature";
  List.iteri
    (fun i plan ->
      let v = ps.(i) in
      match plan with
      | Pscalar (t, _, Lreg p) ->
          let r = copy c p in
          if cls_of_ty t = Gpr then norm c (width t) r;
          def c v r
      | Pscalar (t, _, Lstack o) ->
          let a = { (at Bin) with disp = o } in
          if cls_of_ty t = Xmm then begin
            let r = fresh c Xmm in
            emit c (Fload (is_dbl t, r, a));
            def c v r
          end
          else begin
            let r = fresh c Gpr in
            emit c (Load (mem_size t, false, r, a));
            norm c (width t) r;
            def c v r
          end
      | Pregs (size, l) -> def c v (spill_eightbytes c size l)
      | Pstack (_, o) ->
          let r = fresh c Gpr in
          emit c (Lea (r, { (at Bin) with disp = o }));
          def c v r)
    plans

let ret c vals =
  let sig_ = c.fn.sig_ in
  match (plan_ret sig_.result_abis, vals) with
  | Rnone, [||] -> set_term c (Ret [])
  | Rscalar t, [| v |] ->
      let r = use c v in
      if cls_of_ty t = Xmm then begin
        emit c (Copy (X 0, r));
        set_term c (Ret [ X 0 ])
      end
      else begin
        let r = match sig_.result_abis with [ Ext (_, Sx) ] -> sx c (width t) r | _ -> r in
        emit c (Copy (G Rax, r));
        set_term c (Ret [ G Rax ])
      end
  | Rregs (size, l), [| p |] ->
      let moves = load_eightbytes c size (use c p) l in
      List.iter (fun (phys, t) -> emit c (Copy (phys, t))) moves;
      set_term c (Ret (List.map fst moves))
  | Rsret (size, _), [| p |] ->
      let dst = Option.get c.sret in
      copy_bytes c ~src:(at_reg (use c p)) ~dst:(at_reg dst) size;
      emit c (Copy (G Rax, dst));
      set_term c (Ret [ G Rax ])
  | _ -> unsupported "return value count"

(* r := r rounded up to [align] when that is more than the 16 the stack gives *)
let round_up_ptr c r align =
  if align > 16 then begin
    emit c (Alui (Add, r, align - 1));
    emit c (Alui (And, r, - align))
  end

(* ---------- variadic arguments (SysV va_list) ----------

   va_list: { u32 gp_offset; u32 fp_offset; ptr overflow_arg_area;
   ptr reg_save_area }, 24 bytes. *)

let va_info c =
  match c.va with Some v -> v | None -> unsupported "va_start in a function that is not variadic"

let va_start c p =
  let gp, fp, stack, s = va_info c in
  let rp = use c p in
  let t1 = movi c (Int64.of_int (8 * gp)) in
  emit c (Store (4, at_reg rp, t1));
  let t2 = movi c (Int64.of_int (48 + (16 * fp))) in
  emit c (Store (4, { (at_reg rp) with disp = 4 }, t2));
  let t3 = fresh c Gpr in
  emit c (Lea (t3, { (at Bin) with disp = stack }));
  emit c (Store (8, { (at_reg rp) with disp = 8 }, t3));
  let t4 = fresh c Gpr in
  emit c (Lea (t4, slot_addr s));
  emit c (Store (8, { (at_reg rp) with disp = 16 }, t4))

(* the next [size] bytes (rounded to 8) of the overflow area, aligned to
   [align]: returns their address and advances the va_list *)
let va_overflow c rp size align =
  let ov = fresh c Gpr in
  emit c (Load (8, false, ov, { (at_reg rp) with disp = 8 }));
  if align > 8 then begin
    emit c (Alui (Add, ov, align - 1));
    emit c (Alui (And, ov, - align))
  end;
  let r = copy c ov in
  emit c (Alui (Add, ov, align_up size 8));
  emit c (Store (8, { (at_reg rp) with disp = 8 }, ov));
  r

let va_arg c t p =
  let rp = use c p in
  let cls = cls_of_ty t in
  let off_disp, limit, step = match cls with Gpr -> (0, 48, 8) | Xmm -> (4, 176, 16) in
  let res = fresh c cls in
  let load_from addr =
    match cls with
    | Gpr -> emit c (Load (mem_size t, false, res, addr))
    | Xmm -> emit c (Fload (is_dbl t, res, addr))
  in
  let off = fresh c Gpr in
  emit c (Load (4, false, off, { (at_reg rp) with disp = off_disp }));
  let in_regs = new_block c.mf and on_stack = new_block c.mf and join = new_block c.mf in
  emit c (Alui (Cmp, off, limit));
  set_term c (Jcc (Ae, on_stack, in_regs));
  c.cur <- in_regs;
  let base = fresh c Gpr in
  emit c (Load (8, false, base, { (at_reg rp) with disp = 16 }));
  load_from { base = Breg base; index = Some off; scale = 1; disp = 0 };
  let o2 = copy c off in
  emit c (Alui (Add, o2, step));
  emit c (Store (4, { (at_reg rp) with disp = off_disp }, o2));
  set_term c (Jmp join);
  c.cur <- on_stack;
  load_from (at_reg (va_overflow c rp 8 8));
  set_term c (Jmp join);
  c.cur <- join;
  res

let va_arg_agg c (abi : F.abi) p =
  let size, align, fields =
    match abi with Agg { size; align; fields } -> (size, align, fields) | _ -> unsupported "va_arg_agg of a scalar"
  in
  let rp = use c p in
  let res = fresh c Gpr in
  (match classify ~size ~align ~fields with
  | Memory -> emit c (Copy (res, va_overflow c rp size align))
  | Regs ebs ->
      let ng = List.length (List.filter (( = ) Eint) ebs) in
      let nx = List.length ebs - ng in
      let gp = fresh c Gpr and fp = fresh c Gpr in
      emit c (Load (4, false, gp, at_reg rp));
      emit c (Load (4, false, fp, { (at_reg rp) with disp = 4 }));
      let on_stack = new_block c.mf and in_regs = new_block c.mf and join = new_block c.mf in
      (* every eightbyte must fit, else the whole value is on the stack *)
      let fits = new_block c.mf in
      emit c (Alui (Cmp, gp, 48 - (8 * ng)));
      set_term c (Jcc (A, on_stack, fits));
      c.cur <- fits;
      emit c (Alui (Cmp, fp, 176 - (16 * nx)));
      set_term c (Jcc (A, on_stack, in_regs));
      c.cur <- in_regs;
      let s = new_slot c.mf ~size:(align_up size 8) ~align:(max 8 align) in
      let area = fresh c Gpr in
      emit c (Load (8, false, area, { (at_reg rp) with disp = 16 }));
      List.iteri
        (fun i eb ->
          let off = if eb = Eint then gp else fp in
          let t = fresh c Gpr in
          emit c (Load (8, false, t, { base = Breg area; index = Some off; scale = 1; disp = 0 }));
          emit c (Store (8, slot_addr ~disp:(8 * i) s, t));
          emit c (Alui (Add, off, if eb = Eint then 8 else 16)))
        ebs;
      emit c (Store (4, at_reg rp, gp));
      emit c (Store (4, { (at_reg rp) with disp = 4 }, fp));
      emit c (Lea (res, slot_addr s));
      set_term c (Jmp join);
      c.cur <- on_stack;
      emit c (Copy (res, va_overflow c rp size align));
      set_term c (Jmp join);
      c.cur <- join);
  res

(* ---------- instructions and terminators ---------- *)

let inst c i =
  let ir = F.inst c.fn i in
  let res k = ir.results.(k) in
  let rty () = ty c (res 0) in
  let set r = def c (res 0) r in
  match ir.op with
  | Nop | Dbg_value _ -> ()
  | Ibin (op, _, a, b) -> set (ibin c op (width (rty ())) a b)
  | Iun (op, _, a) -> set (iun c op (width (rty ())) a)
  | Fbin (op, _, a, b) -> set (fbin c op (is_dbl (rty ())) a b)
  | Fun (op, _, a) -> set (fun_ c op (is_dbl (rty ())) a)
  | Fma (_, a, b, d) -> (
      (* a call to the correctly rounded helper Mir_runtime.add appended *)
      match rty () with
      | (F.F32 | F.F64) as t ->
          let sig_ = { F.param_abis = [ Scalar t; Scalar t; Scalar t ]; result_abis = [ Scalar t ]; variadic = false } in
          call c ~sig_ ~callee:(F.Direct (Mir_runtime.fma_name (t = F.F64))) ~args:[| a; b; d |] ~va_aggs:[] ir.results
      | _ -> unsupported "vector fma")
  | Fsh (left, a, b, n) -> set (funnel c left (width (rty ())) a b n)
  | Ovf (op, a, b) ->
      let v, o = ovf c op (width (ty c a)) a b in
      def c (res 0) v;
      def c (res 1) o
  | Icmp (p, a, b) ->
      (match ty c a with F.Vec _ -> unsupported "vector icmp" | _ -> ());
      set (icmp c p a b)
  | Fcmp (p, _, a, b) -> set (fcmp c p a b)
  | Select (cnd, x, y) ->
      let t = rty () in
      let rc = use c cnd and rx = use c x and ry = use c y in
      if cls_of_ty t = Xmm then begin
        let dbl = is_dbl t in
        let gx = to_gpr c dbl rx and gy = to_gpr c dbl ry in
        let r = copy c gy in
        emit c (Test (rc, rc));
        emit c (Cmov (Ne, r, gx));
        set (to_xmm c dbl r)
      end
      else begin
        let r = copy c ry in
        emit c (Test (rc, rc));
        emit c (Cmov (Ne, r, rx));
        set r
      end
  | Freeze a -> set (copy c (use c a))
  | Conv (op, a) -> set (conv c op (rty ()) a)
  | Alloca _ when Hashtbl.mem c.cells (F.resolve c.fn (res 0)) ->
      let cell, zero = Hashtbl.find c.cells (F.resolve c.fn (res 0)) in
      if zero then emit c (Copy (cell, zero_of c (cls_of_reg c cell)))
  | Alloca (n, align) -> (
      match F.int_value c.fn n with
      | Some k when c.fir_block = F.entry && Int64.compare k 0L >= 0 && Int64.compare k 0x1000_0000L < 0 ->
          if align > 0x1000 then unsupported "alloca with align %d" align;
          (* frame slots are at most 16-aligned (rbp is): a larger alignment
             takes align - 16 extra bytes and rounds the address up *)
          let extra = max 0 (align - 16) in
          let s = new_slot c.mf ~size:(max 1 (Int64.to_int k) + extra) ~align:(min 16 (max 1 align)) in
          let t = fresh c Gpr in
          emit c (Lea (t, slot_addr s));
          round_up_ptr c t align;
          if align <= 16 then Hashtbl.replace c.frame (F.resolve c.fn (res 0)) s;
          set t
      | _ ->
          if align > 0x1000 then unsupported "alloca with align %d" align;
          (* Salloc returns a 16-aligned block; see above for larger alignments *)
          let t = copy c (use c n) in
          emit c (Alui (Add, t, 15));
          emit c (Alui (And, t, -16));
          if align > 16 then emit c (Alui (Add, t, align - 16));
          let d = fresh c Gpr in
          emit c (Salloc (d, t));
          round_up_ptr c d align;
          set d)
  | Load (_, p) when Hashtbl.mem c.cells (F.resolve c.fn p) ->
      set (copy c (fst (Hashtbl.find c.cells (F.resolve c.fn p))))
  | Store (_, v, p) when Hashtbl.mem c.cells (F.resolve c.fn p) ->
      emit c (Copy (fst (Hashtbl.find c.cells (F.resolve c.fn p)), use c v))
  | Load (_, p) ->
      let t = rty () in
      let addr = addr_of c p in
      let r = fresh c (cls_of_ty t) in
      if cls_of_ty t = Xmm then emit c (Fload (is_dbl t, r, addr))
      else emit c (Load (mem_size t, false, r, addr));
      set r
  | Store (mem, v, p) ->
      let t = ty c v in
      let size = mem_size t in
      let rv = use c v and addr = addr_of c p in
      if mem.order = Seq_cst then begin
        let x = if cls_of_ty t = Xmm then to_gpr c (is_dbl t) rv else copy c rv in
        emit c (Xchg (size, addr, x))
      end
      else if cls_of_ty t = Xmm then emit c (Fstore (is_dbl t, addr, rv))
      else emit c (Store (size, addr, rv))
  | Ptradd (_, p, idx, scale, disp) -> set (ptradd c p idx scale disp)
  | Memcpy (_, d, s, n) ->
      let rd = use c d and rs = use c s and rn = use c n in
      emit c (Copy (G Rdi, rd));
      emit c (Copy (G Rsi, rs));
      emit c (Copy (G Rcx, rn));
      emit c Rep_movsb
  | Memmove (_, d, s, n) -> memmove c d s n
  | Memset (_, d, b, n) ->
      let rd = use c d and rb = use c b and rn = use c n in
      emit c (Copy (G Rdi, rd));
      emit c (Copy (G Rax, rb));
      emit c (Copy (G Rcx, rn));
      emit c Rep_stosb
  | Rmw (op, _, p, v) -> set (rmw c op (ty c v) p v)
  | Cmpxchg { p; expected; desired; _ } ->
      let t = ty c expected in
      if cls_of_ty t <> Gpr then unsupported "cmpxchg of %s" (F.ty_to_string t);
      let rp = use c p and re = use c expected and rd = use c desired in
      emit c (Copy (G Rax, re));
      emit c (Cmpxchg (mem_size t, at_reg rp, rd));
      let ok = setcc c E in
      def c (res 0) (copy c (G Rax));
      def c (res 1) ok
  | Fence o -> if o = Seq_cst then emit c Mfence
  | Call { sig_; callee; args; va_aggs; _ } -> call c ~sig_ ~callee ~args ~va_aggs ir.results
  | Syscall args ->
      let regs = [| G Rax; G Rdi; G Rsi; G Rdx; G R10; G R8; G R9 |] in
      if Array.length args > 7 then unsupported "syscall with %d operands" (Array.length args);
      let rs = Array.map (use c) args in
      let used = Array.to_list (Array.mapi (fun i r -> emit c (Copy (regs.(i), r)); regs.(i)) rs) in
      emit c (Syscall used);
      set (copy c (G Rax))
  | Trap -> emit c Ud2
  | Va_start p -> va_start c p
  | Va_arg p -> set (va_arg c (rty ()) p)
  | Va_arg_agg (abi, p) -> set (va_arg_agg c abi p)
  | Stack_save ->
      let d = fresh c Gpr in
      emit c (Spget d);
      set d
  | Stack_restore p -> emit c (Spset (use c p))
  | Extract _ | Insert _ | Shuffle _ | Splat _ | Reduce _ | Masked_load _ | Masked_store _ | Compress _
  | Expand _ ->
      unsupported "vector operations"

(* parallel copies of an edge's arguments into the target's parameters *)
let edge c (t : F.target) =
  let ps = (F.block c.fn t.dest).params in
  let tmps = Array.map (fun a -> copy c (use c a)) t.args in
  Array.iteri (fun i p -> emit c (Copy (vreg c p, tmps.(i)))) ps

(* the block an edge jumps to: the target itself, or a new block holding
   the argument copies *)
let edge_block c (t : F.target) =
  if Array.length t.args = 0 then t.dest
  else begin
    let saved = c.cur in
    let b = new_block c.mf in
    c.cur <- b;
    edge c t;
    set_term c (Jmp t.dest);
    c.cur <- saved;
    b
  end

let term c (t : F.term) =
  match t with
  | Jump tg ->
      edge c tg;
      set_term c (Jmp tg.dest)
  | Br (v, tt, te, _) -> (
      match F.int_value c.fn v with
      | Some k ->
          let tg = if k <> 0L then tt else te in
          edge c tg;
          set_term c (Jmp tg.dest)
      | None ->
          let rv = use c v in
          let bt = edge_block c tt and be = edge_block c te in
          emit c (Test (rv, rv));
          set_term c (Jcc (Ne, bt, be)))
  | Switch (v, default, cases) ->
      let rv = use c v in
      let w = width (ty c v) in
      Array.iter
        (fun (limbs, tg) ->
          let k = mask w limbs.(0) in
          let bt = edge_block c tg in
          let next = new_block c.mf in
          if fits32 k then emit c (Alui (Cmp, rv, Int64.to_int k))
          else begin
            let r = movi c k in
            emit c (Alu (Cmp, rv, r))
          end;
          set_term c (Jcc (E, bt, next));
          c.cur <- next)
        cases;
      edge c default;
      set_term c (Jmp default.dest)
  | Ret vals -> ret c vals
  | Unreachable -> set_term c Halt
  | Indirect_br (v, targets) ->
      Array.iter
        (fun b -> if Array.length (F.block c.fn b).params > 0 then unsupported "indirect_br to a block with parameters")
        targets;
      emit c (Copy (G R11, use c v));
      set_term c (Jmpr (G R11, Array.to_list targets))

(* ---------- entry ---------- *)

(* the per-module symbol facts selection needs *)
type env = {
  local_syms : (string, unit) Hashtbl.t;
  label_funcs : (string, unit) Hashtbl.t;
  tls_syms : (string, unit) Hashtbl.t;  (** thread_local globals, defined or declared *)
  blk_refs : (string * int, unit) Hashtbl.t;  (** blocks whose address is taken *)
}

let env (m : F.modul) =
  let local_syms = Hashtbl.create 64 and label_funcs = Hashtbl.create 64 in
  let note name defined linkage vis =
    if defined && (linkage = F.Local || vis <> F.Default) then Hashtbl.replace local_syms name ()
  in
  F.Vec.iter
    (fun (f : F.func) ->
      note f.name f.defined f.linkage f.visibility;
      if f.defined && f.linkage <> F.Weak then Hashtbl.replace label_funcs f.name ())
    m.funcs;
  let tls_syms = Hashtbl.create 8 in
  F.Vec.iter
    (fun (g : F.global) ->
      note g.gname g.gdefined g.glinkage g.gvisibility;
      if g.thread_local then Hashtbl.replace tls_syms g.gname ())
    m.globals;
  F.Vec.iter (fun (a : F.alias) -> note a.aname true a.alinkage a.avisibility) m.aliases;
  (* an alias of a thread-local variable is thread-local too *)
  F.Vec.iter (fun (a : F.alias) -> if Hashtbl.mem tls_syms a.target then Hashtbl.replace tls_syms a.aname ()) m.aliases;
  { local_syms; label_funcs; tls_syms; blk_refs = Hashtbl.create 8 }

(* Allocas that can live in a virtual register instead of memory: static
   (entry block, constant size, align <= 16), and only ever accessed by
   plain (non-volatile, non-atomic) loads and stores of one scalar type of
   exactly the alloca's size, with the alloca as the pointer operand. The
   address is used nowhere else, so nothing else can see the memory, and a
   load is a copy of the variable, a store a copy into it. Mir does not need
   SSA (a virtual register may be written many times), so there is nothing
   more to it: no phis, no dominance. A variable that some path can read
   before writing it starts at zero, so that every path defines it (reading
   an uninitialized C local is undefined, so any value is right); the others
   are not initialized, so their live range starts at their first store and
   does not cover the whole function. Returns (value, type, needs zero);
   [select] makes the registers. *)
let promotable_allocas (fn : F.func) =
  let cand : (int, F.ty option) Hashtbl.t = Hashtbl.create 16 in
  let size = Hashtbl.create 16 in
  let blk = F.Vec.get fn.blocks F.entry in
  F.Vec.iter
    (fun i ->
      let ir = F.inst fn i in
      match ir.op with
      | Alloca (n, align) when align <= 16 -> (
          match F.int_value fn n with
          | Some k when Int64.compare k 1L >= 0 && Int64.compare k 8L <= 0 ->
              Hashtbl.replace cand ir.results.(0) None;
              Hashtbl.replace size ir.results.(0) (Int64.to_int k)
          | _ -> ())
      | _ -> ())
    blk.insts;
  if Hashtbl.length cand > 0 then begin
    let r = F.resolve fn in
    let bad v = Hashtbl.remove cand (r v) in
    let access p t =
      let p = r p in
      match Hashtbl.find_opt cand p with
      | None -> ()
      | Some prev ->
          let scalar = match t with F.Int w -> w <= 64 | F.Ptr _ | F.F32 | F.F64 -> true | _ -> false in
          if (not scalar) || F.size_of t <> Hashtbl.find size p || (prev <> None && prev <> Some t) then
            Hashtbl.remove cand p
          else Hashtbl.replace cand p (Some t)
    in
    F.Vec.iter
      (fun (b : F.block_rec) ->
        F.Vec.iter
          (fun i ->
            let ir = F.inst fn i in
            match ir.op with
            | Load (m, p) when (not m.volatile) && m.order = Plain -> access p (F.ty_of fn ir.results.(0))
            | Store (m, v, p) when (not m.volatile) && m.order = Plain ->
                bad v;
                access p (F.ty_of fn v)
            | Dbg_value _ -> ()
            | op -> F.iter_op_uses bad op)
          b.insts;
        F.iter_term_uses bad b.term)
      fn.blocks
  end;
  let cells = Hashtbl.fold (fun v t acc -> match t with Some t -> (v, t) :: acc | None -> acc) cand [] in
  if cells = [] then []
  else begin
    (* per cell: the blocks that load it before storing it (upward exposed)
       and the blocks that store it *)
    let r = F.resolve fn in
    let ue = Hashtbl.create 16 and defs = Hashtbl.create 16 in
    let nb = F.Vec.length fn.blocks in
    let preds = Array.make nb [] in
    F.Vec.iteri
      (fun b (blk : F.block_rec) ->
        List.iter (fun s -> preds.(s) <- b :: preds.(s)) (F.successors blk.term);
        let seen = Hashtbl.create 8 in
        F.Vec.iter
          (fun i ->
            match (F.inst fn i).op with
            | Load (_, p) when not (Hashtbl.mem seen (r p)) ->
                Hashtbl.replace seen (r p) ();
                Hashtbl.add ue (r p) b
            | Store (_, _, p) when not (Hashtbl.mem seen (r p)) ->
                Hashtbl.replace seen (r p) ();
                Hashtbl.add defs (r p) b
            | Store (_, _, p) -> Hashtbl.add defs (r p) b
            | _ -> ())
          blk.insts)
      fn.blocks;
    (* live at the entry: reachable backwards from an upward-exposed load
       without passing a block that stores it *)
    let mark = Array.make nb (-1) and stores = Array.make nb (-1) in
    List.mapi
      (fun k (v, t) ->
        List.iter (fun b -> stores.(b) <- k) (Hashtbl.find_all defs v);
        let stack = ref (Hashtbl.find_all ue v) in
        List.iter (fun b -> mark.(b) <- k) !stack;
        let live = ref false in
        while !stack <> [] && not !live do
          let b = List.hd !stack in
          stack := List.tl !stack;
          if b = F.entry then live := true
          else
            List.iter
              (fun p ->
                if mark.(p) <> k && stores.(p) <> k then begin
                  mark.(p) <- k;
                  stack := p :: !stack
                end)
              preds.(b)
        done;
        (v, t, !live))
      cells
  end

let select ~pic (e : env) (fn : F.func) =
  let mf = Mir.create fn.name in
  for _ = 1 to F.num_blocks fn do
    ignore (new_block mf)
  done;
  let c =
    { fn; mf; pic; local_syms = e.local_syms; label_funcs = e.label_funcs; tls_syms = e.tls_syms; vals = Hashtbl.create 256;
      cur = 0; fir_block = 0; sret = None; va = None; blk_refs = e.blk_refs;
      frame = Hashtbl.create 16;
      cells =
        (let h = Hashtbl.create 16 in
         List.iter (fun (v, t, z) -> Hashtbl.replace h v (new_vreg mf (cls_of_ty t), z)) (promotable_allocas fn);
         h) }
  in
  params c;
  (* A Loc marker before the code of each Fir instruction whose location
     differs from the previous marker in the same Mir block. The markers
     are there with or without -g, so debug info never changes the code. *)
  let last = ref (-1, 0) in
  let mark loc =
    if loc <> 0 && !last <> (c.cur, loc) then begin
      emit c (Loc loc);
      last := (c.cur, loc)
    end
  in
  F.Vec.iteri
    (fun b (blk : F.block_rec) ->
      c.cur <- b;
      c.fir_block <- b;
      F.Vec.iter (fun i -> mark (F.inst fn i).loc; inst c i) blk.insts;
      mark blk.term_loc;
      term c blk.term)
    fn.blocks;
  Dynarray.iter (fun (b : block) -> b.code <- List.rev b.code) mf.blocks;
  mf
