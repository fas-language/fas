(* Runtime helpers for Fir_wide, written in Fir text (only i64 and narrower,
   so they need no legalization themselves). Fir_wide parses the ones a
   module calls and appends them with local linkage.

   Limb arrays are little-endian i64s, [k] of them, in memory the caller
   owns. The helpers favour obviously-correct code over speed: division is
   one bit per iteration (after skipping leading zero limbs, with a native
   divide when both operands fit in 64 bits). *)

(* q, r <- a / b, a % b, all unsigned [k]-limb numbers. b must not be 0
   (Fir makes that UB). Bit-serial restoring division: for each bit of a,
   from the top, r = 2r + bit; if r >= b (or the doubling carried out of the
   top limb), r -= b and the quotient bit is set. *)
let udivmod =
  {|
func @__fir_udivmod (ptr, ptr, ptr, ptr, i64) -> () local {
^e(%q: ptr, %r: ptr, %a: ptr, %b: ptr, %k: i64):
  %bytes = mul i64 %k, i64 8
  memset %q, i8 0, %bytes
  memset %r, i8 0, %bytes
  jump ^atop(%k)
; ta = number of limbs of a up to its top nonzero one
^atop(%ta: i64):
  %ta0 = icmp eq i1 %ta, i64 0
  br %ta0, ^done, ^atop1
^atop1:
  %tam = sub i64 %ta, i64 1
  %pat = ptradd ptr %a, %tam, 8, 0
  %avt = load i64 %pat, align 8
  %avz = icmp eq i1 %avt, i64 0
  br %avz, ^atop(%tam), ^btop(%k)
^btop(%tb: i64):
  %tb0 = icmp eq i1 %tb, i64 0
  br %tb0, ^slow, ^btop1
^btop1:
  %tbm = sub i64 %tb, i64 1
  %pbt = ptradd ptr %b, %tbm, 8, 0
  %bvt = load i64 %pbt, align 8
  %bvz = icmp eq i1 %bvt, i64 0
  br %bvz, ^btop(%tbm), ^fast
; a < b when a has fewer limbs: q = 0, r = a; both in one limb: divide
^fast:
  %fewer = icmp ult i1 %ta, %tb
  br %fewer, ^copya, ^fast1
^copya:
  %abytes = mul i64 %ta, i64 8
  memcpy %r, %a, %abytes
  jump ^done
^fast1:
  %one = icmp eq i1 %ta, i64 1
  br %one, ^div64, ^slow
^div64:
  %a0 = load i64 %a, align 8
  %b0 = load i64 %b, align 8
  %q0 = udiv i64 %a0, %b0
  %r0 = urem i64 %a0, %b0
  store %q0, %q, align 8
  store %r0, %r, align 8
  jump ^done
^slow:
  %nb = mul i64 %ta, i64 64
  jump ^bit(%nb)
; i = bits of a still to bring down; the current one is bit i - 1
^bit(%i: i64):
  %fin = icmp eq i1 %i, i64 0
  br %fin, ^done, ^step
^step:
  %bi = sub i64 %i, i64 1
  %li = lshr i64 %bi, i64 6
  %sh = and i64 %bi, i64 63
  %pal = ptradd ptr %a, %li, 8, 0
  %al = load i64 %pal, align 8
  %bit0 = lshr i64 %al, %sh
  %bit = and i64 %bit0, i64 1
  jump ^shl(i64 0, %bit)
; r = 2r + bit, limb j, carry in %cin
^shl(%j: i64, %cin: i64):
  %sd = icmp eq i1 %j, %k
  br %sd, ^cmp0(%cin), ^shl1
^shl1:
  %prj = ptradd ptr %r, %j, 8, 0
  %rj = load i64 %prj, align 8
  %rs = shl i64 %rj, i64 1
  %rn = or i64 %rs, %cin
  store %rn, %prj, align 8
  %co = lshr i64 %rj, i64 63
  %j1 = add i64 %j, i64 1
  jump ^shl(%j1, %co)
; subtract when the doubling carried out, or r >= b (compared from the top)
^cmp0(%over: i64):
  %ov = icmp ne i1 %over, i64 0
  br %ov, ^sub(i64 0, i64 0), ^cmp(%k)
^cmp(%m: i64):
  %ce = icmp eq i1 %m, i64 0
  br %ce, ^sub(i64 0, i64 0), ^cmp1
^cmp1:
  %mm = sub i64 %m, i64 1
  %prm = ptradd ptr %r, %mm, 8, 0
  %rm = load i64 %prm, align 8
  %pbm = ptradd ptr %b, %mm, 8, 0
  %bm = load i64 %pbm, align 8
  %lt = icmp ult i1 %rm, %bm
  br %lt, ^bit(%bi), ^cmp2
^cmp2:
  %gt = icmp ugt i1 %rm, %bm
  br %gt, ^sub(i64 0, i64 0), ^cmp(%mm)
; r -= b, limb j2, borrow in %bw
^sub(%j2: i64, %bw: i64):
  %sdn = icmp eq i1 %j2, %k
  br %sdn, ^setq, ^sub1
^sub1:
  %prj2 = ptradd ptr %r, %j2, 8, 0
  %r2 = load i64 %prj2, align 8
  %pbj = ptradd ptr %b, %j2, 8, 0
  %b2 = load i64 %pbj, align 8
  %d1 = sub i64 %r2, %b2
  %w1 = icmp ult i1 %r2, %b2
  %d2 = sub i64 %d1, %bw
  %w2 = icmp ult i1 %d1, %bw
  store %d2, %prj2, align 8
  %w = or i1 %w1, %w2
  %bw1 = zext i64 %w
  %j3 = add i64 %j2, i64 1
  jump ^sub(%j3, %bw1)
^setq:
  %pq = ptradd ptr %q, %li, 8, 0
  %qv = load i64 %pq, align 8
  %qbit = shl i64 i64 1, %sh
  %qn = or i64 %qv, %qbit
  store %qn, %pq, align 8
  jump ^bit(%bi)
^done:
  ret
}
|}

(* The top 64 significant bits of the unsigned [k]-limb number at a, with
   every lower nonzero bit ORed into bit 0 ("sticky"), and in *e the power of
   two they are scaled by: a ~ result * 2^e. Converting the result to a
   float rounds exactly as converting a would (64 bits leave more than two
   guard bits below any float's precision), and the scaling is exact. 0
   gives 0. *)
let wide_top =
  {|
func @__fir_wide_top (ptr, i64, ptr) -> (i64) local {
^e(%a: ptr, %k: i64, %eo: ptr):
  store i64 0, %eo, align 8
  jump ^top(%k)
^top(%t: i64):
  %t0 = icmp eq i1 %t, i64 0
  br %t0, ^zero, ^top1
^zero:
  ret i64 0
^top1:
  %tm = sub i64 %t, i64 1
  %pt = ptradd ptr %a, %tm, 8, 0
  %hi = load i64 %pt, align 8
  %hz = icmp eq i1 %hi, i64 0
  br %hz, ^top(%tm), ^found
^found:
  %single = icmp eq i1 %tm, i64 0
  br %single, ^one, ^many
^one:
  ret %hi
^many:
  %t2 = sub i64 %tm, i64 1
  %pl = ptradd ptr %a, %t2, 8, 0
  %lo = load i64 %pl, align 8
  %s = clz i64 %hi
  %top = fshl i64 %hi, %lo, %s
  %rest = shl i64 %lo, %s
  %restnz = icmp ne i1 %rest, i64 0
  %stick0 = zext i64 %restnz
  jump ^low(%t2, %stick0)
; OR the limbs below the two top ones into the sticky bit
^low(%j: i64, %st: i64):
  %j0 = icmp eq i1 %j, i64 0
  br %j0, ^fin, ^low1
^low1:
  %jm = sub i64 %j, i64 1
  %pj = ptradd ptr %a, %jm, 8, 0
  %lj = load i64 %pj, align 8
  %nz = icmp ne i1 %lj, i64 0
  %nzw = zext i64 %nz
  %st1 = or i64 %st, %nzw
  jump ^low(%jm, %st1)
^fin:
  %m = or i64 %top, %st
  %e64 = mul i64 %tm, i64 64
  %e = sub i64 %e64, %s
  store %e, %eo, align 8
  ret %m
}
|}

(* Unsigned [k]-limb number -> f64 / f32, round to nearest even: the top
   bits convert (one rounding), then a power-of-two scale (exact; past the
   exponent range the result is +inf, as the true value is too) *)
let utof64 =
  {|
func @__fir_utof64 (ptr, i64) -> (f64) local {
^e(%a: ptr, %k: i64):
  %eo = alloca ptr i64 8, align 8
  %m = call (ptr, i64, ptr) -> (i64) @__fir_wide_top(%a, %k, %eo)
  %e = load i64 %eo, align 8
  %f = uitofp f64 %m
  %big = icmp ugt i1 %e, i64 1023
  %be = add i64 %e, i64 1023
  %bits = shl i64 %be, i64 52
  %bits2 = select i64 %big, i64 0x7ff0000000000000, %bits
  %scale = bitcast f64 %bits2
  %r = fmul f64 %f, %scale
  ret %r
}
|}

let utof32 =
  {|
func @__fir_utof32 (ptr, i64) -> (f32) local {
^e(%a: ptr, %k: i64):
  %eo = alloca ptr i64 8, align 8
  %m = call (ptr, i64, ptr) -> (i64) @__fir_wide_top(%a, %k, %eo)
  %e = load i64 %eo, align 8
  %f = uitofp f32 %m
  %big = icmp ugt i1 %e, i64 127
  %be = add i64 %e, i64 127
  %bits = shl i64 %be, i64 23
  %bits2 = select i64 %big, i64 0x7f800000, %bits
  %bits32 = trunc i32 %bits2
  %scale = bitcast f32 %bits32
  %r = fmul f32 %f, %scale
  ret %r
}
|}

(* out[0..k) <- trunc(x) for a non-negative x. Below 2^64 one conversion;
   above, the 53-bit significand shifted into place. Any other input (NaN,
   infinity, too big) leaves some value but never writes outside out. *)
let f64tou =
  {|
func @__fir_f64tou (ptr, f64, i64) -> () local {
^e(%out: ptr, %x: f64, %k: i64):
  %bytes = mul i64 %k, i64 8
  memset %out, i8 0, %bytes
  %small = fcmp olt i1 %x, f64 0x43f0000000000000
  br %small, ^small, ^big
^small:
  %u = fptoui_sat i64 %x
  store %u, %out, align 8
  ret
^big:
  %bits = bitcast i64 %x
  %ex = lshr i64 %bits, i64 52
  %ex1 = and i64 %ex, i64 0x7ff
  %sh = sub i64 %ex1, i64 1075
  %frac = and i64 %bits, i64 0xfffffffffffff
  %m = or i64 %frac, i64 0x10000000000000
  %q = lshr i64 %sh, i64 6
  %r = and i64 %sh, i64 63
  %lo = shl i64 %m, %r
  %hi = fshl i64 i64 0, %m, %r
  %in0 = icmp ult i1 %q, %k
  br %in0, ^st0, ^done
^st0:
  %p0 = ptradd ptr %out, %q, 8, 0
  store %lo, %p0, align 8
  %q1 = add i64 %q, i64 1
  %in1 = icmp ult i1 %q1, %k
  br %in1, ^st1, ^done
^st1:
  %p1 = ptradd ptr %out, %q1, 8, 0
  store %hi, %p1, align 8
  jump ^done
^done:
  ret
}
|}

(* name -> (text, helpers it calls) *)
let all =
  [
    ("__fir_udivmod", (udivmod, []));
    ("__fir_wide_top", (wide_top, []));
    ("__fir_utof64", (utof64, [ "__fir_wide_top" ]));
    ("__fir_utof32", (utof32, [ "__fir_wide_top" ]));
    ("__fir_f64tou", (f64tou, []));
  ]
