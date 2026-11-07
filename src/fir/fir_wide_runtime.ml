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

(* r <- a * b mod 2^(64k), all [k]-limb numbers, r apart from a and b:
   schoolbook, row i adds a_i * b into r from limb i up, dropping what
   passes limb k. Each step r_(i+j) + lo + carry fits: their sum with hi
   is at most 2^128 - 1, so the new carry hi + c1 + c2 cannot wrap. Fir_wide
   calls it for products of more limbs than it multiplies inline. *)
let mul =
  {|
func @__fir_mul (ptr, ptr, ptr, i64) -> () local {
^e(%r: ptr, %a: ptr, %b: ptr, %k: i64):
  %bytes = mul i64 %k, i64 8
  memset %r, i8 0, %bytes
  jump ^oi(i64 0)
^oi(%i: i64):
  %id = icmp eq i1 %i, %k
  br %id, ^done, ^ob
^ob:
  %pa = ptradd ptr %a, %i, 8, 0
  %ai = load i64 %pa, align 8
  %lim = sub i64 %k, %i
  jump ^ij(i64 0, i64 0)
^ij(%j: i64, %carry: i64):
  %jd = icmp eq i1 %j, %lim
  br %jd, ^onext, ^ib
^ib:
  %pb = ptradd ptr %b, %j, 8, 0
  %bj = load i64 %pb, align 8
  %lo = mul i64 %ai, %bj
  %hi = umulh i64 %ai, %bj
  %ij = add i64 %i, %j
  %pr = ptradd ptr %r, %ij, 8, 0
  %rv = load i64 %pr, align 8
  %s1 = add i64 %rv, %lo
  %c1 = icmp ult i1 %s1, %rv
  %s2 = add i64 %s1, %carry
  %c2 = icmp ult i1 %s2, %s1
  store %s2, %pr, align 8
  %c1w = zext i64 %c1
  %c2w = zext i64 %c2
  %h1 = add i64 %hi, %c1w
  %h2 = add i64 %h1, %c2w
  %j1 = add i64 %j, i64 1
  jump ^ij(%j1, %h2)
^onext:
  %i1 = add i64 %i, i64 1
  jump ^oi(%i1)
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

(* For f80, whose 64-bit significand is as wide as __fir_wide_top's result,
   a sticky bit folded into bit 0 would be kept, not rounded on: the top 64
   significant bits T (no sticky), in *go the next 64 bits G with every lower
   nonzero bit ORed into bit 0, and in *eo the e with
   a ~ (T + G * 2^-64) * 2^e. 0 gives T = G = e = 0. *)
let wide_top2 =
  {|
func @__fir_wide_top2 (ptr, i64, ptr, ptr) -> (i64) local {
^e(%a: ptr, %k: i64, %eo: ptr, %go: ptr):
  store i64 0, %eo, align 8
  store i64 0, %go, align 8
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
  %s = clz i64 %hi
  %has1 = icmp ugt i1 %tm, i64 0
  br %has1, ^l1, ^fin(i64 0, i64 0, i64 0)
^l1:
  %t1 = sub i64 %tm, i64 1
  %p1 = ptradd ptr %a, %t1, 8, 0
  %mid = load i64 %p1, align 8
  %has2 = icmp ugt i1 %t1, i64 0
  br %has2, ^l2, ^fin(%mid, i64 0, i64 0)
^l2:
  %t2 = sub i64 %t1, i64 1
  %p2 = ptradd ptr %a, %t2, 8, 0
  %lo = load i64 %p2, align 8
  jump ^low(%t2, i64 0)
; OR the limbs below the three top ones into the sticky bit
^low(%j: i64, %st: i64):
  %j0 = icmp eq i1 %j, i64 0
  br %j0, ^fin(%mid, %lo, %st), ^low1
^low1:
  %jm = sub i64 %j, i64 1
  %pj = ptradd ptr %a, %jm, 8, 0
  %lj = load i64 %pj, align 8
  %nz = icmp ne i1 %lj, i64 0
  %nzw = zext i64 %nz
  %st1 = or i64 %st, %nzw
  jump ^low(%jm, %st1)
^fin(%m: i64, %l: i64, %sk: i64):
  %top = fshl i64 %hi, %m, %s
  %g0 = fshl i64 %m, %l, %s
  %rest = shl i64 %l, %s
  %restnz = icmp ne i1 %rest, i64 0
  %rnz = zext i64 %restnz
  %sticky = or i64 %sk, %rnz
  %g = or i64 %g0, %sticky
  store %g, %go, align 8
  %e64 = mul i64 %tm, i64 64
  %e = sub i64 %e64, %s
  store %e, %eo, align 8
  ret %top
}
|}

(* Unsigned [k]-limb number -> f80, round to nearest even: T + G * 2^-64
   is one rounding of exact operands (G's bit 63 is the round bit, the rest
   with the sticky bit decide ties), then the exact scale by 2^e built from
   its bits (+inf past the exponent range) *)
let utof80 =
  {|
func @__fir_utof80 (ptr, i64) -> (f80) local {
^e(%a: ptr, %k: i64):
  %eo = alloca ptr i64 8, align 8
  %go = alloca ptr i64 8, align 8
  %sc = alloca ptr i64 16, align 16
  %t = call (ptr, i64, ptr, ptr) -> (i64) @__fir_wide_top2(%a, %k, %eo, %go)
  %e = load i64 %eo, align 8
  %g = load i64 %go, align 8
  %ft = uitofp f80 %t
  %fg = uitofp f80 %g
  %fg2 = fmul f80 %fg, f80 0x3fbf8000000000000000
  %f = fadd f80 %ft, %fg2
  %big = icmp sgt i1 %e, i64 16383
  %be = add i64 %e, i64 16383
  %be2 = select i64 %big, i64 32767, %be
  %be16 = trunc i16 %be2
  store i64 0x8000000000000000, %sc, align 16
  %sc8 = ptradd ptr %sc, i64 0, 1, 8
  store %be16, %sc8, align 8
  %scale = load f80 %sc, align 16
  %r = fmul f80 %f, %scale
  ret %r
}
|}

(* out[0..k) <- trunc(x) for a non-negative f80 x, as __fir_f64tou: below
   2^64 one conversion; above, the 64-bit significand (explicit integer
   bit) shifted into place *)
let f80tou =
  {|
func @__fir_f80tou (ptr, f80, i64) -> () local {
^e(%out: ptr, %x: f80, %k: i64):
  %sc = alloca ptr i64 16, align 16
  %bytes = mul i64 %k, i64 8
  memset %out, i8 0, %bytes
  %small = fcmp olt i1 %x, f80 0x403f8000000000000000
  br %small, ^small, ^big
^small:
  %u = fptoui_sat i64 %x
  store %u, %out, align 8
  ret
^big:
  store %x, %sc, align 16
  %m = load i64 %sc, align 16
  %sc8 = ptradd ptr %sc, i64 0, 1, 8
  %exw = load i16 %sc8, align 8
  %ex = zext i64 %exw
  %ex1 = and i64 %ex, i64 0x7fff
  %sh = sub i64 %ex1, i64 16446
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

(* ---------- f16 (IEEE binary16), held as its i16 bits ---------- *)

(* f16 -> f32, exact: normal numbers rebias the exponent, subnormals are
   m * 2^-24 (an exact product), infinities and NaNs keep their payload
   (the quiet bit lands on the f32 quiet bit) *)
let h2f =
  {|
func @__fir_h2f (i16) -> (f32) local {
^e(%h: i16):
  %x = zext i32 %h
  %s0 = and i32 %x, i32 0x8000
  %s = shl i32 %s0, i32 16
  %e1 = lshr i32 %x, i32 10
  %ex = and i32 %e1, i32 31
  %m = and i32 %x, i32 0x3ff
  %m13 = shl i32 %m, i32 13
  %eb = add i32 %ex, i32 112
  %ebs = shl i32 %eb, i32 23
  %nb = or i32 %ebs, %m13
  %special = or i32 %m13, i32 0x7f800000
  %isspec = icmp eq i1 %ex, i32 31
  %b1 = select i32 %isspec, %special, %nb
  %mf = uitofp f32 %m
  %sub = fmul f32 %mf, f32 0x33800000
  %subb = bitcast i32 %sub
  %iszero = icmp eq i1 %ex, i32 0
  %b2 = select i32 %iszero, %subb, %b1
  %b = or i32 %b2, %s
  %f = bitcast f32 %b
  ret %f
}
|}

(* sign (0 or 1) and the finite value mant * 2^e0 -> f16 bits, rounded to
   nearest even once: overflow gives infinity, tiny values subnormals or
   zero. The value is normalized to m * 2^(e - 63) with bit 63 of m set;
   the kept bits are m >> shift (53 for normal results, more below), the
   rest decide the rounding. A carry out of the significand bumps the
   exponent through the addition (up to infinity). *)
let round_h =
  {|
func @__fir_round_h (i64, i64, i64) -> (i16) local {
^e(%sign: i64, %mant: i64, %e0: i64):
  %sbit = shl i64 %sign, i64 15
  %zero = icmp eq i1 %mant, i64 0
  br %zero, ^z, ^nz
^z:
  %zs = trunc i16 %sbit
  ret %zs
^nz:
  %z = clz i64 %mant
  %m = shl i64 %mant, %z
  %e63 = add i64 %e0, i64 63
  %e = sub i64 %e63, %z
  %eb = add i64 %e, i64 15
  %inf = icmp sge i1 %eb, i64 31
  br %inf, ^inf, ^fin
^inf:
  %ib = or i64 %sbit, i64 0x7c00
  %is = trunc i16 %ib
  ret %is
^fin:
  %normal = icmp sge i1 %eb, i64 1
  %sub_sh = sub i64 i64 54, %eb
  %sh0 = select i64 %normal, i64 53, %sub_sh
  %far = icmp sgt i1 %sh0, i64 64
  %m1 = select i64 %far, i64 1, %m
  %sh = select i64 %far, i64 64, %sh0
  %all = icmp eq i1 %sh, i64 64
  %shm = and i64 %sh, i64 63
  %q0 = lshr i64 %m1, %shm
  %q = select i64 %all, i64 0, %q0
  %one = shl i64 i64 1, %shm
  %lowmask = sub i64 %one, i64 1
  %r0 = and i64 %m1, %lowmask
  %rem = select i64 %all, %m1, %r0
  %sh1 = sub i64 %sh, i64 1
  %half = shl i64 i64 1, %sh1
  %gt = icmp ugt i1 %rem, %half
  %eq = icmp eq i1 %rem, %half
  %odd = trunc i1 %q
  %tie = and i1 %eq, %odd
  %up = or i1 %gt, %tie
  %upw = zext i64 %up
  %qr = add i64 %q, %upw
  %em1 = sub i64 %eb, i64 1
  %base0 = shl i64 %em1, i64 10
  %base = select i64 %normal, %base0, i64 0
  %bits = add i64 %base, %qr
  %bs = or i64 %bits, %sbit
  %r = trunc i16 %bs
  ret %r
}
|}

(* f32 / f64 / f80 -> f16, one rounding: the fields go to __fir_round_h;
   infinities stay, NaNs become quiet NaNs keeping the top payload bits *)
let f32toh =
  {|
func @__fir_f32toh (f32) -> (i16) local {
^e(%x: f32):
  %b32 = bitcast i32 %x
  %b = zext i64 %b32
  %sign = lshr i64 %b, i64 31
  %e1 = lshr i64 %b, i64 23
  %ex = and i64 %e1, i64 0xff
  %fr = and i64 %b, i64 0x7fffff
  %special = icmp eq i1 %ex, i64 0xff
  br %special, ^nan_inf, ^finite
^nan_inf:
  %sb = shl i64 %sign, i64 15
  %pay = lshr i64 %fr, i64 13
  %isnan = icmp ne i1 %fr, i64 0
  %nanb = or i64 %pay, i64 0x7e00
  %v = select i64 %isnan, %nanb, i64 0x7c00
  %r = or i64 %v, %sb
  %rh = trunc i16 %r
  ret %rh
^finite:
  %sub = icmp eq i1 %ex, i64 0
  %mn = or i64 %fr, i64 0x800000
  %mant = select i64 %sub, %fr, %mn
  %en = sub i64 %ex, i64 150
  %e0 = select i64 %sub, i64 -149, %en
  %h = call (i64, i64, i64) -> (i16) @__fir_round_h(%sign, %mant, %e0)
  ret %h
}
|}

let f64toh =
  {|
func @__fir_f64toh (f64) -> (i16) local {
^e(%x: f64):
  %b = bitcast i64 %x
  %sign = lshr i64 %b, i64 63
  %e1 = lshr i64 %b, i64 52
  %ex = and i64 %e1, i64 0x7ff
  %fr = and i64 %b, i64 0xfffffffffffff
  %special = icmp eq i1 %ex, i64 0x7ff
  br %special, ^nan_inf, ^finite
^nan_inf:
  %sb = shl i64 %sign, i64 15
  %pay = lshr i64 %fr, i64 42
  %isnan = icmp ne i1 %fr, i64 0
  %nanb = or i64 %pay, i64 0x7e00
  %v = select i64 %isnan, %nanb, i64 0x7c00
  %r = or i64 %v, %sb
  %rh = trunc i16 %r
  ret %rh
^finite:
  %sub = icmp eq i1 %ex, i64 0
  %mn = or i64 %fr, i64 0x10000000000000
  %mant = select i64 %sub, %fr, %mn
  %en = sub i64 %ex, i64 1075
  %e0 = select i64 %sub, i64 -1074, %en
  %h = call (i64, i64, i64) -> (i16) @__fir_round_h(%sign, %mant, %e0)
  ret %h
}
|}

(* f80: the significand has an explicit integer bit; a zero exponent field
   scales as exponent 1 (denormals and pseudo-denormals alike) *)
let f80toh =
  {|
func @__fir_f80toh (f80) -> (i16) local {
^e(%x: f80):
  %sc = alloca ptr i64 16, align 16
  store %x, %sc, align 16
  %mant = load i64 %sc, align 16
  %sc8 = ptradd ptr %sc, i64 0, 1, 8
  %hw = load i16 %sc8, align 8
  %hi = zext i64 %hw
  %sign = lshr i64 %hi, i64 15
  %ex = and i64 %hi, i64 0x7fff
  %special = icmp eq i1 %ex, i64 0x7fff
  br %special, ^nan_inf, ^finite
^nan_inf:
  %sb = shl i64 %sign, i64 15
  %fr = shl i64 %mant, i64 1
  %isnan = icmp ne i1 %fr, i64 0
  %p0 = lshr i64 %mant, i64 53
  %pay = and i64 %p0, i64 0x3ff
  %nanb = or i64 %pay, i64 0x7e00
  %v = select i64 %isnan, %nanb, i64 0x7c00
  %r = or i64 %v, %sb
  %rh = trunc i16 %r
  ret %rh
^finite:
  %sub = icmp eq i1 %ex, i64 0
  %ex1 = select i64 %sub, i64 1, %ex
  %e0 = sub i64 %ex1, i64 16446
  %h = call (i64, i64, i64) -> (i16) @__fir_round_h(%sign, %mant, %e0)
  ret %h
}
|}

(* fma on f16, correctly rounded: in f64 the product of two f16 values is
   exact, and the sum's rounding error is exact too (TwoSum). Rounding the
   sum to odd (when inexact and even, step one ulp towards the error) keeps
   the information the single final rounding to f16 needs: 53 bits are far
   more than f16's 11 + 2. *)
let fmah =
  {|
func @__fir_fmah (i16, i16, i16) -> (i16) local {
^e(%a: i16, %b: i16, %c: i16):
  %af = call (i16) -> (f32) @__fir_h2f(%a)
  %bf = call (i16) -> (f32) @__fir_h2f(%b)
  %cf = call (i16) -> (f32) @__fir_h2f(%c)
  %ad = fpext f64 %af
  %bd = fpext f64 %bf
  %cd = fpext f64 %cf
  %p = fmul f64 %ad, %bd
  %s = fadd f64 %p, %cd
  %bb = fsub f64 %s, %p
  %t1 = fsub f64 %s, %bb
  %t2 = fsub f64 %p, %t1
  %t3 = fsub f64 %cd, %bb
  %err = fadd f64 %t2, %t3
  %inexact = fcmp one i1 %err, f64 0
  %sb = bitcast i64 %s
  %low = trunc i1 %sb
  %even = xor i1 %low, i1 1
  %adj = and i1 %inexact, %even
  %errneg = fcmp olt i1 %err, f64 0
  %sneg = fcmp olt i1 %s, f64 0
  %same = xor i1 %errneg, %sneg
  %away = xor i1 %same, i1 1
  %step = select i64 %away, i64 1, i64 -1
  %sb1 = add i64 %sb, %step
  %sb2 = select i64 %adj, %sb1, %sb
  %so = bitcast f64 %sb2
  %h = call (f64) -> (i16) @__fir_f64toh(%so)
  ret %h
}
|}

(* atomic_rmw fadd (sub = 0) / fsub (sub <> 0) on f16 bits at p: a
   seq_cst compare-and-swap loop (at least as strong as any ordering the
   instruction asked for); the result is the old value *)
let rmwh =
  {|
func @__fir_rmwh (ptr, i16, i64) -> (i16) local {
^e(%p: ptr, %v: i16, %sub: i64):
  %o0 = load i16 %p, align 2 seq_cst
  %b = call (i16) -> (f32) @__fir_h2f(%v)
  %issub = icmp ne i1 %sub, i64 0
  jump ^l(%o0)
^l(%old: i16):
  %a = call (i16) -> (f32) @__fir_h2f(%old)
  %s = fadd f32 %a, %b
  %d = fsub f32 %a, %b
  %r = select f32 %issub, %d, %s
  %n = call (f32) -> (i16) @__fir_f32toh(%r)
  %got, %ok = cmpxchg (i16, i1) %p, %old, %n, align 2 seq_cst, fail seq_cst
  br %ok, ^x, ^l(%got)
^x:
  ret %old
}
|}

(* ---------- f80 fma ---------- *)

(* The 192-bit integer l2:l1:l0 times 2^(pos - 256), into the 12 limbs at
   buf: limbs 4..6 hold the integer part (the caller makes sure it fits),
   limbs 0..3 the 256 bits below it. A value shifted out entirely (pos < 0)
   leaves only a sticky 1 in limb 0 (callers pass a nonzero value). *)
let place3 =
  {|
func @__fir_place3 (ptr, i64, i64, i64, i64) -> () local {
^e(%buf: ptr, %l0: i64, %l1: i64, %l2: i64, %pos: i64):
  memset %buf, i8 0, i64 96
  %gone = icmp slt i1 %pos, i64 0
  br %gone, ^sticky, ^put
^sticky:
  store i64 1, %buf, align 8
  ret
^put:
  %q = lshr i64 %pos, i64 6
  %r = and i64 %pos, i64 63
  %w0 = shl i64 %l0, %r
  %w1 = fshl i64 %l1, %l0, %r
  %w2 = fshl i64 %l2, %l1, %r
  %w3 = fshl i64 i64 0, %l2, %r
  %p0 = ptradd ptr %buf, %q, 8, 0
  store %w0, %p0, align 8
  %p1 = ptradd ptr %buf, %q, 8, 8
  store %w1, %p1, align 8
  %p2 = ptradd ptr %buf, %q, 8, 16
  store %w2, %p2, align 8
  %p3 = ptradd ptr %buf, %q, 8, 24
  store %w3, %p3, align 8
  ret
}
|}

(* fma on f80, correctly rounded (round to nearest even), as musl's fma
   but with 64-bit significands: x = mx * 2^ex with mx in [2^63, 2^64)
   after normalizing subnormals, likewise y and z. The product P = mx * my
   (128 bits, exact) and mz go into one 192-bit window R * 2^W:
     d = ez - (ex + ey) >= 128: W = ez - 64; z is exact at bit 64, P lies
       below it and its lost bits become a sticky 1 at bit 0;
     0 <= d < 128: W = ex + ey; both are exact (z ends below bit 191);
     d < 0: W = ex + ey - 63; P is exact at bit 63, z below it, with a
       sticky 1 for its lost bits.
   Whenever bits are lost, the exact operand has zeros in bits 0..62, so
   the computed sum or difference is odd and lies within 1 of the true
   one: the same bits from bit 1 up, and a nonzero tail in both. The sum
   then rounds once to 64 bits (or, below the normal range, at 2^-16445).
   Zeros, infinities and NaNs go through x * y + z, which is exact for
   them; a zero z gives x * y (keeps the product's sign); an infinite z
   with a finite product is z. *)
let fma80 =
  {|
func @__fir_fma80 (f80, f80, f80) -> (f80) local {
^e(%x: f80, %y: f80, %z: f80):
  %sl = alloca ptr i64 48, align 16
  %ba = alloca ptr i64 96, align 8
  %bb = alloca ptr i64 96, align 8
  %bc = alloca ptr i64 96, align 8
  store %x, %sl, align 16
  %sly = ptradd ptr %sl, i64 0, 1, 16
  store %y, %sly, align 16
  %slz = ptradd ptr %sl, i64 0, 1, 32
  store %z, %slz, align 16
  %mx = load i64 %sl, align 16
  %my = load i64 %sly, align 16
  %mz = load i64 %slz, align 16
  %sx8 = ptradd ptr %sl, i64 0, 1, 8
  %sy8 = ptradd ptr %sl, i64 0, 1, 24
  %sz8 = ptradd ptr %sl, i64 0, 1, 40
  %hx16 = load i16 %sx8, align 8
  %hy16 = load i16 %sy8, align 8
  %hz16 = load i16 %sz8, align 8
  %hx = zext i64 %hx16
  %hy = zext i64 %hy16
  %hz = zext i64 %hz16
  %bx = and i64 %hx, i64 0x7fff
  %by = and i64 %hy, i64 0x7fff
  %bz = and i64 %hz, i64 0x7fff
  %xs = icmp eq i1 %bx, i64 0x7fff
  %ys = icmp eq i1 %by, i64 0x7fff
  %x0 = icmp eq i1 %mx, i64 0
  %y0 = icmp eq i1 %my, i64 0
  %zn = fcmp uno i1 %z, %z
  %o1 = or i1 %xs, %ys
  %o2 = or i1 %x0, %y0
  %o3 = or i1 %o1, %o2
  %simple = or i1 %o3, %zn
  br %simple, ^simple, ^finite
^simple:
  %sp = fmul f80 %x, %y
  %ss = fadd f80 %sp, %z
  ret %ss
^finite:
  %zs = icmp eq i1 %bz, i64 0x7fff
  br %zs, ^zinf, ^f2
^zinf:
  ret %z
^f2:
  %z0 = icmp eq i1 %mz, i64 0
  br %z0, ^zzero, ^go
^zzero:
  %zp = fmul f80 %x, %y
  ret %zp
^go:
  %nx = clz i64 %mx
  %ny = clz i64 %my
  %nz = clz i64 %mz
  %nmx = shl i64 %mx, %nx
  %nmy = shl i64 %my, %ny
  %nmz = shl i64 %mz, %nz
  %bx1 = umax i64 %bx, i64 1
  %by1 = umax i64 %by, i64 1
  %bz1 = umax i64 %bz, i64 1
  %ex0 = sub i64 %bx1, %nx
  %ey0 = sub i64 %by1, %ny
  %ez0 = sub i64 %bz1, %nz
  %ex = sub i64 %ex0, i64 16446
  %ey = sub i64 %ey0, i64 16446
  %ez = sub i64 %ez0, i64 16446
  %e = add i64 %ex, %ey
  %d = sub i64 %ez, %e
  %plo = mul i64 %nmx, %nmy
  %phi = umulh i64 %nmx, %nmy
  %dbig = icmp sge i1 %d, i64 128
  %dneg = icmp slt i1 %d, i64 0
  %wbig = sub i64 %ez, i64 64
  %wneg = sub i64 %e, i64 63
  %w1 = select i64 %dneg, %wneg, %e
  %w = select i64 %dbig, %wbig, %w1
  %posp = sub i64 %e, %w
  %posz = sub i64 %ez, %w
  %bpp = add i64 %posp, i64 256
  %bpz = add i64 %posz, i64 256
  call (ptr, i64, i64, i64, i64) -> () @__fir_place3(%ba, %plo, %phi, i64 0, %bpp)
  call (ptr, i64, i64, i64, i64) -> () @__fir_place3(%bb, %nmz, i64 0, i64 0, %bpz)
  %a0p = ptradd ptr %ba, i64 0, 1, 32
  %a1p = ptradd ptr %ba, i64 0, 1, 40
  %a2p = ptradd ptr %ba, i64 0, 1, 48
  %a0w = load i64 %a0p, align 8
  %a1 = load i64 %a1p, align 8
  %a2 = load i64 %a2p, align 8
  %b0p = ptradd ptr %bb, i64 0, 1, 32
  %b1p = ptradd ptr %bb, i64 0, 1, 40
  %b2p = ptradd ptr %bb, i64 0, 1, 48
  %b0w = load i64 %b0p, align 8
  %b1 = load i64 %b1p, align 8
  %b2 = load i64 %b2p, align 8
  %sta = call (ptr) -> (i64) @__fir_low4(%ba)
  %stb = call (ptr) -> (i64) @__fir_low4(%bb)
  %a0 = or i64 %a0w, %sta
  %b0 = or i64 %b0w, %stb
  %sgx = lshr i64 %hx, i64 15
  %sgy = lshr i64 %hy, i64 15
  %sgz = lshr i64 %hz, i64 15
  %sgp = xor i64 %sgx, %sgy
  %same = icmp eq i1 %sgp, %sgz
  ; sum
  %s0 = add i64 %a0, %b0
  %c0 = icmp ult i1 %s0, %a0
  %c0w = zext i64 %c0
  %s1a = add i64 %a1, %b1
  %c1a = icmp ult i1 %s1a, %a1
  %s1 = add i64 %s1a, %c0w
  %c1b = icmp ult i1 %s1, %s1a
  %c1 = or i1 %c1a, %c1b
  %c1w = zext i64 %c1
  %s2a = add i64 %a2, %b2
  %s2 = add i64 %s2a, %c1w
  ; difference P - Z, negated when negative
  %d0 = sub i64 %a0, %b0
  %r0b = icmp ult i1 %a0, %b0
  %r0w = zext i64 %r0b
  %d1a = sub i64 %a1, %b1
  %r1a = icmp ult i1 %a1, %b1
  %d1 = sub i64 %d1a, %r0w
  %r1b = icmp ult i1 %d1a, %r0w
  %r1 = or i1 %r1a, %r1b
  %r1w = zext i64 %r1
  %d2a = sub i64 %a2, %b2
  %d2 = sub i64 %d2a, %r1w
  %neg = icmp slt i1 %d2, i64 0
  %n0 = sub i64 i64 0, %d0
  %g0 = icmp ne i1 %d0, i64 0
  %g0w = zext i64 %g0
  %n1a = sub i64 i64 0, %d1
  %n1 = sub i64 %n1a, %g0w
  %d1nz = icmp ne i1 %d1, i64 0
  %g1 = or i1 %d1nz, %g0
  %g1w = zext i64 %g1
  %n2a = sub i64 i64 0, %d2
  %n2 = sub i64 %n2a, %g1w
  %e0 = select i64 %neg, %n0, %d0
  %e1 = select i64 %neg, %n1, %d1
  %e2 = select i64 %neg, %n2, %d2
  %negw = zext i64 %neg
  %sgd = xor i64 %sgp, %negw
  %r0 = select i64 %same, %s0, %e0
  %r1v = select i64 %same, %s1, %e1
  %r2 = select i64 %same, %s2, %e2
  %sg = select i64 %same, %sgp, %sgd
  %o01 = or i64 %r0, %r1v
  %oall = or i64 %o01, %r2
  %iszero = icmp eq i1 %oall, i64 0
  br %iszero, ^exactzero, ^round
^exactzero:
  ret f80 0x0
^round:
  ; L: the index of the top bit of R
  %c2 = clz i64 %r2
  %cc1 = clz i64 %r1v
  %cc0 = clz i64 %r0
  %l2 = sub i64 i64 191, %c2
  %l1 = sub i64 i64 127, %cc1
  %l0 = sub i64 i64 63, %cc0
  %h2 = icmp ne i1 %r2, i64 0
  %h1 = icmp ne i1 %r1v, i64 0
  %l10 = select i64 %h1, %l1, %l0
  %l = select i64 %h2, %l2, %l10
  ; drop k bits: 64 remain, or fewer at the subnormal quantum 2^-16445
  %kn = sub i64 %l, i64 63
  %ksub0 = sub i64 i64 -16445, %w
  %k = smax i64 %kn, %ksub0
  %bpc = sub i64 i64 256, %k
  call (ptr, i64, i64, i64, i64) -> () @__fir_place3(%bc, %r0, %r1v, %r2, %bpc)
  %mp = ptradd ptr %bc, i64 0, 1, 32
  %m = load i64 %mp, align 8
  %tp = ptradd ptr %bc, i64 0, 1, 24
  %t = load i64 %tp, align 8
  %below = call (ptr) -> (i64) @__fir_low3(%bc)
  %guard = lshr i64 %t, i64 63
  %t1 = shl i64 %t, i64 1
  %rest = or i64 %t1, %below
  %restnz = icmp ne i1 %rest, i64 0
  %mlow = and i64 %m, i64 1
  %modd = icmp ne i1 %mlow, i64 0
  %gset = icmp ne i1 %guard, i64 0
  %tie = or i1 %restnz, %modd
  %up = and i1 %gset, %tie
  %upw = zext i64 %up
  %m1 = add i64 %m, %upw
  %wrap0 = icmp eq i1 %m1, i64 0
  %wrap = and i1 %up, %wrap0
  %m2 = select i64 %wrap, i64 0x8000000000000000, %m1
  %wrapw = zext i64 %wrap
  %k1 = add i64 %k, %wrapw
  %q = add i64 %k1, %w
  ; the value is m2 * 2^q
  %top = icmp slt i1 %m2, i64 0
  %bq = add i64 %q, i64 16446
  %bexp = select i64 %top, %bq, i64 0
  %ovf = icmp sge i1 %bexp, i64 0x7fff
  %bexp2 = select i64 %ovf, i64 0x7fff, %bexp
  %m3 = select i64 %ovf, i64 0x8000000000000000, %m2
  %sgs = shl i64 %sg, i64 15
  %hw = or i64 %bexp2, %sgs
  %hw16 = trunc i16 %hw
  store %m3, %sl, align 16
  store %hw16, %sx8, align 8
  %res = load f80 %sl, align 16
  ret %res
}

; nonzero (1) or zero (0): limbs 0..3 of a place3 buffer
func @__fir_low4 (ptr) -> (i64) local {
^e(%b: ptr):
  %p3 = ptradd ptr %b, i64 0, 1, 24
  %v3 = load i64 %p3, align 8
  %r = call (ptr) -> (i64) @__fir_low3(%b)
  %v = or i64 %r, %v3
  %nz = icmp ne i1 %v, i64 0
  %w = zext i64 %nz
  ret %w
}

; the OR of limbs 0..2 of a place3 buffer
func @__fir_low3 (ptr) -> (i64) local {
^e(%b: ptr):
  %v0 = load i64 %b, align 8
  %p1 = ptradd ptr %b, i64 0, 1, 8
  %v1 = load i64 %p1, align 8
  %p2 = ptradd ptr %b, i64 0, 1, 16
  %v2 = load i64 %p2, align 8
  %a = or i64 %v0, %v1
  %r = or i64 %a, %v2
  ret %r
}
|}

(* name -> (text, helpers it calls) *)
let all =
  [
    ("__fir_udivmod", (udivmod, []));
    ("__fir_mul", (mul, []));
    ("__fir_wide_top", (wide_top, []));
    ("__fir_utof64", (utof64, [ "__fir_wide_top" ]));
    ("__fir_utof32", (utof32, [ "__fir_wide_top" ]));
    ("__fir_f64tou", (f64tou, []));
    ("__fir_wide_top2", (wide_top2, []));
    ("__fir_utof80", (utof80, [ "__fir_wide_top2" ]));
    ("__fir_f80tou", (f80tou, []));
    ("__fir_h2f", (h2f, []));
    ("__fir_round_h", (round_h, []));
    ("__fir_f32toh", (f32toh, [ "__fir_round_h" ]));
    ("__fir_f64toh", (f64toh, [ "__fir_round_h" ]));
    ("__fir_f80toh", (f80toh, [ "__fir_round_h" ]));
    ("__fir_fmah", (fmah, [ "__fir_h2f"; "__fir_f64toh" ]));
    ("__fir_rmwh", (rmwh, [ "__fir_h2f"; "__fir_f32toh" ]));
    ("__fir_place3", (place3, []));
    ("__fir_fma80", (fma80, [ "__fir_place3" ]));
  ]
