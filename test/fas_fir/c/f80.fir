; f80 (C long double, x87 extended precision) scalars: arithmetic, frem,
; fmin/fmax, sign operations, rounding, all fcmp predicates, conversions
; (plain and saturating) to and from integers, f32 and f64, select, loop
; parameters, loads and stores, the SysV ABI (f80 arguments in memory,
; results in st0, beyond the register arguments) and f80 through `...`
; both ways. Checked against clang by f80.c.
triple "x86_64-unknown-linux-gnu"
declare @c_mix (f80, f64, f80, i32) -> (f80) global
declare @c_vsum (i32, ...) -> (f80) global

func @f80_add (f80, f80) -> (f80) global {
^e(%a: f80, %b: f80):
  %r = fadd f80 %a, %b
  ret %r
}
func @f80_sub (f80, f80) -> (f80) global {
^e(%a: f80, %b: f80):
  %r = fsub f80 %a, %b
  ret %r
}
func @f80_mul (f80, f80) -> (f80) global {
^e(%a: f80, %b: f80):
  %r = fmul f80 %a, %b
  ret %r
}
func @f80_div (f80, f80) -> (f80) global {
^e(%a: f80, %b: f80):
  %r = fdiv f80 %a, %b
  ret %r
}
func @f80_rem (f80, f80) -> (f80) global {
^e(%a: f80, %b: f80):
  %r = frem f80 %a, %b
  ret %r
}
func @f80_min (f80, f80) -> (f80) global {
^e(%a: f80, %b: f80):
  %r = fmin f80 %a, %b
  ret %r
}
func @f80_max (f80, f80) -> (f80) global {
^e(%a: f80, %b: f80):
  %r = fmax f80 %a, %b
  ret %r
}
; correctly rounded: a software helper (__fir_fma80)
func @f80_fma (f80, f80, f80) -> (f80) global {
^e(%a: f80, %b: f80, %c: f80):
  %r = fma f80 %a, %b, %c
  ret %r
}
func @f80_copysign (f80, f80) -> (f80) global {
^e(%a: f80, %b: f80):
  %r = copysign f80 %a, %b
  ret %r
}
func @f80_neg (f80) -> (f80) global {
^e(%a: f80):
  %r = fneg f80 %a
  ret %r
}
func @f80_abs (f80) -> (f80) global {
^e(%a: f80):
  %r = fabs f80 %a
  ret %r
}
func @f80_sqrt (f80) -> (f80) global {
^e(%a: f80):
  %r = sqrt f80 %a
  ret %r
}
func @f80_floor (f80) -> (f80) global {
^e(%a: f80):
  %r = floor f80 %a
  ret %r
}
func @f80_ceil (f80) -> (f80) global {
^e(%a: f80):
  %r = ceil f80 %a
  ret %r
}
func @f80_trunc (f80) -> (f80) global {
^e(%a: f80):
  %r = ftrunc f80 %a
  ret %r
}
func @f80_roundeven (f80) -> (f80) global {
^e(%a: f80):
  %r = roundeven f80 %a
  ret %r
}

; bit k set when predicate k holds: oeq ogt oge olt ole one ord ueq ugt uge
; ult ule une uno (0..13), true (14); false would be bit 15
func @f80_cmp (f80, f80) -> (i32) global {
^e(%a: f80, %b: f80):
  %c0 = fcmp oeq i1 %a, %b
  %c1 = fcmp ogt i1 %a, %b
  %c2 = fcmp oge i1 %a, %b
  %c3 = fcmp olt i1 %a, %b
  %c4 = fcmp ole i1 %a, %b
  %c5 = fcmp one i1 %a, %b
  %c6 = fcmp ord i1 %a, %b
  %c7 = fcmp ueq i1 %a, %b
  %c8 = fcmp ugt i1 %a, %b
  %c9 = fcmp uge i1 %a, %b
  %c10 = fcmp ult i1 %a, %b
  %c11 = fcmp ule i1 %a, %b
  %c12 = fcmp une i1 %a, %b
  %c13 = fcmp uno i1 %a, %b
  %c14 = fcmp true i1 %a, %b
  %c15 = fcmp false i1 %a, %b
  %z0 = zext i32 %c0
  %z1 = zext i32 %c1
  %z2 = zext i32 %c2
  %z3 = zext i32 %c3
  %z4 = zext i32 %c4
  %z5 = zext i32 %c5
  %z6 = zext i32 %c6
  %z7 = zext i32 %c7
  %z8 = zext i32 %c8
  %z9 = zext i32 %c9
  %z10 = zext i32 %c10
  %z11 = zext i32 %c11
  %z12 = zext i32 %c12
  %z13 = zext i32 %c13
  %z14 = zext i32 %c14
  %z15 = zext i32 %c15
  %s1 = shl i32 %z1, i32 1
  %s2 = shl i32 %z2, i32 2
  %s3 = shl i32 %z3, i32 3
  %s4 = shl i32 %z4, i32 4
  %s5 = shl i32 %z5, i32 5
  %s6 = shl i32 %z6, i32 6
  %s7 = shl i32 %z7, i32 7
  %s8 = shl i32 %z8, i32 8
  %s9 = shl i32 %z9, i32 9
  %s10 = shl i32 %z10, i32 10
  %s11 = shl i32 %z11, i32 11
  %s12 = shl i32 %z12, i32 12
  %s13 = shl i32 %z13, i32 13
  %s14 = shl i32 %z14, i32 14
  %s15 = shl i32 %z15, i32 15
  %o1 = or i32 %z0, %s1
  %o2 = or i32 %o1, %s2
  %o3 = or i32 %o2, %s3
  %o4 = or i32 %o3, %s4
  %o5 = or i32 %o4, %s5
  %o6 = or i32 %o5, %s6
  %o7 = or i32 %o6, %s7
  %o8 = or i32 %o7, %s8
  %o9 = or i32 %o8, %s9
  %o10 = or i32 %o9, %s10
  %o11 = or i32 %o10, %s11
  %o12 = or i32 %o11, %s12
  %o13 = or i32 %o12, %s13
  %o14 = or i32 %o13, %s14
  %o15 = or i32 %o14, %s15
  ret %o15
}

; conversions
func @f80_to_i32 (f80) -> (i32) global {
^e(%a: f80):
  %r = fptosi i32 %a
  ret %r
}
func @f80_to_i64 (f80) -> (i64) global {
^e(%a: f80):
  %r = fptosi i64 %a
  ret %r
}
func @f80_to_u32 (f80) -> (i32) global {
^e(%a: f80):
  %r = fptoui i32 %a
  ret %r
}
func @f80_to_u64 (f80) -> (i64) global {
^e(%a: f80):
  %r = fptoui i64 %a
  ret %r
}
func @f80_sat_i8 (f80) -> (i8) global {
^e(%a: f80):
  %r = fptosi_sat i8 %a
  ret %r
}
func @f80_sat_i64 (f80) -> (i64) global {
^e(%a: f80):
  %r = fptosi_sat i64 %a
  ret %r
}
func @f80_sat_u16 (f80) -> (i16) global {
^e(%a: f80):
  %r = fptoui_sat i16 %a
  ret %r
}
func @f80_sat_u64 (f80) -> (i64) global {
^e(%a: f80):
  %r = fptoui_sat i64 %a
  ret %r
}
func @i32_to_f80 (i32) -> (f80) global {
^e(%a: i32):
  %r = sitofp f80 %a
  ret %r
}
func @i64_to_f80 (i64) -> (f80) global {
^e(%a: i64):
  %r = sitofp f80 %a
  ret %r
}
func @u32_to_f80 (i32) -> (f80) global {
^e(%a: i32):
  %r = uitofp f80 %a
  ret %r
}
func @u64_to_f80 (i64) -> (f80) global {
^e(%a: i64):
  %r = uitofp f80 %a
  ret %r
}
func @f80_to_f64 (f80) -> (f64) global {
^e(%a: f80):
  %r = fptrunc f64 %a
  ret %r
}
func @f80_to_f32 (f80) -> (f32) global {
^e(%a: f80):
  %r = fptrunc f32 %a
  ret %r
}
func @f64_to_f80 (f64) -> (f80) global {
^e(%a: f64):
  %r = fpext f80 %a
  ret %r
}
func @f32_to_f80 (f32) -> (f80) global {
^e(%a: f32):
  %r = fpext f80 %a
  ret %r
}

func @f80_select (i32, f80, f80) -> (f80) global {
^e(%c: i32, %a: f80, %b: f80):
  %t = icmp ne i1 %c, i32 0
  %r = select f80 %t, %a, %b
  %f = freeze f80 %r
  ret %f
}

; (a, b) = (b, a + b) n times, then a: the loop parameters swap every
; iteration, so the edge copies must be parallel
func @f80_fib (f80, f80, i32) -> (f80) global {
^e(%a0: f80, %b0: f80, %n: i32):
  jump ^loop(%a0, %b0, i32 0)
^loop(%a: f80, %b: f80, %i: i32):
  %done = icmp eq i1 %i, %n
  br %done, ^exit, ^body
^body:
  %s = fadd f80 %a, %b
  %i2 = add i32 %i, i32 1
  jump ^loop(%b, %s, %i2)
^exit:
  ret %a
}

; p[i] = p[i] * k + 0.5 for i < n, then the sum of the results
func @f80_scale (ptr, i64, f80) -> (f80) global {
^e(%p: ptr, %n: i64, %k: f80):
  jump ^loop(i64 0, f80 0x0)
^loop(%i: i64, %acc: f80):
  %done = icmp eq i1 %i, %n
  br %done, ^exit, ^body
^body:
  %q = ptradd ptr %p, %i, 16, 0
  %x = load f80 %q, align 16
  %y = fmul f80 %x, %k
  %h = fadd f80 %y, f80 0x3ffe8000000000000000
  store %h, %q, align 16
  %acc2 = fadd f80 %acc, %h
  %i2 = add i64 %i, i64 1
  jump ^loop(%i2, %acc2)
^exit:
  ret %acc
}

; more f80 arguments than fit anywhere but the stack, mixed with others
func @f80_many (i32, f80, f64, f80, i64, f80, f80, i32, f80, f32, f80) -> (f80) global {
^e(%i0: i32, %a: f80, %d: f64, %b: f80, %l: i64, %c: f80, %e: f80, %i1: i32, %f: f80, %s: f32, %g: f80):
  %di = fpext f80 %d
  %li = sitofp f80 %l
  %ii0 = sitofp f80 %i0
  %ii1 = sitofp f80 %i1
  %si = fpext f80 %s
  %t1 = fmul f80 %a, %ii0
  %t2 = fadd f80 %t1, %b
  %t3 = fsub f80 %t2, %di
  %t4 = fmul f80 %t3, %c
  %t5 = fadd f80 %t4, %li
  %t6 = fdiv f80 %t5, %e
  %t7 = fadd f80 %t6, %ii1
  %t8 = fmul f80 %t7, %f
  %t9 = fsub f80 %t8, %si
  %t10 = fadd f80 %t9, %g
  ret %t10
}

; calls into C: an f80 result in st0, f80 arguments in memory
func @f80_calls_c (f80, f80) -> (f80) global {
^e(%a: f80, %b: f80):
  %r = call (f80, f64, f80, i32) -> (f80) @c_mix(%a, f64 0x3ff8000000000000, %b, i32 7)
  %v = call (i32, ...) -> (f80) @c_vsum(i32 3, %r, f64 0x4000000000000000, %a)
  %w = fadd f80 %v, %b
  ret %w
}

; the sum of n (f80, i32) pairs passed through `...`
func @f80_vsum (i32, ...) -> (f80) global {
^e(%n: i32):
  %ap = alloca ptr i64 24, align 8
  va_start %ap
  jump ^loop(i32 0, f80 0x0)
^loop(%i: i32, %acc: f80):
  %done = icmp eq i1 %i, %n
  br %done, ^exit, ^body
^body:
  %x = va_arg f80 %ap
  %k = va_arg i32 %ap
  %kf = sitofp f80 %k
  %t = fmul f80 %x, %kf
  %acc2 = fadd f80 %acc, %t
  %i2 = add i32 %i, i32 1
  jump ^loop(%i2, %acc2)
^exit:
  ret %acc
}
