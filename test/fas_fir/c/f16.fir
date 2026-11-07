; f16 (_Float16): Fir_wide computes it as i16 bits through f32 and integer
; rounding helpers; the ABI passes it in SSE registers. Checked by f16.c
triple "x86_64-unknown-linux-gnu"
func @h_fadd (f16, f16) -> (f16) global {
^e(%a: f16, %b: f16):
  %r = fadd f16 %a, %b
  ret %r
}
func @h_fsub (f16, f16) -> (f16) global {
^e(%a: f16, %b: f16):
  %r = fsub f16 %a, %b
  ret %r
}
func @h_fmul (f16, f16) -> (f16) global {
^e(%a: f16, %b: f16):
  %r = fmul f16 %a, %b
  ret %r
}
func @h_fdiv (f16, f16) -> (f16) global {
^e(%a: f16, %b: f16):
  %r = fdiv f16 %a, %b
  ret %r
}
func @h_frem (f16, f16) -> (f16) global {
^e(%a: f16, %b: f16):
  %r = frem f16 %a, %b
  ret %r
}
func @h_fmin (f16, f16) -> (f16) global {
^e(%a: f16, %b: f16):
  %r = fmin f16 %a, %b
  ret %r
}
func @h_fmax (f16, f16) -> (f16) global {
^e(%a: f16, %b: f16):
  %r = fmax f16 %a, %b
  ret %r
}
func @h_copysign (f16, f16) -> (f16) global {
^e(%a: f16, %b: f16):
  %r = copysign f16 %a, %b
  ret %r
}
func @h_fneg (f16) -> (f16) global {
^e(%a: f16):
  %r = fneg f16 %a
  ret %r
}
func @h_fabs (f16) -> (f16) global {
^e(%a: f16):
  %r = fabs f16 %a
  ret %r
}
func @h_sqrt (f16) -> (f16) global {
^e(%a: f16):
  %r = sqrt f16 %a
  ret %r
}
func @h_floor (f16) -> (f16) global {
^e(%a: f16):
  %r = floor f16 %a
  ret %r
}
func @h_ceil (f16) -> (f16) global {
^e(%a: f16):
  %r = ceil f16 %a
  ret %r
}
func @h_ftrunc (f16) -> (f16) global {
^e(%a: f16):
  %r = ftrunc f16 %a
  ret %r
}
func @h_roundeven (f16) -> (f16) global {
^e(%a: f16):
  %r = roundeven f16 %a
  ret %r
}
func @h_fma (f16, f16, f16) -> (f16) global {
^e(%a: f16, %b: f16, %c: f16):
  %r = fma f16 %a, %b, %c
  ret %r
}
func @h_cmp (f16, f16) -> (i32) global {
^e(%a: f16, %b: f16):
  %c0 = fcmp oeq i1 %a, %b
  %z0 = zext i32 %c0
  %s0 = shl i32 %z0, i32 0
  %o0 = or i32 i32 0, %s0
  %c1 = fcmp ogt i1 %a, %b
  %z1 = zext i32 %c1
  %s1 = shl i32 %z1, i32 1
  %o1 = or i32 %o0, %s1
  %c2 = fcmp oge i1 %a, %b
  %z2 = zext i32 %c2
  %s2 = shl i32 %z2, i32 2
  %o2 = or i32 %o1, %s2
  %c3 = fcmp olt i1 %a, %b
  %z3 = zext i32 %c3
  %s3 = shl i32 %z3, i32 3
  %o3 = or i32 %o2, %s3
  %c4 = fcmp ole i1 %a, %b
  %z4 = zext i32 %c4
  %s4 = shl i32 %z4, i32 4
  %o4 = or i32 %o3, %s4
  %c5 = fcmp one i1 %a, %b
  %z5 = zext i32 %c5
  %s5 = shl i32 %z5, i32 5
  %o5 = or i32 %o4, %s5
  %c6 = fcmp ord i1 %a, %b
  %z6 = zext i32 %c6
  %s6 = shl i32 %z6, i32 6
  %o6 = or i32 %o5, %s6
  %c7 = fcmp ueq i1 %a, %b
  %z7 = zext i32 %c7
  %s7 = shl i32 %z7, i32 7
  %o7 = or i32 %o6, %s7
  %c8 = fcmp ugt i1 %a, %b
  %z8 = zext i32 %c8
  %s8 = shl i32 %z8, i32 8
  %o8 = or i32 %o7, %s8
  %c9 = fcmp uge i1 %a, %b
  %z9 = zext i32 %c9
  %s9 = shl i32 %z9, i32 9
  %o9 = or i32 %o8, %s9
  %c10 = fcmp ult i1 %a, %b
  %z10 = zext i32 %c10
  %s10 = shl i32 %z10, i32 10
  %o10 = or i32 %o9, %s10
  %c11 = fcmp ule i1 %a, %b
  %z11 = zext i32 %c11
  %s11 = shl i32 %z11, i32 11
  %o11 = or i32 %o10, %s11
  %c12 = fcmp une i1 %a, %b
  %z12 = zext i32 %c12
  %s12 = shl i32 %z12, i32 12
  %o12 = or i32 %o11, %s12
  %c13 = fcmp uno i1 %a, %b
  %z13 = zext i32 %c13
  %s13 = shl i32 %z13, i32 13
  %o13 = or i32 %o12, %s13
  %c14 = fcmp true i1 %a, %b
  %z14 = zext i32 %c14
  %s14 = shl i32 %z14, i32 14
  %o14 = or i32 %o13, %s14
  %c15 = fcmp false i1 %a, %b
  %z15 = zext i32 %c15
  %s15 = shl i32 %z15, i32 15
  %o15 = or i32 %o14, %s15
  ret %o15
}
func @h_fptosi_i32 (f16) -> (i32) global {
^e(%a: f16):
  %r = fptosi i32 %a
  ret %r
}
func @h_fptosi_i64 (f16) -> (i64) global {
^e(%a: f16):
  %r = fptosi i64 %a
  ret %r
}
func @h_fptoui_i32 (f16) -> (i32) global {
^e(%a: f16):
  %r = fptoui i32 %a
  ret %r
}
func @h_fptoui_i64 (f16) -> (i64) global {
^e(%a: f16):
  %r = fptoui i64 %a
  ret %r
}
func @h_fptosi_sat_i8 (f16) -> (i8) global {
^e(%a: f16):
  %r = fptosi_sat i8 %a
  ret %r
}
func @h_fptosi_sat_i64 (f16) -> (i64) global {
^e(%a: f16):
  %r = fptosi_sat i64 %a
  ret %r
}
func @h_fptoui_sat_i16 (f16) -> (i16) global {
^e(%a: f16):
  %r = fptoui_sat i16 %a
  ret %r
}
func @h_fptoui_sat_i64 (f16) -> (i64) global {
^e(%a: f16):
  %r = fptoui_sat i64 %a
  ret %r
}
func @h_fptosi_i8 (f16) -> (i8) global {
^e(%a: f16):
  %r = fptosi i8 %a
  ret %r
}
func @h_fptosi_sat_i33 (ptr, f16) -> () global {
^e(%p: ptr, %a: f16):
  %r = fptosi_sat i33 %a
  store %r, %p, align 8
  ret
}
func @h_sitofp_i32 (i32) -> (f16) global {
^e(%a: i32):
  %r = sitofp f16 %a
  ret %r
}
func @h_sitofp_i64 (i64) -> (f16) global {
^e(%a: i64):
  %r = sitofp f16 %a
  ret %r
}
func @h_uitofp_i32 (i32) -> (f16) global {
^e(%a: i32):
  %r = uitofp f16 %a
  ret %r
}
func @h_uitofp_i64 (i64) -> (f16) global {
^e(%a: i64):
  %r = uitofp f16 %a
  ret %r
}
func @h_sitofp_i8 (i8) -> (f16) global {
^e(%a: i8):
  %r = sitofp f16 %a
  ret %r
}
func @h_uitofp_i16 (i16) -> (f16) global {
^e(%a: i16):
  %r = uitofp f16 %a
  ret %r
}
func @h_sitofp_i128 (ptr) -> (f16) global {
^e(%p: ptr):
  %a = load i128 %p, align 16
  %r = sitofp f16 %a
  ret %r
}
func @h_uitofp_i128 (ptr) -> (f16) global {
^e(%p: ptr):
  %a = load i128 %p, align 16
  %r = uitofp f16 %a
  ret %r
}
func @h_to_f32 (f16) -> (f32) global {
^e(%a: f16):
  %r = fpext f32 %a
  ret %r
}
func @h_from_f32 (f32) -> (f16) global {
^e(%a: f32):
  %r = fptrunc f16 %a
  ret %r
}
func @h_to_f64 (f16) -> (f64) global {
^e(%a: f16):
  %r = fpext f64 %a
  ret %r
}
func @h_from_f64 (f64) -> (f16) global {
^e(%a: f64):
  %r = fptrunc f16 %a
  ret %r
}
func @h_to_f80 (f16) -> (f80) global {
^e(%a: f16):
  %r = fpext f80 %a
  ret %r
}
func @h_from_f80 (f80) -> (f16) global {
^e(%a: f80):
  %r = fptrunc f16 %a
  ret %r
}
func @h_bits (f16) -> (i16) global {
^e(%a: f16):
  %r = bitcast i16 %a
  ret %r
}
func @h_of_bits (i16) -> (f16) global {
^e(%a: i16):
  %r = bitcast f16 %a
  ret %r
}
func @h_select (i32, f16, f16) -> (f16) global {
^e(%c: i32, %a: f16, %b: f16):
  %t = icmp ne i1 %c, i32 0
  %s = select f16 %t, %a, %b
  %r = freeze f16 %s
  ret %r
}
func @h_scale (ptr, i64, f16) -> (f16) global {
^e(%p: ptr, %n: i64, %k: f16):
  jump ^l(i64 0, f16 0x0000)
^l(%i: i64, %s: f16):
    %d = icmp eq i1 %i, %n
    br %d, ^x, ^b
^b:
    %q = ptradd ptr %p, %i, 2, 0
    %v = load f16 %q, align 2
    %m = fmul f16 %v, %k
    %w = fadd f16 %m, f16 0x3800
    store %w, %q, align 2
    %s1 = fadd f16 %s, %w
    %i1 = add i64 %i, i64 1
    jump ^l(%i1, %s1)
^x:
    ret %s
}
func @h_many (f16, f16, f16, f16, f16, f16, f16, f16, f16, f16, i32, f32, f16) -> (f16) global {
^e(%a0: f16, %a1: f16, %a2: f16, %a3: f16, %a4: f16, %a5: f16, %a6: f16, %a7: f16, %a8: f16, %a9: f16, %n: i32, %f: f32, %z: f16):
  %t0 = fadd f16 %a0, %a1
  %t1 = fadd f16 %t0, %a2
  %t2 = fmul f16 %t1, %a3
  %t3 = fadd f16 %t2, %a4
  %t4 = fmul f16 %t3, %a5
  %t5 = fadd f16 %t4, %a6
  %t6 = fmul f16 %t5, %a7
  %t7 = fadd f16 %t6, %a8
  %t8 = fmul f16 %t7, %a9
  %nf = sitofp f16 %n
  %u = fadd f16 %t8, %nf
  %ff = fptrunc f16 %f
  %v = fsub f16 %u, %ff
  %r = fadd f16 %v, %z
  ret %r
}
declare @c_hmix (f16, i32, f16, f64) -> (f16) global
declare @c_hsum (i32, ...) -> (f16) global
func @h_calls_c (f16, f16) -> (f16) global {
^e(%a: f16, %b: f16):
  %m = call (f16, i32, f16, f64) -> (f16) @c_hmix(%a, i32 3, %b, f64 0x3ff8000000000000)
  %s = call (i32, ...) -> (f16) @c_hsum(i32 3, %m, %a, %b)
  %r = fadd f16 %s, %a
  ret %r
}
func @h_vsum (i32, ...) -> (f16) global {
^e(%n: i32):
  %ap = alloca ptr i64 24, align 8
  va_start %ap
  jump ^l(i32 0, f16 0x0000)
^l(%i: i32, %s: f16):
  %d = icmp eq i1 %i, %n
  br %d, ^x, ^b
^b:
  %v = va_arg f16 %ap
  %s1 = fadd f16 %s, %v
  %i1 = add i32 %i, i32 1
  jump ^l(%i1, %s1)
^x:
  ret %s
}

; atomics: fadd/fsub as a compare-and-swap loop, xchg and cmpxchg on the bits
func @h_rmw_add (ptr, f16) -> (f16) global {
^e(%p: ptr, %v: f16):
  %o = atomic_rmw fadd f16 %p, %v, align 2 seq_cst
  ret %o
}
func @h_rmw_sub (ptr, f16) -> (f16) global {
^e(%p: ptr, %v: f16):
  %o = atomic_rmw fsub f16 %p, %v, align 2 acquire
  ret %o
}
func @h_xchg (ptr, f16) -> (f16) global {
^e(%p: ptr, %v: f16):
  %o = atomic_rmw xchg f16 %p, %v, align 2 seq_cst
  ret %o
}
func @h_cas (ptr, f16, f16) -> (i32) global {
^e(%p: ptr, %e: f16, %d: f16):
  %o, %ok = cmpxchg (f16, i1) %p, %e, %d, align 2 seq_cst, fail seq_cst
  %z = zext i32 %ok
  ret %z
}
