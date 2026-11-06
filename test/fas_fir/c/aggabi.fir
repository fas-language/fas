; SysV struct-by-value: every eightbyte class pairing, MEMORY class with sret,
; register exhaustion and both call directions. (Structs through `...` are
; in vastruct.fir.)
; Checked against clang by aggabi.c.
;   II  = {long a, b}            INTEGER, INTEGER
;   DD  = {double x, y}          SSE, SSE
;   ID  = {long a; double d}     INTEGER, SSE
;   DI  = {double d; long a}     SSE, INTEGER
;   FFI = {float x, y; int z}    SSE (two floats), INTEGER
;   BIG = {long a, b, c}         MEMORY (24 bytes)
;   S1  = {int a}                INTEGER, one eightbyte
triple "x86_64-unknown-linux-gnu"
declare @c_ii (agg(16, 8, [0: i64, 8: i64])) -> (i64) global
declare @c_mk_ii (i64, i64) -> (agg(16, 8, [0: i64, 8: i64])) global
declare @c_mk_big (i64) -> (agg(24, 8, [0: i64, 8: i64, 16: i64])) global
declare @c_big (agg(24, 8, [0: i64, 8: i64, 16: i64])) -> (i64) global
declare @c_mk_dd (f64, f64) -> (agg(16, 8, [0: f64, 8: f64])) global
declare @c_ffi (agg(12, 4, [0: f32, 4: f32, 8: i32])) -> (i64) global
declare @c_exhaust (i64, i64, i64, i64, i64, agg(16, 8, [0: i64, 8: i64]), i64) -> (i64) global

func @fir_ii (agg(16, 8, [0: i64, 8: i64])) -> (i64) global {
^e(%p: ptr):
  %a = load i64 %p, align 8
  %q = ptradd ptr %p, i64 0, 1, 8
  %b = load i64 %q, align 8
  %a3 = mul i64 %a, i64 3
  %r = add i64 %a3, %b
  ret %r
}
func @fir_dd (agg(16, 8, [0: f64, 8: f64])) -> (f64) global {
^e(%p: ptr):
  %x = load f64 %p, align 8
  %q = ptradd ptr %p, i64 0, 1, 8
  %y = load f64 %q, align 8
  %r = fsub f64 %x, %y
  ret %r
}
func @fir_id (agg(16, 8, [0: i64, 8: f64])) -> (f64) global {
^e(%p: ptr):
  %a = load i64 %p, align 8
  %q = ptradd ptr %p, i64 0, 1, 8
  %d = load f64 %q, align 8
  %af = sitofp f64 %a
  %d2 = fmul f64 %d, f64 0x4000000000000000
  %r = fadd f64 %af, %d2
  ret %r
}
func @fir_di (agg(16, 8, [0: f64, 8: i64])) -> (f64) global {
^e(%p: ptr):
  %d = load f64 %p, align 8
  %q = ptradd ptr %p, i64 0, 1, 8
  %a = load i64 %q, align 8
  %af = sitofp f64 %a
  %d2 = fmul f64 %d, f64 0x4000000000000000
  %r = fsub f64 %d2, %af
  ret %r
}
func @fir_ffi (agg(12, 4, [0: f32, 4: f32, 8: i32])) -> (i64) global {
^e(%p: ptr):
  %x = load f32 %p, align 4
  %q = ptradd ptr %p, i64 0, 1, 4
  %y = load f32 %q, align 4
  %q2 = ptradd ptr %p, i64 0, 1, 8
  %z = load i32 %q2, align 4
  %xi = fptosi i64 %x
  %yi = fptosi i64 %y
  %zi = sext i64 %z
  %t = mul i64 %xi, i64 100
  %t1 = mul i64 %yi, i64 10
  %t2 = add i64 %t, %t1
  %r = add i64 %t2, %zi
  ret %r
}
func @fir_s1 (agg(4, 4, [0: i32])) -> (i32) global {
^e(%p: ptr):
  %a = load i32 %p, align 4
  %r = mul i32 %a, i32 -2
  ret %r
}
func @fir_big (agg(24, 8, [0: i64, 8: i64, 16: i64])) -> (i64) global {
^e(%p: ptr):
  %a = load i64 %p, align 8
  %q = ptradd ptr %p, i64 0, 1, 8
  %b = load i64 %q, align 8
  %q2 = ptradd ptr %p, i64 0, 1, 16
  %c = load i64 %q2, align 8
  %b2 = mul i64 %b, i64 2
  %c3 = mul i64 %c, i64 3
  %t = add i64 %a, %b2
  %r = add i64 %t, %c3
  ret %r
}
; five integers leave one GPR, so the II goes to the stack and %g takes r9
func @fir_exhaust (i64, i64, i64, i64, i64, agg(16, 8, [0: i64, 8: i64]), i64) -> (i64) global {
^e(%a: i64, %b: i64, %c: i64, %d: i64, %e: i64, %p: ptr, %g: i64):
  %x = load i64 %p, align 8
  %q = ptradd ptr %p, i64 0, 1, 8
  %y = load i64 %q, align 8
  %t1 = mul i64 %a, i64 2
  %t2 = add i64 %t1, %b
  %t3 = mul i64 %t2, i64 2
  %t4 = add i64 %t3, %c
  %t5 = mul i64 %t4, i64 2
  %t6 = add i64 %t5, %d
  %t7 = mul i64 %t6, i64 2
  %t8 = add i64 %t7, %e
  %t9 = mul i64 %t8, i64 2
  %t10 = add i64 %t9, %x
  %t11 = mul i64 %t10, i64 2
  %t12 = add i64 %t11, %y
  %t13 = mul i64 %t12, i64 2
  %r = add i64 %t13, %g
  ret %r
}
func @fir_mk_ii (i64, i64) -> (agg(16, 8, [0: i64, 8: i64])) global {
^e(%a: i64, %b: i64):
  %s = alloca ptr i64 16, align 8
  store %a, %s, align 8
  %q = ptradd ptr %s, i64 0, 1, 8
  store %b, %q, align 8
  ret %s
}
func @fir_mk_dd (f64, f64) -> (agg(16, 8, [0: f64, 8: f64])) global {
^e(%x: f64, %y: f64):
  %s = alloca ptr i64 16, align 8
  store %x, %s, align 8
  %q = ptradd ptr %s, i64 0, 1, 8
  store %y, %q, align 8
  ret %s
}
func @fir_mk_di (f64, i64) -> (agg(16, 8, [0: f64, 8: i64])) global {
^e(%d: f64, %a: i64):
  %s = alloca ptr i64 16, align 8
  store %d, %s, align 8
  %q = ptradd ptr %s, i64 0, 1, 8
  store %a, %q, align 8
  ret %s
}
func @fir_mk_ffi (f32, f32, i32) -> (agg(12, 4, [0: f32, 4: f32, 8: i32])) global {
^e(%x: f32, %y: f32, %z: i32):
  %s = alloca ptr i64 12, align 4
  store %x, %s, align 4
  %q = ptradd ptr %s, i64 0, 1, 4
  store %y, %q, align 4
  %q2 = ptradd ptr %s, i64 0, 1, 8
  store %z, %q2, align 4
  ret %s
}
func @fir_mk_big (i64) -> (agg(24, 8, [0: i64, 8: i64, 16: i64])) global {
^e(%n: i64):
  %s = alloca ptr i64 24, align 8
  store %n, %s, align 8
  %q = ptradd ptr %s, i64 0, 1, 8
  %n1 = add i64 %n, i64 1
  store %n1, %q, align 8
  %q2 = ptradd ptr %s, i64 0, 1, 16
  %n2 = add i64 %n, i64 2
  store %n2, %q2, align 8
  ret %s
}
; Fir calling C: results feed straight back in as arguments
func @fir_calls_c () -> (i64) global {
^e:
  %ii = call (i64, i64) -> (agg(16, 8, [0: i64, 8: i64])) @c_mk_ii(i64 5, i64 7)
  %r1 = call (agg(16, 8, [0: i64, 8: i64])) -> (i64) @c_ii(%ii)
  %big = call (i64) -> (agg(24, 8, [0: i64, 8: i64, 16: i64])) @c_mk_big(i64 10)
  %r2 = call (agg(24, 8, [0: i64, 8: i64, 16: i64])) -> (i64) @c_big(%big)
  %dd = call (f64, f64) -> (agg(16, 8, [0: f64, 8: f64])) @c_mk_dd(f64 0x3ff8000000000000, f64 0xc010000000000000)
  %x = load f64 %dd, align 8
  %q = ptradd ptr %dd, i64 0, 1, 8
  %y = load f64 %q, align 8
  %xy = fmul f64 %x, %y
  %r3 = fptosi i64 %xy
  %f = alloca ptr i64 12, align 4
  store f32 0x40400000, %f, align 4
  %fq = ptradd ptr %f, i64 0, 1, 4
  store f32 0x40800000, %fq, align 4
  %fq2 = ptradd ptr %f, i64 0, 1, 8
  store i32 5, %fq2, align 4
  %r4 = call (agg(12, 4, [0: f32, 4: f32, 8: i32])) -> (i64) @c_ffi(%f)
  %r5 = call (i64, i64, i64, i64, i64, agg(16, 8, [0: i64, 8: i64]), i64) -> (i64) @c_exhaust(i64 1, i64 2, i64 3, i64 4, i64 5, %ii, i64 9)
  %s1 = mul i64 %r1, i64 1000003
  %s2 = add i64 %s1, %r2
  %s3 = mul i64 %s2, i64 1000003
  %s4 = add i64 %s3, %r3
  %s5 = mul i64 %s4, i64 1000003
  %s6 = add i64 %s5, %r4
  %s7 = mul i64 %s6, i64 1000003
  %s8 = add i64 %s7, %r5
  %r = add i64 %s8, i64 0
  ret %r
}
; C3 = {char a, b, c}: 3 bytes in one INTEGER eightbyte, copied to a padded slot
func @fir_c3 (agg(3, 1, [0: i8, 1: i8, 2: i8])) -> (agg(3, 1, [0: i8, 1: i8, 2: i8])) global {
^e(%p: ptr):
  %a = load i8 %p, align 1
  %q = ptradd ptr %p, i64 0, 1, 1
  %b = load i8 %q, align 1
  %q2 = ptradd ptr %p, i64 0, 1, 2
  %c = load i8 %q2, align 1
  %s = alloca ptr i64 3, align 1
  store %c, %s, align 1
  %s1 = ptradd ptr %s, i64 0, 1, 1
  %b1 = add i8 %b, i8 1
  store %b1, %s1, align 1
  %s2 = ptradd ptr %s, i64 0, 1, 2
  store %a, %s2, align 1
  ret %s
}
; I5 = {int a[5]}: 20 bytes, MEMORY class, reversed and returned through sret
func @fir_i5 (agg(20, 4, [0: i32, 4: i32, 8: i32, 12: i32, 16: i32])) -> (agg(20, 4, [0: i32, 4: i32, 8: i32, 12: i32, 16: i32])) global {
^e(%p: ptr):
  %s = alloca ptr i64 20, align 4
  jump ^loop(i64 0)
^loop(%i: i64):
  %done = icmp eq i1 %i, i64 5
  br %done, ^exit, ^body
^body:
  %src = ptradd ptr %p, %i, 4, 0
  %v = load i32 %src, align 4
  %j = sub i64 i64 4, %i
  %dst = ptradd ptr %s, %j, 4, 0
  store %v, %dst, align 4
  %i1 = add i64 %i, i64 1
  jump ^loop(%i1)
^exit:
  ret %s
}
; IFF = {int i; float f, g}: first eightbyte mixes INTEGER and SSE -> INTEGER
func @fir_iff (agg(12, 4, [0: i32, 4: f32, 8: f32])) -> (agg(12, 4, [0: i32, 4: f32, 8: f32])) global {
^e(%p: ptr):
  %i = load i32 %p, align 4
  %q = ptradd ptr %p, i64 0, 1, 4
  %f = load f32 %q, align 4
  %q2 = ptradd ptr %p, i64 0, 1, 8
  %g = load f32 %q2, align 4
  %s = alloca ptr i64 12, align 4
  %i2 = mul i32 %i, i32 -1
  store %i2, %s, align 4
  %s1 = ptradd ptr %s, i64 0, 1, 4
  store %g, %s1, align 4
  %s2 = ptradd ptr %s, i64 0, 1, 8
  store %f, %s2, align 4
  ret %s
}
; indirect call through a C function pointer, struct in and struct out
func @fir_apply (ptr, agg(16, 8, [0: f64, 8: i64])) -> (agg(16, 8, [0: f64, 8: i64])) global {
^e(%fn: ptr, %p: ptr):
  %r = call (agg(16, 8, [0: f64, 8: i64])) -> (agg(16, 8, [0: f64, 8: i64])) %fn(%p)
  ret %r
}
