; SysV ABI of vectors and of structs with vector, i128 and over-aligned
; members, both call directions. Checked against clang by vecabi.c: each
; @fir_X is called by C with a reference result; each @fir_call_X just
; forwards to C's @c_X, so its arguments and result cross the boundary in
; both directions.
;   <4 x f32>, <2 x f64>, <4 x i32>  one xmm register (SSE + SSEUP)
;   <2 x f32>, <1 x i64>             one xmm register (SSE)
;   <4 x i8>                         one GPR (INTEGER)
;   <3 x f32>                        16 bytes as C has it: one xmm register
;   <8 x f32>                        MEMORY, 32-aligned on the stack
;   nine <4 x i32>                   past the 8 xmm registers: the stack
;   SI  = {__int128 a}               INTEGER, INTEGER; the stack 16-aligned
;   SV  = {v4f a}                    SSE + SSEUP
;   S2V = {v2f a; float b}           SSE, SSE
;   SA  = {double d} aligned(16)     SSE, NO_CLASS: one xmm register
;   EV  = {struct {} e; float f}     an empty member takes nothing
; and vectors and an SV through `...` (va_arg) both ways.
triple "x86_64-unknown-linux-gnu"
declare @c_v4f (<4 x f32>, <4 x f32>) -> (<4 x f32>) global
declare @c_v2f (<2 x f32>, f32) -> (<2 x f32>) global
declare @c_v4c (<4 x i8>, <4 x i8>) -> (<4 x i8>) global
declare @c_v2d (<2 x f64>) -> (f64) global
declare @c_v1l (<1 x i64>) -> (<1 x i64>) global
declare @c_v3f (<3 x f32>) -> (<3 x f32>) global
declare @c_v8f (i64, i64, i64, i64, i64, i64, i64, <8 x f32>) -> (f32) global
declare @c_v9 (<4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, i32) -> (i32) global
declare @c_si (i64, agg(16, 16, [0: i128]), agg(16, 16, [0: i128]), agg(16, 16, [0: i128])) -> (agg(16, 16, [0: i128])) global
declare @c_sv (agg(16, 16, [0: <4 x f32>])) -> (agg(16, 16, [0: <4 x f32>])) global
declare @c_s2v (agg(16, 8, [0: <2 x f32>, 8: f32])) -> (agg(16, 8, [0: <2 x f32>, 8: f32])) global
declare @c_sa (agg(16, 16, [0: f64])) -> (agg(16, 16, [0: f64])) global
declare @c_ev (agg(4, 4, [0: f32]), f32) -> (f32) global
declare @c_vsum (i32, ...) -> (f32) global

func @fir_v4f (<4 x f32>, <4 x f32>) -> (<4 x f32>) global {
^e(%a: <4 x f32>, %b: <4 x f32>):
  %m = fmul <4 x f32> %a, %b
  %r = fadd <4 x f32> %m, %a
  ret %r
}
func @fir_v2f (<2 x f32>, f32) -> (<2 x f32>) global {
^e(%a: <2 x f32>, %s: f32):
  %sv = splat <2 x f32> %s
  %r = fmul <2 x f32> %a, %sv
  ret %r
}
func @fir_v4c (<4 x i8>, <4 x i8>) -> (<4 x i8>) global {
^e(%a: <4 x i8>, %b: <4 x i8>):
  %r = add <4 x i8> %a, %b
  ret %r
}
func @fir_v2d (<2 x f64>) -> (f64) global {
^e(%a: <2 x f64>):
  %x = extract f64 %a, i32 0
  %y = extract f64 %a, i32 1
  %r = fsub f64 %x, %y
  ret %r
}
func @fir_v1l (<1 x i64>) -> (<1 x i64>) global {
^e(%a: <1 x i64>):
  %t = splat <1 x i64> i64 3
  %r = mul <1 x i64> %a, %t
  ret %r
}
func @fir_v3f (<3 x f32>) -> (<3 x f32>) global {
^e(%a: <3 x f32>):
  %r = fadd <3 x f32> %a, %a
  ret %r
}
; l0 * 2 + l7 + x7
func @fir_v8f (i64, i64, i64, i64, i64, i64, i64, <8 x f32>) -> (f32) global {
^e(%x1: i64, %x2: i64, %x3: i64, %x4: i64, %x5: i64, %x6: i64, %x7: i64, %v: <8 x f32>):
  %l0 = extract f32 %v, i32 0
  %l7 = extract f32 %v, i32 7
  %d = fadd f32 %l0, %l0
  %s = fadd f32 %d, %l7
  %xf = sitofp f32 %x7
  %r = fadd f32 %s, %xf
  ret %r
}
; a[2] + i[1] * 3 + h[0] + k
func @fir_v9 (<4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, i32) -> (i32) global {
^e(%a: <4 x i32>, %b: <4 x i32>, %c: <4 x i32>, %d: <4 x i32>, %e: <4 x i32>, %f: <4 x i32>, %g: <4 x i32>, %h: <4 x i32>, %i: <4 x i32>, %k: i32):
  %a2 = extract i32 %a, i32 2
  %i1 = extract i32 %i, i32 1
  %h0 = extract i32 %h, i32 0
  %t = mul i32 %i1, i32 3
  %s = add i32 %a2, %t
  %s2 = add i32 %s, %h0
  %r = add i32 %s2, %k
  ret %r
}
; x + a + 2 b + 3 c (c no longer fits the registers)
func @fir_si (i64, agg(16, 16, [0: i128]), agg(16, 16, [0: i128]), agg(16, 16, [0: i128])) -> (agg(16, 16, [0: i128])) global {
^e(%x: i64, %pa: ptr, %pb: ptr, %pc: ptr):
  %a = load i128 %pa, align 16
  %b = load i128 %pb, align 16
  %c = load i128 %pc, align 16
  %xw = sext i128 %x
  %b2 = mul i128 %b, i128 2
  %c3 = mul i128 %c, i128 3
  %s1 = add i128 %xw, %a
  %s2 = add i128 %s1, %b2
  %s3 = add i128 %s2, %c3
  %out = alloca ptr i64 16, align 16
  store %s3, %out, align 16
  ret %out
}
; the lanes reversed
func @fir_sv (agg(16, 16, [0: <4 x f32>])) -> (agg(16, 16, [0: <4 x f32>])) global {
^e(%p: ptr):
  %v = load <4 x f32> %p, align 16
  %r = shuffle <4 x f32> %v, %v, [3, 2, 1, 0]
  %out = alloca ptr i64 16, align 16
  store %r, %out, align 16
  ret %out
}
; {a * b, b + 1}
func @fir_s2v (agg(16, 8, [0: <2 x f32>, 8: f32])) -> (agg(16, 8, [0: <2 x f32>, 8: f32])) global {
^e(%p: ptr):
  %a = load <2 x f32> %p, align 8
  %q = ptradd ptr %p, i64 0, 1, 8
  %b = load f32 %q, align 4
  %bv = splat <2 x f32> %b
  %m = fmul <2 x f32> %a, %bv
  %b1 = fadd f32 %b, f32 0x3f800000
  %out = alloca ptr i64 16, align 8
  store %m, %out, align 8
  %oq = ptradd ptr %out, i64 0, 1, 8
  store %b1, %oq, align 4
  ret %out
}
func @fir_sa (agg(16, 16, [0: f64])) -> (agg(16, 16, [0: f64])) global {
^e(%p: ptr):
  %d = load f64 %p, align 16
  %r = fmul f64 %d, f64 0x4008000000000000
  %out = alloca ptr i64 16, align 16
  store %r, %out, align 16
  ret %out
}
func @fir_ev (agg(4, 4, [0: f32]), f32) -> (f32) global {
^e(%p: ptr, %g: f32):
  %f = load f32 %p, align 4
  %r = fsub f32 %f, %g
  ret %r
}
; n vectors (lane i mod 4 of the i-th), then an SV (its lane 3) and a
; double: sum * 10 + lane + d
func @fir_vsum (i32, ...) -> (f32) global {
^e(%n: i32):
  %ap = alloca ptr i64 24, align 8
  va_start %ap
  jump ^l(i32 0, f32 0x00000000)
^l(%i: i32, %s: f32):
  %done = icmp eq i1 %i, %n
  br %done, ^x, ^b
^b:
  %v = va_arg <4 x f32> %ap
  %k = and i32 %i, i32 3
  %l = extract f32 %v, %k
  %s1 = fadd f32 %s, %l
  %i1 = add i32 %i, i32 1
  jump ^l(%i1, %s1)
^x:
  %p = va_arg agg(16, 16, [0: <4 x f32>]) %ap
  %q = ptradd ptr %p, i64 0, 1, 12
  %l3 = load f32 %q, align 4
  %d = va_arg f64 %ap
  %df = fptrunc f32 %d
  %s10 = fmul f32 %s, f32 0x41200000
  %t = fadd f32 %s10, %l3
  %r = fadd f32 %t, %df
  ret %r
}

; ---- forwarding to C ----
func @fir_call_v4f (<4 x f32>, <4 x f32>) -> (<4 x f32>) global {
^e(%a: <4 x f32>, %b: <4 x f32>):
  %r = call (<4 x f32>, <4 x f32>) -> (<4 x f32>) @c_v4f(%a, %b)
  ret %r
}
func @fir_call_v2f (<2 x f32>, f32) -> (<2 x f32>) global {
^e(%a: <2 x f32>, %s: f32):
  %r = call (<2 x f32>, f32) -> (<2 x f32>) @c_v2f(%a, %s)
  ret %r
}
func @fir_call_v4c (<4 x i8>, <4 x i8>) -> (<4 x i8>) global {
^e(%a: <4 x i8>, %b: <4 x i8>):
  %r = call (<4 x i8>, <4 x i8>) -> (<4 x i8>) @c_v4c(%a, %b)
  ret %r
}
func @fir_call_v2d (<2 x f64>) -> (f64) global {
^e(%a: <2 x f64>):
  %r = call (<2 x f64>) -> (f64) @c_v2d(%a)
  ret %r
}
func @fir_call_v1l (<1 x i64>) -> (<1 x i64>) global {
^e(%a: <1 x i64>):
  %r = call (<1 x i64>) -> (<1 x i64>) @c_v1l(%a)
  ret %r
}
func @fir_call_v3f (<3 x f32>) -> (<3 x f32>) global {
^e(%a: <3 x f32>):
  %r = call (<3 x f32>) -> (<3 x f32>) @c_v3f(%a)
  ret %r
}
func @fir_call_v8f (i64, <8 x f32>) -> (f32) global {
^e(%x: i64, %v: <8 x f32>):
  %r = call (i64, i64, i64, i64, i64, i64, i64, <8 x f32>) -> (f32) @c_v8f(i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, %x, %v)
  ret %r
}
func @fir_call_v9 (<4 x i32>, <4 x i32>, i32) -> (i32) global {
^e(%a: <4 x i32>, %b: <4 x i32>, %k: i32):
  %r = call (<4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, <4 x i32>, i32) -> (i32) @c_v9(%a, %b, %b, %b, %b, %b, %b, %a, %b, %k)
  ret %r
}
func @fir_call_si (i64, agg(16, 16, [0: i128]), agg(16, 16, [0: i128]), agg(16, 16, [0: i128])) -> (agg(16, 16, [0: i128])) global {
^e(%x: i64, %a: ptr, %b: ptr, %c: ptr):
  %r = call (i64, agg(16, 16, [0: i128]), agg(16, 16, [0: i128]), agg(16, 16, [0: i128])) -> (agg(16, 16, [0: i128])) @c_si(%x, %a, %b, %c)
  ret %r
}
func @fir_call_sv (agg(16, 16, [0: <4 x f32>])) -> (agg(16, 16, [0: <4 x f32>])) global {
^e(%p: ptr):
  %r = call (agg(16, 16, [0: <4 x f32>])) -> (agg(16, 16, [0: <4 x f32>])) @c_sv(%p)
  ret %r
}
func @fir_call_s2v (agg(16, 8, [0: <2 x f32>, 8: f32])) -> (agg(16, 8, [0: <2 x f32>, 8: f32])) global {
^e(%p: ptr):
  %r = call (agg(16, 8, [0: <2 x f32>, 8: f32])) -> (agg(16, 8, [0: <2 x f32>, 8: f32])) @c_s2v(%p)
  ret %r
}
func @fir_call_sa (agg(16, 16, [0: f64])) -> (agg(16, 16, [0: f64])) global {
^e(%p: ptr):
  %r = call (agg(16, 16, [0: f64])) -> (agg(16, 16, [0: f64])) @c_sa(%p)
  ret %r
}
func @fir_call_ev (agg(4, 4, [0: f32]), f32) -> (f32) global {
^e(%p: ptr, %g: f32):
  %r = call (agg(4, 4, [0: f32]), f32) -> (f32) @c_ev(%p, %g)
  ret %r
}
; c_vsum(3, a, b, a, s, d)
func @fir_call_vsum (<4 x f32>, <4 x f32>, agg(16, 16, [0: <4 x f32>]), f64) -> (f32) global {
^e(%a: <4 x f32>, %b: <4 x f32>, %s: ptr, %d: f64):
  %r = call (i32, ...) -> (f32) @c_vsum(i32 3, %a, %b, %a, agg(16, 16, [0: <4 x f32>]) %s, %d)
  ret %r
}
