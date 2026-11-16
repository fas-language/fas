; Several results (SPEC §5): the callee side. main.fir is the caller; the
; two are built with each backend and linked in all four pairings
; (test/fir_multi.sh), so the native and LLVM paths must agree.
triple "x86_64-unknown-linux-gnu"

global @g size 8 align 8 = zero

; registers only: f32 in xmm0, i32 in rax, f64 in xmm1, a zext i8 in rdx
func @m1 (i64) -> (f32, i32, f64, zext i8) global noinline {
^e(%x: i64):
  %f = sitofp f32 %x
  %t = trunc i32 %x
  %i = mul i32 %t, i32 3
  %d = sitofp f64 %x
  %d2 = fadd f64 %d, %d
  ret %f, %i, %d2, i8 200
}

; seven arguments behind the hidden block pointer (rdi): a..e in rsi..r9,
; f and g on the stack; the third result goes in the block
func @m3 (i64, i64, i64, i64, i64, i64, i64) -> (i64, i64, i64) global noinline {
^e(%a: i64, %b: i64, %c: i64, %d: i64, %e2: i64, %f: i64, %g2: i64):
  %ab = add i64 %a, %b
  %cd = mul i64 %c, %d
  %ef = add i64 %e2, %f
  %efg = add i64 %ef, %g2
  ret %ab, %cd, %efg
}

; every class: rax, rdx, then the block for the rest of the integers, two
; f64s in xmm0/xmm1 and a third in the block, and f16, f80, i128, a vector,
; an agg and an i100 (all in the block)
func @m2 (i64) -> (i64, ptr, i8, f64, f64, f64, sext i16, f16, f80, i128, <4 x i32>, agg(24, 8, [0: i64, 8: i64, 16: i64]), i100, f32) global noinline {
^e(%x: i64):
  %x1 = add i64 %x, i64 1
  store i64 77, ptr @g, align 8
  %xd = sitofp f64 %x
  %ten = fadd f64 %xd, %xd
  %two = sitofp f64 i32 2
  %half = fdiv f64 %xd, %two
  %three = sitofp f64 i32 3
  %h = sitofp f16 i32 4
  %l = sitofp f80 i32 9
  %s = alloca ptr i64 24, align 8
  store i64 11, %s, align 8
  %s1 = ptradd ptr %s, i64 0, 1, 8
  store i64 12, %s1, align 8
  %s2 = ptradd ptr %s, i64 0, 1, 16
  store i64 13, %s2, align 8
  %f = sitofp f32 i32 21
  ret %x1, ptr @g, i8 7, %ten, %half, %three, i16 -3, %h, %l, i128 0x10000000000000005, <4 x i32> [i32 1, i32 2, i32 3, i32 4], %s, i100 0x400000000000000008, %f
}
