; fir_fuzz find: masked_load's lane branches split the entry block, and
; Fir_scalarize carried the alloca after it out of the entry block, which
; the verifier then refused (Fir.hoist_const_allocas)
triple "x86_64-unknown-linux-gnu"

func @main (i32, ptr) -> (i32) global {
^entry(%argc: i32, %argv: ptr):
  %m = alloca ptr i64 64, align 32
  memset %m, i8 0, i64 64
  %c = icmp eq <3 x i1> <3 x i32> [i32 1, i32 2, i32 3], <3 x i32> [i32 1, i32 0, i32 3]
  %v = masked_load <3 x i32> %m, %c, <3 x i32> [i32 7, i32 8, i32 9], align 4
  %p = alloca ptr i64 16, align 16
  store %v, %p, align 4
  %q = ptradd ptr %p, i64 1, 4, 0
  %x = load i32 %q, align 4
  ret %x
}
