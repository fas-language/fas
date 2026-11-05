; Fuzzer find: storing the i64 constant MIN emitted `mov m64, imm32 0`
; (Int64.to_int dropped bit 63 before the fits-in-imm32 check).
triple "x86_64-unknown-linux-gnu"

func @f0 (i8, f64, i64, i64, i64) -> (i64) local {
^entry(%p0: i8, %p1: f64, %p2: i64, %p3: i64, %p4: i64):
  ret %p4
}

func @main (i32, ptr) -> (i32) global {
^entry(%argc: i32, %argv: ptr):
  %out = alloca ptr i64 8, align 8
  %v1 = call (i8, f64, i64, i64, i64) -> (i64) @f0(i8 -2, f64 0x8000000000000000, i64 2147483647, i64 -4313711643905825979, i64 -9223372036854775808)
  store %v1, %out, align 8
  %v2 = ptrtoint i64 %out
  %v3 = syscall i64 i64 1, i64 1, %v2, i64 8
  ret i32 0
}
