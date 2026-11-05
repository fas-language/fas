; Fuzzer finds:
; - fshl/fshr were lowered as rotations (second operand ignored) and the
;   narrow left shift was not truncated back to the type's width.
; - 64-bit saturating add/sub were unsupported natively.
; Operands come in as parameters of a local function so that they are not
; constant folded away.
triple "x86_64-unknown-linux-gnu"

func @f (ptr, i64, i64, i64, i8, i8, i8) -> () local {
^entry(%o: ptr, %a: i64, %b: i64, %k: i64, %c: i8, %d: i8, %j: i8):
  %r0 = fshl i64 %a, %b, %k
  store %r0, %o, align 8
  %r1 = fshr i64 %a, %b, %k
  %o1 = ptradd ptr %o, i64 0, 1, 8
  store %r1, %o1, align 8
  %r2 = fshl i8 %c, %d, %j
  %r2w = zext i64 %r2
  %o2 = ptradd ptr %o, i64 0, 1, 16
  store %r2w, %o2, align 8
  %r3 = fshr i8 %c, %d, %j
  %r3w = zext i64 %r3
  %o3 = ptradd ptr %o, i64 0, 1, 24
  store %r3w, %o3, align 8
  %z = and i64 %k, i64 0
  %r4 = fshl i64 %a, %b, %z
  %o4 = ptradd ptr %o, i64 0, 1, 32
  store %r4, %o4, align 8
  %r5 = fshr i64 %a, %b, %z
  %o5 = ptradd ptr %o, i64 0, 1, 40
  store %r5, %o5, align 8
  %r6 = uadd_sat i64 %a, %b
  %o6 = ptradd ptr %o, i64 0, 1, 48
  store %r6, %o6, align 8
  %r7 = usub_sat i64 %b, %a
  %o7 = ptradd ptr %o, i64 0, 1, 56
  store %r7, %o7, align 8
  %r8 = sadd_sat i64 %a, %a
  %o8 = ptradd ptr %o, i64 0, 1, 64
  store %r8, %o8, align 8
  %r9 = ssub_sat i64 %b, %a
  %o9 = ptradd ptr %o, i64 0, 1, 72
  store %r9, %o9, align 8
  %r10 = sadd_sat i64 %b, %k
  %o10 = ptradd ptr %o, i64 0, 1, 80
  store %r10, %o10, align 8
  %r11 = ssub_sat i64 %k, %b
  %o11 = ptradd ptr %o, i64 0, 1, 88
  store %r11, %o11, align 8
  ret
}

func @main (i32, ptr) -> (i32) global {
^entry(%argc: i32, %argv: ptr):
  %out = alloca ptr i64 96, align 8
  call (ptr, i64, i64, i64, i8, i8, i8) -> () @f(%out, i64 -6917529027641081856, i64 81985529216486895, i64 13, i8 -1, i8 0, i8 3)
  %p = ptrtoint i64 %out
  %w = syscall i64 i64 1, i64 1, %p, i64 96
  call (ptr, i64, i64, i64, i8, i8, i8) -> () @f(%out, i64 9223372036854775807, i64 -9223372036854775808, i64 -1, i8 -91, i8 60, i8 13)
  %w2 = syscall i64 i64 1, i64 1, %p, i64 96
  ret i32 0
}
