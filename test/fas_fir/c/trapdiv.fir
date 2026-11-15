; The `trap` flag on udiv/sdiv/urem/srem (SPEC §6, survey A6): a zero
; divisor, and MIN / -1 for the signed ones, trap where the division
; stands. Checked by trapdiv.c, which runs each case in a child process:
; the defined inputs must give C's value, the others must die by a signal
; (which one is unspecified: x86's #DE gives SIGFPE, a `trap` SIGILL).
; The native backend leaves 64-bit divisions and narrower unsigned ones to
; the hardware and checks the rest (Fir_trapdiv); LLVM checks all.
triple "x86_64-unknown-linux-gnu"

func @sd8 (i8, i8) -> (i8) global {
^e(%a: i8, %b: i8):
  %v = sdiv trap i8 %a, %b
  ret %v
}

func @sd16 (i16, i16) -> (i16) global {
^e(%a: i16, %b: i16):
  %v = sdiv trap i16 %a, %b
  ret %v
}

func @sd32 (i32, i32) -> (i32) global {
^e(%a: i32, %b: i32):
  %v = sdiv trap i32 %a, %b
  ret %v
}

func @sr32 (i32, i32) -> (i32) global {
^e(%a: i32, %b: i32):
  %v = srem trap i32 %a, %b
  ret %v
}

func @sd64 (i64, i64) -> (i64) global {
^e(%a: i64, %b: i64):
  %v = sdiv trap i64 %a, %b
  ret %v
}

func @sr64 (i64, i64) -> (i64) global {
^e(%a: i64, %b: i64):
  %v = srem trap i64 %a, %b
  ret %v
}

func @ud32 (i32, i32) -> (i32) global {
^e(%a: i32, %b: i32):
  %v = udiv trap i32 %a, %b
  ret %v
}

func @ur64 (i64, i64) -> (i64) global {
^e(%a: i64, %b: i64):
  %v = urem trap i64 %a, %b
  ret %v
}

; an odd width: MIN of i24 is -8388608
func @sd24 (i32, i32) -> (i32) global {
^e(%a: i32, %b: i32):
  %a24 = trunc i24 %a
  %b24 = trunc i24 %b
  %v24 = sdiv trap i24 %a24, %b24
  %v = sext i32 %v24
  ret %v
}

; wider than 64 bits: done in limbs, so always checked
func @sd128 (i128, i128) -> (i128) global {
^e(%a: i128, %b: i128):
  %v = sdiv trap i128 %a, %b
  ret %v
}

func @ud128 (i128, i128) -> (i128) global {
^e(%a: i128, %b: i128):
  %v = udiv trap i128 %a, %b
  ret %v
}

; a vector: any lane traps
func @sdv (ptr, ptr, ptr) -> () global {
^e(%a: ptr, %b: ptr, %r: ptr):
  %x = load <4 x i32> %a, align 4
  %y = load <4 x i32> %b, align 4
  %v = sdiv trap <4 x i32> %x, %y
  store %v, %r, align 4
  ret
}

; the result is unused: the division still traps
func @dead (i32, i32) -> (i32) global {
^e(%a: i32, %b: i32):
  %v = sdiv trap i32 %a, %b
  ret i32 7
}

; a constant zero divisor traps when it runs, not at compile time
func @kzero (i32, i32) -> (i32) global {
^e(%a: i32, %c: i32):
  %z = icmp eq i1 %c, i32 0
  br %z, ^skip, ^div
^div:
  %v = udiv trap i32 %a, i32 0
  ret %v
^skip:
  ret i32 5
}

; a constant divisor that cannot trap needs no check
func @kseven (i32) -> (i32) global {
^e(%a: i32):
  %v = sdiv trap i32 %a, i32 -7
  ret %v
}
