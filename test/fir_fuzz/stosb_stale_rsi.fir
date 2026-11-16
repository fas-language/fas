; fir_fuzz find: Mir_ct refused a memset (rep stosb) because rsi still
; held a secret (live across the memset), though stosb reads only rdi,
; rcx and al
triple "x86_64-unknown-linux-gnu"

func @f (i64, i64, i8) -> (i8) local {
^e(%a: i64, %b: i64, %c: i8):
  %mem = alloca ptr i64 64, align 32
  memset %mem, i8 0, i64 64
  %s = secret i64 %b
  %ix = and i64 %a, i64 31
  %p = ptradd ptr %mem, %ix, 1, 0
  memset %p, %c, i64 16
  %t = mul i64 %s, %s
  %u = declassify i64 %t
  %k = trunc i8 %u
  %r = load i8 %mem, align 1
  %r2 = add i8 %r, %k
  ret %r2
}

func @main (i32, ptr) -> (i32) global {
^entry(%argc: i32, %argv: ptr):
  %r = call (i64, i64, i8) -> (i8) @f(i64 3, i64 9, i8 5)
  %z = zext i32 %r
  ret %z
}
