; Constant-time marks (SPEC §8): secret and declassify give their operand
; unchanged on every type class the backends split differently: i32, i128
; (limbs in Fir_wide), f16 (an i16 inside), ptr and a vector (lanes in
; Fir_scalarize); f80 (in memory) is in secret80.fir. Expected values are worked out in the comments.
triple "x86_64-unknown-linux-gnu"
declare @putnum (i64) -> () global

global @g size 8 align 8 = zero

func @main () -> (i32) global {
^e:
  %a = secret i32 i32 40
  %b = add i32 %a, i32 2
  %c = declassify i32 %b
  %c64 = zext i64 %c
  call (i64) -> () @putnum(%c64)                       ; 42
  %w = secret i128 i128 0x10000000000000005
  %w2 = lshr i128 %w, i128 64
  %w3 = add i128 %w2, %w
  %w4 = declassify i128 %w3
  %wl = trunc i64 %w4
  call (i64) -> () @putnum(%wl)                        ; 1 + 5 = 6
  %h = sitofp f16 i32 3
  %hs = secret f16 %h
  %hd = fadd f16 %hs, %hs
  %hp = declassify f16 %hd
  %hi = fptosi i64 %hp
  call (i64) -> () @putnum(%hi)                        ; 6
  %p = secret ptr ptr @g
  store i64 11, %p, align 8
  %pd = declassify ptr %p
  %l = load i64 %pd, align 8
  call (i64) -> () @putnum(%l)                         ; 11
  %v = splat <4 x i32> i32 3
  %vs = secret <4 x i32> %v
  %vm = mul <4 x i32> %vs, %vs
  %vd = declassify <4 x i32> %vm
  %r = reduce add i32 %vd
  %r64 = zext i64 %r
  call (i64) -> () @putnum(%r64)                       ; 4 * 9 = 36
  ret i32 0
}
