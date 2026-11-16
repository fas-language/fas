; constant-time marks (SPEC §8): every operation allowed on a secret, over
; i8..i128, must select and legalize with no branch, division or address
; from the secret; Fir_ct and Mir_ct check it on every native build
triple "x86_64-unknown-linux-gnu"

declare @putnum (i64) -> () global

func @ops (i64, i64, i32) -> (i64) {
^e(%a0: i64, %b0: i64, %n0: i32):
  %a = secret i64 %a0
  %b = secret i64 %b0
  %n = secret i32 %n0
  %c = and i32 %n, i32 63
  %c64 = zext i64 %c
  %x1 = add i64 %a, %b
  %x2 = mul i64 %x1, i64 0x9e3779b97f4a7c15
  %x3 = shl i64 %x2, %c64
  %x4 = lshr i64 %x3, %c64
  %x5 = ashr i64 %x2, %c64
  %x6 = fshl i64 %x5, %x5, %c64
  %x7 = xor i64 %x4, %x6
  %x8 = smin i64 %x7, %a
  %x9 = umax i64 %x8, %b
  %h = umulh i64 %x9, %a
  %s = smulh i64 %x9, %b
  %u = uadd_sat i64 %h, %s
  %v = ssub_sat i64 %u, %a
  %w = abs i64 %v
  %y = bswap i64 %w
  %lt = icmp ult i1 %a, %b
  %z = select i64 %lt, %y, %x9
  %t8 = trunc i8 %z
  %e8 = sext i64 %t8
  %r1, %o1 = uadd_ovf (i64, i1) %z, %e8
  %o64 = zext i64 %o1
  %r2 = add i64 %r1, %o64
  %r3, %o2 = smul_ovf (i64, i1) %r2, %b
  %o3 = zext i64 %o2
  %r4 = or i64 %r3, %o3
  %d = declassify i64 %r4
  ret %d
}

func @wide (i64, i64, i32) -> (i64) {
^e(%a0: i64, %b0: i64, %n0: i32):
  %a1 = zext i128 %a0
  %a = secret i128 %a1
  %bs = secret i64 %b0
  %b = sext i128 %bs
  %n = secret i32 %n0
  %c = and i32 %n, i32 127
  %c128 = zext i128 %c
  %x1 = mul i128 %a, %b
  %x2 = add i128 %x1, %a
  %x3 = shl i128 %x2, %c128
  %x4 = lshr i128 %x3, %c128
  %x5 = ashr i128 %x2, %c128
  %x6 = xor i128 %x4, %x5
  %lt = icmp slt i1 %x6, %b
  %z = select i128 %lt, %x6, %x2
  %lo = trunc i64 %z
  %hs = lshr i128 %z, i128 64
  %hi = trunc i64 %hs
  %m = xor i64 %lo, %hi
  %d = declassify i64 %m
  ret %d
}

; a constant-time compare: public loads, marked secret, folded with no branch
func @cteq (ptr, ptr, i64) -> (i1) {
^e(%p: ptr, %q: ptr, %len: i64):
  jump ^l(i64 0, i8 0)
^l(%i: i64, %acc: i8):
  %more = icmp ult i1 %i, %len
  br %more, ^b, ^x
^b:
  %pa = ptradd ptr %p, %i, 1, 0
  %qa = ptradd ptr %q, %i, 1, 0
  %la = load i8 %pa, align 1
  %lb = load i8 %qa, align 1
  %sa = secret i8 %la
  %sb = secret i8 %lb
  %df = xor i8 %sa, %sb
  %acc2 = or i8 %acc, %df
  %i2 = add i64 %i, i64 1
  jump ^l(%i2, %acc2)
^x:
  %z = icmp eq i1 %acc, i8 0
  %r = declassify i1 %z
  ret %r
}

func @main () -> (i32) global {
^e:
  %r = call (i64, i64, i32) -> (i64) @ops(i64 123456789, i64 -987654321, i32 13)
  call (i64) -> () @putnum(%r)
  %w = call (i64, i64, i32) -> (i64) @wide(i64 0x0123456789abcdef, i64 -5, i32 70)
  call (i64) -> () @putnum(%w)
  %p = alloca ptr i64 8, align 8
  %q = alloca ptr i64 8, align 8
  store i64 0x1122334455667788, %p, align 8
  store i64 0x1122334455667788, %q, align 8
  %e1 = call (ptr, ptr, i64) -> (i1) @cteq(%p, %q, i64 8)
  %e1w = zext i64 %e1
  call (i64) -> () @putnum(%e1w)
  store i8 0, %q, align 1
  %e2 = call (ptr, ptr, i64) -> (i1) @cteq(%p, %q, i64 8)
  %e2w = zext i64 %e2
  call (i64) -> () @putnum(%e2w)
  ret i32 0
}
