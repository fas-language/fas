; clmul / clmul_hi at odd and wide widths (Fir_wide limbs, the
; __fir_clmul helper for legal ones), expected from a bit-by-bit reference.
; Each result is printed as 32-bit pieces, low first.
triple "x86_64-unknown-linux-gnu"
declare @putnum (i64) -> () global

func @p32 (i64) -> () local {
^e(%x: i64):
  %lo = and i64 %x, i64 4294967295
  call (i64) -> () @putnum(%lo)
  %hi = lshr i64 %x, i64 32
  call (i64) -> () @putnum(%hi)
  ret
}

func @c7 (i7, i7) -> () local {
^e(%a: i7, %b: i7):
  %l = clmul i7 %a, %b
  %lz0 = zext i64 %l
  call (i64) -> () @p32(%lz0)
  %h = clmul_hi i7 %a, %b
  %hz0 = zext i64 %h
  call (i64) -> () @p32(%hz0)
  ret
}

func @c37 (i37, i37) -> () local {
^e(%a: i37, %b: i37):
  %l = clmul i37 %a, %b
  %lz0 = zext i64 %l
  call (i64) -> () @p32(%lz0)
  %h = clmul_hi i37 %a, %b
  %hz0 = zext i64 %h
  call (i64) -> () @p32(%hz0)
  ret
}

func @c64 (i64, i64) -> () local {
^e(%a: i64, %b: i64):
  %l = clmul i64 %a, %b
  call (i64) -> () @p32(%l)
  %h = clmul_hi i64 %a, %b
  call (i64) -> () @p32(%h)
  ret
}

func @c100 (i100, i100) -> () local {
^e(%a: i100, %b: i100):
  %l = clmul i100 %a, %b
  %ls0 = lshr i100 %l, i100 0
  %lz0 = trunc i64 %ls0
  call (i64) -> () @p32(%lz0)
  %ls1 = lshr i100 %l, i100 64
  %lz1 = trunc i64 %ls1
  call (i64) -> () @p32(%lz1)
  %h = clmul_hi i100 %a, %b
  %hs0 = lshr i100 %h, i100 0
  %hz0 = trunc i64 %hs0
  call (i64) -> () @p32(%hz0)
  %hs1 = lshr i100 %h, i100 64
  %hz1 = trunc i64 %hs1
  call (i64) -> () @p32(%hz1)
  ret
}

func @c128 (i128, i128) -> () local {
^e(%a: i128, %b: i128):
  %l = clmul i128 %a, %b
  %ls0 = lshr i128 %l, i128 0
  %lz0 = trunc i64 %ls0
  call (i64) -> () @p32(%lz0)
  %ls1 = lshr i128 %l, i128 64
  %lz1 = trunc i64 %ls1
  call (i64) -> () @p32(%lz1)
  %h = clmul_hi i128 %a, %b
  %hs0 = lshr i128 %h, i128 0
  %hz0 = trunc i64 %hs0
  call (i64) -> () @p32(%hz0)
  %hs1 = lshr i128 %h, i128 64
  %hz1 = trunc i64 %hs1
  call (i64) -> () @p32(%hz1)
  ret
}

func @c13 (i13, i13) -> () local {
^e(%a: i13, %b: i13):
  %l = clmul i13 %a, %b
  %lz0 = zext i64 %l
  call (i64) -> () @p32(%lz0)
  %h = clmul_hi i13 %a, %b
  %hz0 = zext i64 %h
  call (i64) -> () @p32(%hz0)
  ret
}

func @c65 (i65, i65) -> () local {
^e(%a: i65, %b: i65):
  %l = clmul i65 %a, %b
  %ls0 = lshr i65 %l, i65 0
  %lz0 = trunc i64 %ls0
  call (i64) -> () @p32(%lz0)
  %ls1 = lshr i65 %l, i65 64
  %lz1 = trunc i64 %ls1
  call (i64) -> () @p32(%lz1)
  %h = clmul_hi i65 %a, %b
  %hs0 = lshr i65 %h, i65 0
  %hz0 = trunc i64 %hs0
  call (i64) -> () @p32(%hz0)
  %hs1 = lshr i65 %h, i65 64
  %hz1 = trunc i64 %hs1
  call (i64) -> () @p32(%hz1)
  ret
}

func @main () -> (i32) global {
^e:
  call (i7, i7) -> () @c7(i7 85, i7 127)
  call (i37, i37) -> () @c37(i37 133396631311, i37 78187493521)
  call (i64, i64) -> () @c64(i64 18364758544493064720, i64 9223372036854775809)
  %a100l = zext i100 i64 5001117282205630755
  %a100h0 = zext i100 i64 34359738659
  %a100h = shl i100 %a100h0, i100 64
  %a100 = or i100 %a100h, %a100l
  %b100l = zext i100 i64 18446744073709551615
  %b100h0 = zext i100 i64 68719476735
  %b100h = shl i100 %b100h0, i100 64
  %b100 = or i100 %b100h, %b100l
  call (i100, i100) -> () @c100(%a100, %b100)
  %a128l = zext i128 i64 81985529216486895
  %a128h0 = zext i128 i64 16045690984503098046
  %a128h = shl i128 %a128h0, i128 64
  %a128 = or i128 %a128h, %a128l
  %b128l = zext i128 i64 3
  %b128h0 = zext i128 i64 18446744073709551615
  %b128h = shl i128 %b128h0, i128 64
  %b128 = or i128 %b128h, %b128l
  call (i128, i128) -> () @c128(%a128, %b128)
  call (i13, i13) -> () @c13(i13 6844, i13 8191)
  %a65l = zext i65 i64 5
  %a65h0 = zext i65 i64 1
  %a65h = shl i65 %a65h0, i65 64
  %a65 = or i65 %a65h, %a65l
  %b65l = zext i65 i64 3
  %b65h0 = zext i65 i64 1
  %b65h = shl i65 %b65h0, i65 64
  %b65 = or i65 %b65h, %b65l
  call (i65, i65) -> () @c65(%a65, %b65)
  ret i32 0
}
