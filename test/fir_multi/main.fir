; Several results (SPEC §5): the caller side of lib.fir.
triple "x86_64-unknown-linux-gnu"
declare @putnum (i64) -> () global
declare @m1 (i64) -> (f32, i32, f64, zext i8) global
declare @m3 (i64, i64, i64, i64, i64, i64, i64) -> (i64, i64, i64) global
declare @m2 (i64) -> (i64, ptr, i8, f64, f64, f64, sext i16, f16, f80, i128, <4 x i32>, agg(24, 8, [0: i64, 8: i64, 16: i64]), i100, f32) global

func @main () -> (i32) global {
^e:
  %f, %i, %d, %b = call (i64) -> (f32, i32, f64, zext i8) @m1(i64 4)
  %f1 = fptosi i64 %f
  call (i64) -> () @putnum(%f1)                         ; 4
  %i1 = sext i64 %i
  call (i64) -> () @putnum(%i1)                         ; 12
  %d1 = fptosi i64 %d
  call (i64) -> () @putnum(%d1)                         ; 8
  %b1 = zext i64 %b
  call (i64) -> () @putnum(%b1)                         ; 200
  %p, %q, %r = call (i64, i64, i64, i64, i64, i64, i64) -> (i64, i64, i64) @m3(i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, i64 7)
  call (i64) -> () @putnum(%p)                          ; 3
  call (i64) -> () @putnum(%q)                          ; 12
  call (i64) -> () @putnum(%r)                          ; 18
  %a1, %a2, %a3, %a4, %a5, %a6, %a7, %a8, %a9, %a10, %a11, %a12, %a13, %a14 = call (i64) -> (i64, ptr, i8, f64, f64, f64, sext i16, f16, f80, i128, <4 x i32>, agg(24, 8, [0: i64, 8: i64, 16: i64]), i100, f32) @m2(i64 5)
  call (i64) -> () @putnum(%a1)                         ; 6
  %g = load i64 %a2, align 8
  call (i64) -> () @putnum(%g)                          ; 77
  %c3 = zext i64 %a3
  call (i64) -> () @putnum(%c3)                         ; 7
  %c4 = fptosi i64 %a4
  call (i64) -> () @putnum(%c4)                         ; 10
  %a5x = fadd f64 %a5, %a5
  %c5 = fptosi i64 %a5x
  call (i64) -> () @putnum(%c5)                         ; 5
  %c6 = fptosi i64 %a6
  call (i64) -> () @putnum(%c6)                         ; 3
  %a7x = sext i64 %a7
  %c7 = add i64 %a7x, i64 10
  call (i64) -> () @putnum(%c7)                         ; 7
  %c8 = fptosi i64 %a8
  call (i64) -> () @putnum(%c8)                         ; 4
  %c9 = fptosi i64 %a9
  call (i64) -> () @putnum(%c9)                         ; 9
  %w = lshr i128 %a10, i128 64
  %w1 = add i128 %w, %a10
  %c10 = trunc i64 %w1
  call (i64) -> () @putnum(%c10)                        ; 1 + 5 = 6
  %c11 = reduce add i32 %a11
  %c11x = zext i64 %c11
  call (i64) -> () @putnum(%c11x)                       ; 10
  %e0 = load i64 %a12, align 8
  %e1p = ptradd ptr %a12, i64 0, 1, 8
  %e1 = load i64 %e1p, align 8
  %e2p = ptradd ptr %a12, i64 0, 1, 16
  %e2 = load i64 %e2p, align 8
  %e01 = add i64 %e0, %e1
  %c12 = add i64 %e01, %e2
  call (i64) -> () @putnum(%c12)                        ; 36
  %v = lshr i100 %a13, i100 64
  %v1 = add i100 %v, %a13
  %c13 = trunc i64 %v1
  call (i64) -> () @putnum(%c13)                        ; 64 + 8 = 72
  %c14 = fptosi i64 %a14
  call (i64) -> () @putnum(%c14)                        ; 21
  ret i32 0
}
