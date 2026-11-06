; plain fptosi / fptoui (non-saturating): in-range values only, since
; out-of-range inputs are poison. Covers u64 above 2^63, where a plain
; cvttsd2si would overflow.
triple "x86_64-unknown-linux-gnu"
declare @putnum (i64) -> () global

func @show (f64) -> () local {
^e(%x: f64):
  %a = fptosi i32 %x
  %a64 = sext i64 %a
  call (i64) -> () @putnum(%a64)
  %b = fptoui i64 %x
  call (i64) -> () @putnum(%b)
  %f = fptrunc f32 %x
  %c = fptoui i16 %f
  %c64 = zext i64 %c
  call (i64) -> () @putnum(%c64)
  %d = fptosi i8 %f
  %d64 = sext i64 %d
  %d1 = add i64 %d64, i64 1000
  call (i64) -> () @putnum(%d1)
  ret
}

func @big (f64) -> () local {
^e(%x: f64):
  %b = fptoui i64 %x
  call (i64) -> () @putnum(%b)
  %f = fptrunc f32 %x
  %c = fptoui i64 %f
  call (i64) -> () @putnum(%c)
  ret
}

func @main (i32, ptr) -> (i32) global {
^e(%argc: i32, %argv: ptr):
  call (f64) -> () @show(f64 0x4045400000000000)   ; 42.5
  call (f64) -> () @show(f64 0x405fc00000000000)   ; 127.0
  call (f64) -> () @show(f64 0x3fefffffffffffff)   ; just below 1.0
  call (f64) -> () @big(f64 0x43e0000000000000)    ; 2^63
  call (f64) -> () @big(f64 0x43e0000000000001)    ; 2^63 + 2048
  call (f64) -> () @big(f64 0x43efffffe0000000)    ; 2^64 - 2^40 (exact in f32 too)
  call (f64) -> () @big(f64 0x43dfffffffffffff)    ; 2^63 - 1024
  ret i32 0
}
