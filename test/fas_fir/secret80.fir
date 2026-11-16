; Constant-time marks on f80, which the native backend keeps in memory
; (SPEC §8); apart from secret.fir as the interpreter has no f80 arithmetic
triple "x86_64-unknown-linux-gnu"
declare @putnum (i64) -> () global

func @main () -> (i32) global {
^e:
  %x = sitofp f80 i32 7
  %xs = secret f80 %x
  %xm = fmul f80 %xs, %xs
  %xd = declassify f80 %xm
  %xi = fptosi i64 %xd
  call (i64) -> () @putnum(%xi)                        ; 49
  ret i32 0
}
