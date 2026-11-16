; Constant-time marks on f80, which the native backend keeps in memory
; (SPEC §8); apart from secret.fir as the interpreter has no f80 arithmetic.
; No float operation may take a secret (§8), so this covers what an f80
; secret may do: be chosen by a secret condition (a cmov), stored and loaded
; through a public address, and declassified. (Until v0.7 it multiplied a
; secret f80, which Fir_ct now refuses.)
triple "x86_64-unknown-linux-gnu"
declare @putnum (i64) -> () global

func @main () -> (i32) global {
^e:
  %x = sitofp f80 i32 7
  %y = sitofp f80 i32 9
  %xs = secret f80 %x
  %ys = secret f80 %y
  %k = secret i32 i32 3
  %c = icmp ult i1 %k, i32 5
  %z = select f80 %c, %ys, %xs
  %p = alloca ptr i64 16, align 16
  store %z, %p, align 16
  %l = load f80 %p, align 16
  %d = declassify f80 %l
  %di = fptosi i64 %d
  %sq = mul i64 %di, %di
  call (i64) -> () @putnum(%sq)                        ; 81
  ret i32 0
}
