; switch with and without edge weights (SPEC §7: hints, no behaviour), on
; i32 and on i128 (split into limbs by Fir_wide, which drops the weights)
triple "x86_64-unknown-linux-gnu"

declare @putnum (i64) -> () global

func @main () -> (i32) global {
^e:
  jump ^l(i32 0, i64 0)
^l(%i: i32, %acc: i64):
  %d = icmp sge i1 %i, i32 6
  br %d, ^x, ^s, weight 100
^s:
  switch %i, ^dflt, [0: ^a, 2: ^b(i64 20), -7: ^b(i64 7)], weights [1, 50, 30, 0]
^a:
  %a1 = add i64 %acc, i64 1
  jump ^n(%a1)
^b(%k: i64):
  %b1 = add i64 %acc, %k
  jump ^n(%b1)
^dflt:
  %c1 = add i64 %acc, i64 100
  jump ^n(%c1)
^n(%acc2: i64):
  %i2 = add i32 %i, i32 1
  jump ^l(%i2, %acc2)
^x:
  call (i64) -> () @putnum(%acc)                     ; 1 + 100 + 20 + 3*100 = 421
  %w = zext i128 %i
  switch %w, ^wd, [6: ^w6, 0x10000000000000006: ^wd], weights [5, 10, 0]
^w6:
  call (i64) -> () @putnum(i64 6)
  switch %i, ^z, []
^wd:
  call (i64) -> () @putnum(i64 0)
  ret i32 1
^z:
  ret i32 0
}
