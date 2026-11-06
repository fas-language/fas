; "sum=55", then exit with argc: block parameters instead of phis,
; globals with relocations, a declared function, a raw write syscall
; exit: 1
triple "x86_64-unknown-linux-gnu"

global @msg size 4 align 1 local readonly = {bytes "sum="}
global @counter size 8 align 8 local = zero
global @table size 16 align 8 local readonly = {reloc @msg w8, reloc @msg+2 w8}

declare @putnum (i64) -> () global

; sum of 1..n with block parameters instead of phis
func @sum (i64) -> (i64) local {
^entry(%n: i64):
  jump ^loop(i64 1, i64 0)
^loop(%i: i64, %acc: i64):
  %done = icmp ugt i1 %i, %n
  br %done, ^exit, ^body
^body:
  %acc2 = add i64 %acc, %i
  %i2 = add nsw i64 %i, i64 1
  jump ^loop(%i2, %acc2)
^exit:
  ret %acc
}

func @main (i32, ptr) -> (i32) global {
^entry(%argc: i32, %argv: ptr):
  %s = call (i64) -> (i64) @sum(i64 10)
  %p = load ptr ptr @table, align 8
  %pi = ptrtoint i64 %p
  %w = syscall i64 i64 1, i64 1, %pi, i64 4
  call (i64) -> () @putnum(%s)
  %c = load i64 ptr @counter, align 8
  %c1 = add i64 %c, i64 1
  store %c1, ptr @counter, align 8
  ret %argc
}
