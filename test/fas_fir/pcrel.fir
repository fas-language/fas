; reloc ... pcrel w4 (SPEC §4): a table of 32-bit offsets, each the target
; minus the entry's own address. 11 + 44 + 33.
; exit: 88
triple "x86_64-unknown-linux-gnu"
global @a size 8 align 8 local = {bytes "\0b\00\00\00\00\00\00\00"}
global @b size 8 align 8 global = {bytes "\16\00\00\00\2c\00\00\00"}
global @c size 8 align 8 local = {bytes "\21\00\00\00\00\00\00\00"}
; a relative table: entry k = target - &entry k
global @tab size 16 align 4 local readonly = {reloc @a pcrel w4, reloc @b+4 pcrel w4, zeros 4, reloc @c pcrel w4}
func @get (i64) -> (i32) local {
^e(%k: i64):
  %p = ptradd ptr ptr @tab, %k, 4, 0
  %d = load i32 %p, align 4
  %d64 = sext i64 %d
  %q = ptradd ptr %p, %d64, 1, 0
  %v = load i32 %q, align 4
  ret %v
}
func @main () -> (i32) global {
^e:
  %x = call (i64) -> (i32) @get(i64 0)
  %y = call (i64) -> (i32) @get(i64 1)
  %z = call (i64) -> (i32) @get(i64 3)
  %t = add i32 %x, %y
  %t2 = add i32 %t, %z
  ret %t2
}
