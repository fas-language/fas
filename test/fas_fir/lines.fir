; Source locations (`loc` table, `!loc N`): fas-fir -g turns them into DWARF
; line tables (test/native_debug.sh checks those); without -g they change
; nothing. Two "source files", a location shared by two instructions, a
; location only on a terminator, and instructions without one.
; Prints 42, exits 0.
triple "x86_64-unknown-linux-gnu"
loc 1 "lines.c" 3 5
loc 2 "lines.c" 4 12
loc 3 "lines.c" 9 3
loc 4 "lines.c" 10 3
loc 5 "inc/helper.h" 7 10
loc 6 "lines.c" 11 1

declare @putnum (i64) -> () global

func @twice (i64) -> (i64) local {
^entry(%x: i64):
  %a = add i64 %x, %x !loc 5
  ret %a !loc 5
}

func @main (i32, ptr) -> (i32) global {
^entry(%argc: i32, %argv: ptr):
  %c = sext i64 %argc !loc 1
  %d = add i64 %c, i64 20 !loc 1
  %t = call (i64) -> (i64) @twice(%d) !loc 2
  jump ^next !loc 3
^next:
  call (i64) -> () @putnum(%t) !loc 4
  %z = sub i64 %t, %t
  %r = trunc i32 %z
  ret %r !loc 6
}
