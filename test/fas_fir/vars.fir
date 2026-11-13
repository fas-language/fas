; Source variables (`dtype`, `scope`, `var ... at %slot`): fas-fir -g turns
; them into DWARF variables and lexical blocks (test/native_debug.sh asks
; gdb for their values at vars.c:7); without -g they change nothing. A
; parameter, promoted locals (in a register or spilled), a struct in a
; frame slot, an over-aligned local (found through its address) and a
; shadowed `x` in an inner block.
; Prints 10, 30, 60, 114, exits 0.
triple "x86_64-unknown-linux-gnu"
loc 1 "vars.c" 1 1
loc 2 "vars.c" 2 3
loc 3 "vars.c" 3 3 !scope 1
loc 4 "vars.c" 4 3
loc 5 "vars.c" 5 5 !scope 1
loc 6 "vars.c" 6 7 !scope 2
loc 7 "vars.c" 7 7 !scope 2
loc 8 "vars.c" 9 3
scope 1 parent 0
scope 2 parent 1
dtype 1 base "long" signed 8
dtype 2 struct "point" size 16 {"x" 1 0, "y" 1 8}
dtype 3 pointer 2

declare @putnum (i64) -> () global

func @work (i64) -> () local {
  var "n" 1 arg 1 !loc 1 at %n.addr
  var "sum" 1 !loc 2 at %sum
  var "x" 1 !loc 2 at %xo
  var "pt" 2 !loc 4 at %pt
  var "ppt" 3 !loc 4 at %ppt
  var "w" 1 !loc 4 at %w
  var "i" 1 scope 1 !loc 3 at %i
  var "x" 1 scope 2 !loc 6 at %xi
^entry(%n: i64):
  %n.addr = alloca ptr i64 8, align 8
  %sum = alloca ptr i64 8, align 8
  %xo = alloca ptr i64 8, align 8
  %pt = alloca ptr i64 16, align 8
  %ppt = alloca ptr i64 8, align 8
  %i = alloca ptr i64 8, align 8
  %xi = alloca ptr i64 8, align 8
  %w = alloca ptr i64 8, align 32
  store i64 7, %w, align 8 !loc 4
  store %n, %n.addr, align 8 !loc 1
  store i64 0, %sum, align 8 !loc 2
  store i64 100, %xo, align 8 !loc 2
  store i64 3, %pt, align 8 !loc 4
  %pty = ptradd ptr %pt, i64 0, 1, 8 !loc 4
  store i64 4, %pty, align 8 !loc 4
  store %pt, %ppt, align 8 !loc 4
  store i64 1, %i, align 8 !loc 3
  jump ^head !loc 3
^head:
  %iv = load i64 %i, align 8 !loc 3
  %nv = load i64 %n.addr, align 8 !loc 3
  %done = icmp sgt i1 %iv, %nv !loc 3
  br %done, ^exit, ^body !loc 3
^body:
  %s0 = load i64 %sum, align 8 !loc 5
  %s1 = add i64 %s0, %iv !loc 5
  store %s1, %sum, align 8 !loc 5
  %x = mul i64 %s1, i64 10 !loc 6
  store %x, %xi, align 8 !loc 6
  %xv = load i64 %xi, align 8 !loc 7
  call (i64) -> () @putnum(%xv) !loc 7
  %i2 = add i64 %iv, i64 1 !loc 3
  store %i2, %i, align 8 !loc 3
  jump ^head !loc 3
^exit:
  %q = load ptr %ppt, align 8 !loc 8
  %a = load i64 %q, align 8 !loc 8
  %qy = ptradd ptr %q, i64 0, 1, 8 !loc 8
  %b = load i64 %qy, align 8 !loc 8
  %c0 = add i64 %a, %b !loc 8
  %xov = load i64 %xo, align 8 !loc 8
  %c1 = add i64 %c0, %xov !loc 8
  %wv = load i64 %w, align 8 !loc 8
  %c = add i64 %c1, %wv !loc 8
  call (i64) -> () @putnum(%c) !loc 8
  ret !loc 8
}

func @main (i32, ptr) -> (i32) global {
^entry(%argc: i32, %argv: ptr):
  call (i64) -> () @work(i64 3)
  ret i32 0
}
