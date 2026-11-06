; Aliases and weak symbols through the native static link (no C compiler):
; a weak definition nothing overrides, a weak undefined function and global
; (address 0), function and data aliases. Prints 42, 7, 0, 22, 22.
; (c/symbols.fir checks overriding, against C.)
triple "x86_64-unknown-linux-gnu"

declare @putnum (i64) -> () global
declare @absent () -> (i64) weak
global @absent_data size 8 align 8 weak = extern
global @pair size 16 align 8 local = {bytes "\0b\00\00\00\00\00\00\00\16\00\00\00\00\00\00\00"}
alias @second local = @pair + 8
alias @twice_alias global = @twice
alias @seven_alias weak hidden = @seven

func @twice (i64) -> (i64) local {
^e(%x: i64):
  %y = add i64 %x, %x
  ret %y
}

func @seven () -> (i64) weak {
^e:
  ret i64 7
}

func @main (i32, ptr) -> (i32) global {
^e(%argc: i32, %argv: ptr):
  %a = call (i64) -> (i64) @twice_alias(i64 21)
  call (i64) -> () @putnum(%a)
  %b = call () -> (i64) @seven_alias()
  call (i64) -> () @putnum(%b)
  %p = ptrtoint i64 ptr @absent
  %q = ptrtoint i64 ptr @absent_data
  %o = or i64 %p, %q
  %nz = icmp ne i1 %o, i64 0
  br %nz, ^called, ^skip
^called:
  %c = call () -> (i64) @absent()
  call (i64) -> () @putnum(%c)
  ret i32 1
^skip:
  call (i64) -> () @putnum(%o)
  %s = load i64 ptr @second, align 8
  call (i64) -> () @putnum(%s)
  %t = ptradd ptr ptr @pair, i64 0, 1, 8
  %u = load i64 %t, align 8
  call (i64) -> () @putnum(%u)
  ret i32 0
}
