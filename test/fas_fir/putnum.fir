; putnum(x): writes x in decimal and a newline with write(2).
; Linked into every test of test/fas_fir.sh.
triple "x86_64-unknown-linux-gnu"
global @nl size 1 align 1 local readonly = {bytes "\0a"}
func @putnum (i64) -> () global {
^entry(%x: i64):
  %buf = alloca ptr i64 32, align 8
  jump ^digit(%x, i64 31)
^digit(%v: i64, %pos: i64):
  %q = udiv i64 %v, i64 10
  %r = urem i64 %v, i64 10
  %r8 = trunc i8 %r
  %ch = add i8 %r8, i8 48
  %at = ptradd ptr %buf, %pos, 1, 0
  store %ch, %at, align 1
  %more = icmp ne i1 %q, i64 0
  %pos1 = sub i64 %pos, i64 1
  br %more, ^digit(%q, %pos1), ^out(%pos)
^out(%start: i64):
  %from = ptradd ptr %buf, %start, 1, 0
  %len = sub i64 i64 32, %start
  %fromi = ptrtoint i64 %from
  %w = syscall i64 i64 1, i64 1, %fromi, %len
  %nli = ptrtoint i64 ptr @nl
  %w2 = syscall i64 i64 1, i64 1, %nli, i64 1
  ret
}
