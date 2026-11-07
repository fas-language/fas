; agg by value between Fir functions: register and MEMORY classes, results fed
; back as arguments, a recursive struct-returning function, and an align-16
; agg that SysV classification skips (passed by address between local functions)
; llvm: skip aggregates by value (C ABI lowering for LLVM, roadmap v0.7)
triple "x86_64-unknown-linux-gnu"
declare @putnum (i64) -> () global
; {i64 a; f64 d} -> {f64 a*d; i64 a+1}
func @swap (agg(16, 8, [0: i64, 8: f64])) -> (agg(16, 8, [0: f64, 8: i64])) local {
^e(%p: ptr):
  %a = load i64 %p, align 8
  %q = ptradd ptr %p, i64 0, 1, 8
  %d = load f64 %q, align 8
  %af = sitofp f64 %a
  %m = fmul f64 %af, %d
  %s = alloca ptr i64 16, align 8
  store %m, %s, align 8
  %s1 = ptradd ptr %s, i64 0, 1, 8
  %a1 = add i64 %a, i64 1
  store %a1, %s1, align 8
  ret %s
}
; fib pair {f(n), f(n+1)} as a 32-byte MEMORY struct (two unused words)
func @fibp (i64) -> (agg(32, 8, [0: i64, 8: i64, 16: i64, 24: i64])) local noinline {
^e(%n: i64):
  %s = alloca ptr i64 32, align 8
  %z = icmp eq i1 %n, i64 0
  br %z, ^base, ^rec
^base:
  store i64 0, %s, align 8
  %b1 = ptradd ptr %s, i64 0, 1, 8
  store i64 1, %b1, align 8
  jump ^done
^rec:
  %n1 = sub i64 %n, i64 1
  %r = call (i64) -> (agg(32, 8, [0: i64, 8: i64, 16: i64, 24: i64])) @fibp(%n1)
  %x = load i64 %r, align 8
  %rq = ptradd ptr %r, i64 0, 1, 8
  %y = load i64 %rq, align 8
  %xy = add i64 %x, %y
  store %y, %s, align 8
  %s1 = ptradd ptr %s, i64 0, 1, 8
  store %xy, %s1, align 8
  jump ^done
^done:
  %s2 = ptradd ptr %s, i64 0, 1, 16
  store %n, %s2, align 8
  %s3 = ptradd ptr %s, i64 0, 1, 24
  store i64 -1, %s3, align 8
  ret %s
}
; align 16 is beyond the SysV classifier: the backend's own by-address form
func @over (agg(32, 16, [0: i64, 16: i64])) -> (i64) local noinline {
^e(%p: ptr):
  %a = load i64 %p, align 16
  %q = ptradd ptr %p, i64 0, 1, 16
  %b = load i64 %q, align 16
  %r = sub i64 %a, %b
  ret %r
}
func @main () -> (i32) global {
^e:
  %s = alloca ptr i64 16, align 8
  store i64 6, %s, align 8
  %q = ptradd ptr %s, i64 0, 1, 8
  store f64 0x3ff8000000000000, %q, align 8
  %t = call (agg(16, 8, [0: i64, 8: f64])) -> (agg(16, 8, [0: f64, 8: i64])) @swap(%s)
  %m = load f64 %t, align 8
  %mi = fptosi i64 %m
  call (i64) -> () @putnum(%mi)
  %tq = ptradd ptr %t, i64 0, 1, 8
  %a1 = load i64 %tq, align 8
  call (i64) -> () @putnum(%a1)
  %f = call (i64) -> (agg(32, 8, [0: i64, 8: i64, 16: i64, 24: i64])) @fibp(i64 50)
  %f0 = load i64 %f, align 8
  call (i64) -> () @putnum(%f0)
  %fq = ptradd ptr %f, i64 0, 1, 16
  %fn = load i64 %fq, align 8
  call (i64) -> () @putnum(%fn)
  %o = alloca ptr i64 32, align 16
  store i64 100, %o, align 16
  %oq = ptradd ptr %o, i64 0, 1, 16
  store i64 58, %oq, align 16
  %ov = call (agg(32, 16, [0: i64, 16: i64])) -> (i64) @over(%o)
  call (i64) -> () @putnum(%ov)
  ret i32 0
}
