; Frame shapes for the unwinder (test/native_unwind.sh): a leaf; dynamic
; stack allocation in a loop with a call inside it and two returns; an i128
; division (a Fir_wide runtime helper); fma (a Fas runtime helper); inline
; asm that moves rsp; a trap. The single-step walk checks that every
; instruction of a run unwinds back to main. Prints 13, 35, 7, 7, 10.
triple "x86_64-unknown-linux-gnu"

declare @putnum (i64) -> () global

func @leaf (i64) -> (i64) local {
^e(%x: i64):
  %y = mul i64 %x, i64 3
  %z = add i64 %y, i64 1
  ret %z
}

func @dyn (i64) -> (i64) local {
^e(%n: i64):
  jump ^loop(i64 0, i64 0)
^loop(%i: i64, %acc: i64):
  %bytes = mul i64 %n, i64 8
  %a = alloca ptr %bytes, align 16
  %v = call (i64) -> (i64) @leaf(%i)
  store %v, %a, align 8
  %w = load i64 %a, align 8
  %acc1 = add i64 %acc, %w
  %i1 = add i64 %i, i64 1
  %more = icmp ult i1 %i1, %n
  br %more, ^loop(%i1, %acc1), ^done(%acc1)
^done(%r: i64):
  %big = icmp ugt i1 %r, i64 1000
  br %big, ^early, ^late(%r)
^early:
  ret i64 0
^late(%s: i64):
  ret %s
}

func @wide (i64, i64) -> (i64) local {
^e(%a: i64, %b: i64):
  %x = sext i128 %a
  %y = sext i128 %b
  %q = sdiv i128 %x, %y
  %r = trunc i64 %q
  ret %r
}

func @fused (f64, f64, f64) -> (f64) local {
^e(%a: f64, %b: f64, %c: f64):
  %r = fma f64 %a, %b, %c
  ret %r
}

func @pushy (i64) -> (i64) local {
^e(%x: i64):
  %r = asm volatile "push %1\0a\09pop %0" out("=r" i64) in("r" i64 %x)
  %s = call (i64) -> (i64) @leaf(%r)
  ret %s
}

func @boom (i64) -> () local {
^e(%x: i64):
  %z = icmp eq i1 %x, i64 0
  br %z, ^ok, ^bad
^ok:
  ret
^bad:
  trap
  unreachable
}

func @main (i32, ptr) -> (i32) global {
^e(%argc: i32, %argv: ptr):
  %a = sext i64 %argc
  %l = call (i64) -> (i64) @leaf(i64 4)
  call (i64) -> () @putnum(%l)
  %d = call (i64) -> (i64) @dyn(i64 5)
  call (i64) -> () @putnum(%d)
  %w = call (i64, i64) -> (i64) @wide(i64 -21, i64 -3)
  call (i64) -> () @putnum(%w)
  %f = call (f64, f64, f64) -> (f64) @fused(f64 0x4000000000000000, f64 0x4008000000000000, f64 0x3ff0000000000000)
  %fi = fptosi i64 %f
  call (i64) -> () @putnum(%fi)
  %p = call (i64) -> (i64) @pushy(i64 3)
  %a1 = sub i64 %a, i64 1
  call (i64) -> () @boom(%a1)
  call (i64) -> () @putnum(%p)
  ret i32 0
}
