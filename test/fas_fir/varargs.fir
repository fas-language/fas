; variadic definitions: va_start / va_arg over the register save area and
; the stack overflow area (more than 6 integer and 8 double arguments),
; mixed classes, narrow and pointer reads, and va_copy as a 24-byte memcpy
triple "x86_64-unknown-linux-gnu"
global @word size 8 align 8 local = {bytes "\2a\00\00\00\00\00\00\00"}

declare @putnum (i64) -> () global

; sum of n i64 arguments, read twice: once through the va_list and once
; through a copy taken before the first read, so the result is 2 * sum
func @isum (i64, ...) -> (i64) local {
^e(%n: i64):
  %ap = alloca ptr i64 24, align 8
  %cp = alloca ptr i64 24, align 8
  va_start %ap
  memcpy %cp, %ap, i64 24
  jump ^loop(i64 0, i64 0)
^loop(%i: i64, %acc: i64):
  %done = icmp eq i1 %i, %n
  br %done, ^exit, ^body
^body:
  %x = va_arg i64 %ap
  %y = va_arg i64 %cp
  %a1 = add i64 %acc, %x
  %a2 = add i64 %a1, %y
  %i2 = add i64 %i, i64 1
  jump ^loop(%i2, %a2)
^exit:
  ret %acc
}

; sum of n doubles after an i32 count and a double scale: 7 xmm left in
; registers, the rest from the stack
func @fsum (i32, f64, ...) -> (f64) local {
^e(%n: i32, %scale: f64):
  %ap = alloca ptr i64 24, align 8
  va_start %ap
  jump ^loop(i32 0, f64 0x0)
^loop(%i: i32, %acc: f64):
  %done = icmp eq i1 %i, %n
  br %done, ^exit, ^body
^body:
  %x = va_arg f64 %ap
  %a = fadd f64 %acc, %x
  %i2 = add i32 %i, i32 1
  jump ^loop(%i2, %a)
^exit:
  %r = fmul f64 %acc, %scale
  ret %r
}

; alternating (i32, f64) pairs after 5 fixed integers: the classes run
; out of registers at different times
func @mixed (i64, i64, i64, i64, i64, i32, ...) -> (i64) local {
^e(%a: i64, %b: i64, %c: i64, %d: i64, %e: i64, %n: i32):
  %ap = alloca ptr i64 24, align 8
  va_start %ap
  jump ^loop(i32 0, i64 0)
^loop(%i: i32, %acc: i64):
  %done = icmp eq i1 %i, %n
  br %done, ^exit, ^body
^body:
  %x = va_arg i32 %ap
  %xs = sext i64 %x
  %f = va_arg f64 %ap
  %fi = fptosi i64 %f
  %t = mul i64 %acc, i64 10
  %t1 = add i64 %t, %xs
  %t2 = add i64 %t1, %fi
  %i2 = add i32 %i, i32 1
  jump ^loop(%i2, %t2)
^exit:
  ret %acc
}

; reads a pointer and dereferences it
func @deref (i32, ...) -> (i64) local {
^e(%n: i32):
  %ap = alloca ptr i64 24, align 8
  va_start %ap
  %p = va_arg ptr %ap
  %v = load i64 %p, align 8
  ret %v
}

func @main (i32, ptr) -> (i32) global {
^entry(%argc: i32, %argv: ptr):
  ; 1+2+...+10 = 55, doubled
  %s = call (i64, ...) -> (i64) @isum(i64 10, i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, i64 7, i64 8, i64 9, i64 10)
  call (i64) -> () @putnum(%s)
  %z = call (i64, ...) -> (i64) @isum(i64 0)
  call (i64) -> () @putnum(%z)
  ; 1.5 * 10 = 15, times 2.0 = 30 (doubles 1.5 = 0x3ff8000000000000)
  %f = call (i32, f64, ...) -> (f64) @fsum(i32 10, f64 0x4000000000000000, f64 0x3ff8000000000000, f64 0x3ff8000000000000, f64 0x3ff8000000000000, f64 0x3ff8000000000000, f64 0x3ff8000000000000, f64 0x3ff8000000000000, f64 0x3ff8000000000000, f64 0x3ff8000000000000, f64 0x3ff8000000000000, f64 0x3ff8000000000000)
  %fi = fptosi i64 %f
  call (i64) -> () @putnum(%fi)
  ; pairs (1, 2.0) (-3, 4.0) ... 9 of them: digits fold as acc*10 + x + f
  %m = call (i64, i64, i64, i64, i64, i32, ...) -> (i64) @mixed(i64 0, i64 0, i64 0, i64 0, i64 0, i32 9, i32 1, f64 0x4000000000000000, i32 -3, f64 0x4010000000000000, i32 1, f64 0x4000000000000000, i32 -3, f64 0x4010000000000000, i32 1, f64 0x4000000000000000, i32 -3, f64 0x4010000000000000, i32 1, f64 0x4000000000000000, i32 -3, f64 0x4010000000000000, i32 1, f64 0x4000000000000000)
  call (i64) -> () @putnum(%m)
  %w = call (i32, ...) -> (i64) @deref(i32 1, ptr @word)
  call (i64) -> () @putnum(%w)
  ret i32 0
}
