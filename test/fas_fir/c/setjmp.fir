; setjmp/longjmp through a call marked returns_twice. Values computed before
; the setjmp stay live across it, across calls (so in callee-saved registers,
; restored by longjmp, or in frame slots, never shared), and the branch that
; runs before the longjmp makes new values that may reuse those registers.
; Checked by setjmp.c, which owns the jmp_buf and does the longjmp.
triple "x86_64-unknown-linux-gnu"

declare @_setjmp (ptr) -> (i32) global
declare @c_jump (ptr, i32) -> () global
declare @c_touch (i64) -> (i64) global

; one longjmp back; the result mixes the values from before and the value
; longjmp passed
func @fir_sj1 (i64, i64) -> (i64) global {
^e(%a: i64, %b: i64):
  %jb = alloca ptr i64 200, align 16
  %x = mul i64 %a, i64 3
  %y = add i64 %b, i64 7
  %t = call (i64) -> (i64) @c_touch(%x)
  %r = call returns_twice (ptr) -> (i32) @_setjmp(%jb)
  %first = icmp eq i1 %r, i32 0
  br %first, ^go, ^back
^go:
  ; new values across calls: they compete for the same callee-saved registers
  %p = call (i64) -> (i64) @c_touch(%y)
  %q = call (i64) -> (i64) @c_touch(%p)
  %s = add i64 %p, %q
  %u = call (i64) -> (i64) @c_touch(%s)
  %v = trunc i32 %u
  call noreturn (ptr, i32) -> () @c_jump(%jb, %v)
  unreachable
^back:
  %rw = sext i64 %r
  %m = mul i64 %rw, i64 10000
  %y100 = mul i64 %y, i64 100
  %s1 = add i64 %x, %y100
  %s2 = add i64 %s1, %m
  %s3 = add i64 %s2, %t
  ret %s3
}

; longjmp n times; the count is a volatile object (C: an automatic object
; changed between setjmp and longjmp is indeterminate unless volatile), and
; twelve values from before the setjmp, more than the callee-saved
; registers, plus two doubles (no callee-saved xmm), stay live across it
func @fir_sj_loop (i64) -> (i64) global {
^e(%n: i64):
  %jb = alloca ptr i64 200, align 16
  %cnt = alloca ptr i64 8, align 8
  store i64 0, %cnt, align 8 volatile
  %v1 = add i64 %n, i64 1
  %v2 = mul i64 %n, i64 2
  %v3 = add i64 %n, i64 3
  %v4 = mul i64 %n, i64 4
  %v5 = add i64 %n, i64 5
  %v6 = mul i64 %n, i64 6
  %v7 = add i64 %n, i64 7
  %v8 = mul i64 %n, i64 8
  %v9 = add i64 %n, i64 9
  %v10 = mul i64 %n, i64 10
  %v11 = add i64 %n, i64 11
  %v12 = mul i64 %n, i64 12
  %f1 = sitofp f64 %n
  %f2 = fmul f64 %f1, f64 0x3ff8000000000000
  %r = call returns_twice (ptr) -> (i32) @_setjmp(%jb)
  %c = load i64 %cnt, align 8 volatile
  %more = icmp slt i1 %c, %n
  br %more, ^again, ^done
^again:
  %c1 = add i64 %c, i64 1
  store %c1, %cnt, align 8 volatile
  ; clobber what we can before jumping back
  %w1 = call (i64) -> (i64) @c_touch(%c1)
  %w2 = call (i64) -> (i64) @c_touch(%w1)
  %w3 = mul i64 %w1, %w2
  %w4 = call (i64) -> (i64) @c_touch(%w3)
  %wv = trunc i32 %w4
  call noreturn (ptr, i32) -> () @c_jump(%jb, %wv)
  unreachable
^done:
  %rw = sext i64 %r
  %a1 = add i64 %v1, %v2
  %a2 = add i64 %a1, %v3
  %a3 = add i64 %a2, %v4
  %a4 = add i64 %a3, %v5
  %a5 = add i64 %a4, %v6
  %a6 = add i64 %a5, %v7
  %a7 = add i64 %a6, %v8
  %a8 = add i64 %a7, %v9
  %a9 = add i64 %a8, %v10
  %a10 = add i64 %a9, %v11
  %a11 = mul i64 %a10, %v12
  %ff = fadd f64 %f1, %f2
  %fi = fptosi i64 %ff
  %b1 = add i64 %a11, %fi
  %c100 = mul i64 %c, i64 1000000
  %b2 = add i64 %b1, %c100
  %r7 = mul i64 %rw, i64 7
  %b3 = add i64 %b2, %r7
  ret %b3
}
