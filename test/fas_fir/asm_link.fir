; GNU inline asm in a whole program, linked by our own linker: a call from
; asm to a Fas function, a RIP-relative global, local labels, an Intel
; frame-slot operand, and asm inputs pinned to r13-r15 with every other
; register clobbered (the values live across it spill, and the allocator's
; scratch registers are taken by the inputs). @twice and @counter are
; global: LLVM drops local definitions only asm text refers to (C needs `used`)
triple "x86_64-unknown-linux-gnu"
declare @putnum (i64) -> () global
global @counter size 8 align 8 global = zero
func @twice (i64) -> (i64) global noinline {
^e(%x: i64):
  %r = add i64 %x, %x
  ret %r
}
func @via_call (i64) -> (i64) local noinline {
^e(%x: i64):
  %r, %d = asm volatile "call twice" out("=a" i64, "=D" i64) in("1" i64 %x) clobber("rcx", "rdx", "rsi", "r8", "r9", "r10", "r11", "memory", "cc")
  ret %r
}
func @bump () -> (i64) local noinline {
^e:
  asm volatile "addq $3, counter(%%rip)" clobber("memory", "cc")
  %r = asm volatile "movq counter(%%rip), %0" out("=r" i64)
  ret %r
}
func @tri (i64) -> (i64) local noinline {
^e(%n: i64):
  %r, %z = asm "xorl %k0, %k0\0a1:\0a\09addq %1, %0\0a\09decq %1\0a\09jnz 1b" out("=&r" i64, "+r" i64 %n)
  ret %r
}
func @pressure (i64) -> (i64) local noinline {
^e(%x: i64):
  %a = mul i64 %x, i64 3
  %b = add i64 %x, i64 7
  %c = mul i64 %x, %x
  %d = add i64 %x, i64 100
  %e = mul i64 %x, i64 11
  %f = sub i64 %x, i64 1
  %g = xor i64 %x, i64 9
  %s = asm "leaq (%1,%2), %0\0a\09addq %3, %0" out("=r" i64) in("{r13}" i64 %a, "{r14}" i64 %b, "{r15}" i64 %c) clobber("rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12")
  %t1 = add i64 %s, %a
  %t2 = add i64 %t1, %b
  %t3 = add i64 %t2, %c
  %t4 = add i64 %t3, %d
  %t5 = add i64 %t4, %e
  %t6 = add i64 %t5, %f
  %t7 = add i64 %t6, %g
  ret %t7
}
func @frame_mem (i64) -> (i64) local noinline {
^e(%x: i64):
  %p = alloca ptr i64 8, align 8
  store %x, %p, align 8
  %r = asm intel "mov %0, %1\0a\09add %0, %1" out("=&r" i64) in("m" i64 %p)
  ret %r
}
func @main () -> (i32) global {
^e:
  %v1 = call (i64) -> (i64) @via_call(i64 21)
  call (i64) -> () @putnum(%v1)
  %v2 = call () -> (i64) @bump()
  call (i64) -> () @putnum(%v2)
  %v3 = call () -> (i64) @bump()
  call (i64) -> () @putnum(%v3)
  %v4 = call (i64) -> (i64) @tri(i64 10)
  call (i64) -> () @putnum(%v4)
  %v5 = call (i64) -> (i64) @pressure(i64 5)
  call (i64) -> () @putnum(%v5)
  %v6 = call (i64) -> (i64) @frame_mem(i64 9)
  call (i64) -> () @putnum(%v6)
  ret i32 0
}
