; asm goto: a terminator whose template may jump to its label blocks
; (%lN, numbered after the operands) or fall through. Covers a three-way
; branch, Intel syntax, a label that is also the fall-through, values live
; into a label block across an asm that clobbers most registers, and a
; loop whose back edge is taken from inside the asm
triple "x86_64-unknown-linux-gnu"
declare @putnum (i64) -> () global
func @sign (i64) -> (i64) local noinline {
^e(%x: i64):
  asm_goto "testq %0, %0\0a\09js %l1\0a\09jz %l2" in("r" i64 %x) ^pos [^neg, ^zero]
^pos:
  ret i64 1
^neg:
  ret i64 2
^zero:
  ret i64 0
}
func @big (i64) -> (i64) local noinline {
^e(%x: i64):
  asm_goto intel "cmp %0, 10\0a\09jae %l1" in("r" i64 %x) ^small [^large]
^small:
  ret i64 5
^large:
  ret i64 50
}
func @same (i64) -> (i64) local noinline {
^e(%x: i64):
  asm_goto "cmpq $7, %0\0a\09je %l1" in("r" i64 %x) ^out [^out]
^out:
  %r = add i64 %x, i64 1
  ret %r
}
func @live (i64) -> (i64) local noinline {
^e(%x: i64):
  %a = mul i64 %x, i64 3
  %b = add i64 %x, i64 7
  %c = mul i64 %x, %x
  %d = add i64 %x, i64 100
  asm_goto volatile "cmpq $0, %0\0a\09jne %l1" in("r" i64 %x) clobber("rax", "rbx", "rcx", "rdx", "rsi", "rdi", "r8", "r9", "r10", "r11", "r12", "cc") ^f [^t]
^f:
  %s = add i64 %a, %b
  ret %s
^t:
  %u = add i64 %a, %b
  %v = add i64 %u, %c
  %w = add i64 %v, %d
  ret %w
}
func @count (i64) -> (i64) local noinline {
^e(%n: i64):
  %p = alloca ptr i64 8, align 8
  store i64 0, %p, align 8
  %q = alloca ptr i64 8, align 8
  store %n, %q, align 8
  jump ^loop
^loop:
  %k = load i64 %q, align 8
  %s = load i64 %p, align 8
  %s2 = add i64 %s, %k
  store %s2, %p, align 8
  %k2 = sub i64 %k, i64 1
  store %k2, %q, align 8
  asm_goto "testq %0, %0\0a\09jnz %l1" in("r" i64 %k2) ^done [^loop]
^done:
  %r = load i64 %p, align 8
  ret %r
}
func @main () -> (i32) global {
^e:
  %s1 = call (i64) -> (i64) @sign(i64 -5)
  call (i64) -> () @putnum(%s1)
  %s2 = call (i64) -> (i64) @sign(i64 0)
  call (i64) -> () @putnum(%s2)
  %s3 = call (i64) -> (i64) @sign(i64 9)
  call (i64) -> () @putnum(%s3)
  %b1 = call (i64) -> (i64) @big(i64 3)
  call (i64) -> () @putnum(%b1)
  %b2 = call (i64) -> (i64) @big(i64 30)
  call (i64) -> () @putnum(%b2)
  %m1 = call (i64) -> (i64) @same(i64 7)
  call (i64) -> () @putnum(%m1)
  %l1 = call (i64) -> (i64) @live(i64 0)
  call (i64) -> () @putnum(%l1)
  %l2 = call (i64) -> (i64) @live(i64 2)
  call (i64) -> () @putnum(%l2)
  %c1 = call (i64) -> (i64) @count(i64 10)
  call (i64) -> () @putnum(%c1)
  ret i32 0
}
