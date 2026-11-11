; GNU inline asm: operands in registers, memory and immediates, tied and
; read-write operands, explicit registers, flag outputs, named operands,
; operand modifiers, Intel syntax, dialect alternatives, %= and clobbers.
; asm.c calls each function and checks the result.
triple "x86_64-unknown-linux-gnu"

global @fir_asm_cell size 8 align 8 global = {bytes "\05\00\00\00\00\00\00\00"}

; a + b with the result tied to an input ("0")
func @asm_add (i64, i64) -> (i64) global {
^e(%a: i64, %b: i64):
  %r = asm "addq %2, %0" out("=r" i64) in("0" i64 %a, "r" i64 %b)
  ret %r
}

; a * b with a read-write operand ("+r")
func @asm_mul (i64, i64) -> (i64) global {
^e(%a: i64, %b: i64):
  %r = asm "imulq %1, %0" out("+r" i64 %a) in("r" i64 %b)
  ret %r
}

; the high half of the unsigned 128-bit product: two outputs in rax, rdx
func @asm_mulhi (i64, i64) -> (i64) global {
^e(%a: i64, %b: i64):
  %lo, %hi = asm "mulq %3" out("=a" i64, "=d" i64) in("0" i64 %a, "r" i64 %b)
  ret %hi
}

; *p = v through a memory output, then *p += 5 through "+m" and an immediate
func @asm_store (ptr, i64) -> () global {
^e(%p: ptr, %v: i64):
  asm "movq %1, %0" out("=m" i64 %p) in("r" i64 %v)
  asm "addq %1, %0" out("+m" i64 %p) in("ri" i64 5)
  ret
}

; *p from a memory input
func @asm_load (ptr) -> (i64) global {
^e(%p: ptr):
  %r = asm "movq %1, %0" out("=r" i64) in("m" i64 %p)
  ret %r
}

; x == 0 as a flag output
func @asm_iszero (i64) -> (i32) global {
^e(%x: i64):
  %z = asm "testq %1, %1" out("=@ccz" i32) in("r" i64 %x)
  ret %z
}

; a < b (unsigned) as a flag output, with commas in a constraint
func @asm_below (i64, i64) -> (i8) global {
^e(%a: i64, %b: i64):
  %c = asm "cmpq %2, %1" out("=@ccb" i8) in("r,r" i64 %a, "r,m" i64 %b)
  ret %c
}

; named operands and width modifiers: low byte of x zero-extended
func @asm_lowbyte (i64) -> (i32) global {
^e(%x: i64):
  %r = asm "movzbl %b[src], %k[dst]" out([dst] "=r" i32) in([src] "q" i64 %x)
  ret %r
}

; Intel syntax: a + 4 * b
func @asm_lea (i64, i64) -> (i64) global {
^e(%a: i64, %b: i64):
  %r = asm intel "lea %0, [%1 + %2*4]" out("=&r" i64) in("r" i64 %a, "r" i64 %b)
  ret %r
}

; a dialect alternative and an early-clobber output: a - b
func @asm_sub (i64, i64) -> (i64) global {
^e(%a: i64, %b: i64):
  %r = asm "{movq %1, %0; subq %2, %0|mov %0, %1; sub %0, %2}" out("=&r" i64) in("r" i64 %a, "r" i64 %b)
  ret %r
}

; %= labels, used twice: 1 if x is odd, else 0, and a literal $
func @asm_odd (i64) -> (i64) global {
^e(%x: i64):
  %r = asm "xorl %k0, %k0\0a\09testq $1, %1\0a\09jz .Lfas_even%=\0a\09movl $1, %k0\0a.Lfas_even%=:" out("=&r" i64) in("r" i64 %x)
  %s = asm "xorl %k0, %k0\0a\09testq $1, %1\0a\09jz .Lfas_even%=\0a\09movl $1, %k0\0a.Lfas_even%=:" out("=&r" i64) in("r" i64 %x)
  %t = and i64 %r, %s
  ret %t
}

; a + b in xmm registers
func @asm_fadd (f64, f64) -> (f64) global {
^e(%a: f64, %b: f64):
  %r = asm "addsd %2, %0" out("=x" f64) in("0" f64 %a, "x" f64 %b)
  ret %r
}

; a value in a clobbered callee-saved register and a memory clobber: the
; function still returns x, and r12 and rbx survive for the caller
func @asm_clobber (i64) -> (i64) global {
^e(%x: i64):
  asm volatile "movq $7, %%r12\0a\09movq $9, %%rbx" out() in() clobber("r12", "rbx", "memory", "cc")
  ret %x
}

; explicit registers by name: rcx and rsi
func @asm_explicit (i64, i64) -> (i64) global {
^e(%a: i64, %b: i64):
  %r = asm "leaq (%%rcx,%%rsi), %0" out("=r" i64) in("{rcx}" i64 %a, "{rsi}" i64 %b)
  ret %r
}

; "g": anything; the cell global through a memory operand
func @asm_cell (i64) -> (i64) global {
^e(%x: i64):
  %r = asm "movq %1, %0\0a\09addq %2, %0" out("=&r" i64) in("g" i64 %x, "m" i64 ptr @fir_asm_cell)
  ret %r
}

; 32-bit operands: a rotated left by 8
func @asm_rol (i32) -> (i32) global {
^e(%a: i32):
  %r = asm "roll $8, %0" out("+r" i32 %a) in()
  ret %r
}
