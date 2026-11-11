; Top-level asm (module_asm): functions and data defined in asm text, used
; from Fir and using Fir. Covers .text/.data/.rodata/.bss sections, a call
; from asm into a Fir function, a pointer to a Fir function in asm data,
; local labels reached through section-relative relocations, CFI
; directives (an .eh_frame of the asm's own), a local asm symbol, and two
; module_asm entries that are assembled together in order
triple "x86_64-unknown-linux-gnu"
module_asm "\09.text\0a\09.globl asm_add3\0a\09.type asm_add3, @function\0aasm_add3:\0a\09.cfi_startproc\0a\09leaq 3(%rdi), %rax\0a\09ret\0a\09.cfi_endproc\0a\09.size asm_add3, .-asm_add3"
module_asm "\09.globl asm_via\0a\09.type asm_via, @function\0aasm_via:\0a\09.cfi_startproc\0a\09subq $8, %rsp\0a\09.cfi_def_cfa_offset 16\0a\09call twice\0a\09addq $8, %rsp\0a\09.cfi_def_cfa_offset 8\0a\09call helper\0a\09ret\0a\09.cfi_endproc\0ahelper:\0a\09addq $1, %rax\0a\09ret\0a\09.globl asm_sum\0aasm_sum:\0a\09leaq .Ltab(%rip), %rcx\0a\09movq (%rcx), %rax\0a\09addq 8(%rcx), %rax\0a\09movq asm_ptr(%rip), %rdx\0a\09movq %rdi, %rcx\0a\09jmp *%rdx\0a\09.section .rodata\0a\09.p2align 3\0a.Ltab:\0a\09.quad 30, 12\0a\09.data\0a\09.p2align 3\0a\09.globl asm_ptr\0aasm_ptr:\0a\09.quad add_rcx\0a\09.globl asm_counter\0aasm_counter:\0a\09.quad 7\0a\09.bss\0a\09.globl asm_buf\0aasm_buf:\0a\09.zero 64"
declare @putnum (i64) -> () global
declare @asm_add3 (i64) -> (i64) global
declare @asm_via (i64) -> (i64) global
declare @asm_sum (i64) -> (i64) global
global @asm_counter size 8 align 8 global = extern
global @asm_buf size 64 align 8 global = extern
func @twice (i64) -> (i64) global noinline {
^e(%x: i64):
  %r = add i64 %x, %x
  ret %r
}
; asm_sum jumps here with the table sum in rax and its argument in rcx
func @add_rcx () -> (i64) global noinline {
^e:
  %r = asm "addq %%rcx, %%rax" out("={rax}" i64)
  ret %r
}
func @main () -> (i32) global {
^e:
  %a = call (i64) -> (i64) @asm_add3(i64 39)
  call (i64) -> () @putnum(%a)
  %b = call (i64) -> (i64) @asm_via(i64 20)
  call (i64) -> () @putnum(%b)
  %c = load i64 ptr @asm_counter, align 8
  call (i64) -> () @putnum(%c)
  %p = ptradd ptr ptr @asm_buf, i64 0, 1, 56
  store i64 99, %p, align 8
  %d = load i64 %p, align 8
  call (i64) -> () @putnum(%d)
  ret i32 0
}
