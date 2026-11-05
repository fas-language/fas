.section .text.start,"ax",@progbits
.globl _start
.type _start,@function
_start:
    call answer
    cmp $42, %eax
    jne failed
    call *function_pointer(%rip)
    cmp $42, %eax
    jne failed
    movq $negative, %rax
    cmp $-7, %rax
    jne failed
    mov $message, %esi
    cmp message_pointer(%rip), %rsi
    jne failed
    mov relative_answer(%rip), %eax
    lea relative_answer(%rip), %rcx
    cltq
    add %rcx, %rax
    call *%rax
    cmp $42, %eax
    jne failed
    cmpl $12345, state(%rip)
    jne failed
    movq $0, state(%rip)
    cmpq $0, scratch(%rip)
    jne failed
    cmpb $0, scratch+1048575(%rip)
    jne failed
    movb $7, scratch+1048575(%rip)
    mov scratch_pointer(%rip), %rax
    lea scratch+8(%rip), %rcx
    cmp %rax, %rcx
    jne failed
    mov $1, %eax
    mov $1, %edi
    mov message_pointer(%rip), %rsi
    mov $message_length, %edx
    syscall
    cmp $message_length, %eax
    jne failed
    xor %edi, %edi
    jmp exit
failed:
    mov $99, %edi
exit:
    mov $60, %eax
    syscall
.size _start, .-_start
.section .note.GNU-stack,"",@progbits
