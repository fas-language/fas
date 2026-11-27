        .text
        .code32
start32:
        movl %cr3, %eax
        movl %eax, %cr4
        movl %eax, %ds
        movw %ds, (16 + 4)(%eax)
        movl external, %eax
        movw %ax, %fs:external
        movl external, %ecx
        movl external(%eax), %ecx
        movl external(,%eax,4), %ecx
        incw %ax
        decl %ecx
        pushfl
        pushl %eax
        push $external
        pop %ebx
        popfl
        lgdtl (%eax)
        sidtl (%ebx)
        lldt %ax
        ltr %ax
        call *%edi
        jmp *external
        lret $8
        .code64
start64:
        movl external(%eax), %ecx
        movq external, %rax
        lgdtq (%rax)
        lretq
