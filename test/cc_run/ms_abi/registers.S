.text
.globl clobber_sysv
.type clobber_sysv,@function
clobber_sysv:
    xor %edi, %edi
    xor %esi, %esi
    .irp x,6,7,8,9,10,11,12,13,14,15
    pxor %xmm\x, %xmm\x
    .endr
    ret
.size clobber_sysv,.-clobber_sysv

.globl check_preservation
.type check_preservation,@function
check_preservation:
    sub $40, %rsp
    mov $37, %edi
    mov $71, %esi
    .irp x,6,7,8,9,10,11,12,13,14,15
    pcmpeqb %xmm\x, %xmm\x
    .endr
    call preserve@PLT
    cmp $42, %eax
    jne .Lfail
    cmp $37, %edi
    jne .Lfail
    cmp $71, %esi
    jne .Lfail
    .irp x,6,7,8,9,10,11,12,13,14,15
    pmovmskb %xmm\x, %eax
    cmp $65535, %eax
    jne .Lfail
    .endr
    mov $1, %eax
    jmp .Ldone
.Lfail:
    xor %eax, %eax
.Ldone:
    add $40, %rsp
    ret
.size check_preservation,.-check_preservation
.section .note.GNU-stack,"",@progbits
