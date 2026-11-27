    .text
    .macro emit first:req second=(2 + 3) body:vararg
    .byte \first, \second
    \body
    .endm
    emit second=9 first=1 body=.byte 8, 10
    emit 2, , .byte 11, 12
    emit 3 (2+4) .byte 13
    emit 4, 2 + 5, .byte 14

    .macro insn text
    \text
    .endm
    insn "movq %rax, %rbx; nop"

    .macro choose base=%rsp value=15
    .if \base == %rax; .byte 32
    .elseif \base != %rsp; .byte 33
    .else; .byte \value
    .endif
    .endmacro
    choose value=18 base=%rsp
    choose %rbp
    choose

    .macro names word; .globl serial\@, \word; serial\@:; \word:; .endm
    names first
    .rept 1; names second; .endr
    .irp text, rax, rbx
    .ifc \text, rax; .byte 19; .endif
    .ifnc \text, rax; .byte 20; .endif
    .endr
