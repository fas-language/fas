        .text
        call 1f
        jmp .Lother
        jne .Lother
        .quad ordinary, .Lother
        .long .Lgot@GOTPCREL
        .section .text.other, "ax"
        .byte 0
ordinary:
1:      nop
.Lother:
        nop
.Lgot:
        nop
        .data
        .quad .
        .long .Lother - .
