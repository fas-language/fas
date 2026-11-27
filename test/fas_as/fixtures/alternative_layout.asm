        .text
        jmp 2f
        .skip 125, 0x90
old:
        nop
1:      .skip -(((replacement_end - replacement) - (1b - old)) > 0) * ((replacement_end - replacement) - (1b - old)), 0x90
2:      ret

        .pushsection .altinstructions, "a"
        .long old - .
        .long replacement - .
        .byte 2b - old, replacement_end - replacement
        .popsection

        .pushsection .altinstr_replacement, "ax"
replacement:
        mov %rcx, %rax
replacement_end:
        .popsection
