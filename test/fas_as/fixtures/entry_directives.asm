        .text
        .globl vectors
        .type vectors STT_FUNC
vectors:
        vector = 32
        .rept 3
0 :
        .byte 0x6a, vector
        .fill 0b + 8 - ., 1, 0xcc
        vector = vector + 1
        .endr
        .set .Lsize, . - vectors
        .size vectors, .Lsize
        .section .note.entry, "a", @note
        .ascii "" "\0" "entry", "end"
        .asciz "first", "second"
