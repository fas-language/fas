    .text
    .globl entry
    .type entry,@function
entry:
    jmp target
    .org 160
target:
    .org target + 8, 0xcc
    nop
    .size entry, .-entry

    .data
    .long 1
    .org .+4, 0x78
    .long 2
    .bss
    .org 32
