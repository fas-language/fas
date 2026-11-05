.text
.globl answer
.type answer,@function
answer:
    mov $42, %eax
    ret
.size answer, .-answer

.section .rodata,"a",@progbits
.globl message, message_length, negative
message:
    .ascii "fas-ld: objects, pointers, data, BSS: ok\n"
.set message_length, .-message
.set negative, -7

.data
.balign 65536
.globl function_pointer, message_pointer, relative_answer, scratch_pointer, state
function_pointer:
    .quad answer
message_pointer:
    .quad message
relative_answer:
    .long answer-.
scratch_pointer:
    .quad scratch+8
state:
    .quad 12345

.bss
.balign 8192
.globl scratch
.type scratch,@object
scratch:
    .skip 1048576
.size scratch, .-scratch
.section .note.GNU-stack,"",@progbits
