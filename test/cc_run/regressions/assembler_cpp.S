#ifndef __ASSEMBLER__
#error missed assembler preprocessing mode
#endif

#define MOVE(arg) movq $arg,%rax
#define LABEL(name) .L##name
#define VALUE 42

    .macro emit value
    .byte \value
    .endm

    .text
# Assembly comment with an apostrophe: don't expand VALUE
    MOVE(VALUE) # Don't expand VALUE here either
    jmp LABEL(next)
    emit 99
LABEL(next):
    ret
