        .text
        .macro label prefix, index
\prefix\index:
        .byte \index
        .endm
        .macro quoted text
        .ascii "\text"
        .endm
        counter = 5
        .rept 3
        .altmacro
        label example_, %counter
        .noaltmacro
        counter = counter + 1
        .endr
        .altmacro
        label example_, %(counter + 4)
        quoted <comma, space !> exclamation !!>
        .noaltmacro
        quoted "ordinary arguments"
