.section .tdata,"awT",@progbits
.globl datum
.hidden datum
.type datum,@tls_object
datum: .long 42
.size datum,4
.section .debug_test,"",@progbits
.long 0
.reloc 0,R_X86_64_GOTPC32_TLSDESC,datum-4
.section .note.GNU-stack,"",@progbits
