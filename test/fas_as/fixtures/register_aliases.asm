        .set base, %rdi
        .set index, %rcx
        .set dest, %xmm8
        .set alias, dest
        .set segment, %fs
        movaps (% rip), alias
        movups (base,index,4), dest
        movups segment:16(base), alias
        movq % rax, %rcx
