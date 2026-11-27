        andl $(~0xff000000), %ecx
        andl $0xffffffff, %ecx
        andl $0x1ffffffff, %ecx
        andw $0xffff, %cx
        andw $0x1ffff, %cx
        movl $-4294967296, %ecx
        movw $-65536, %cx
