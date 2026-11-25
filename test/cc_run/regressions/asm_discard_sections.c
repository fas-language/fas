// Top-level asm keeps non-allocated sections (the kernel's objtool
// annotations, .discard.noendbr on static-call trampolines), and sections of
// one name from top-level asm, inline asm and C become one section, as an
// assembler makes: objtool reads the first section of a name only.
#include <stdio.h>

asm(".pushsection .static_call.text, \"ax\"\n"
    ".align 4\n"
    ".globl trampoline\n"
    "trampoline:\n"
    "986:\n"
    ".pushsection .discard.noendbr\n"
    ".long 986b\n"
    ".popsection\n"
    "ret; int3; nop; nop; nop\n"
    ".type trampoline, @function\n"
    ".size trampoline, . - trampoline\n"
    ".popsection\n"
    ".pushsection .fas_data,\"aw\"\n"
    ".balign 8\n"
    ".globl asm_value\n"
    "asm_value: .quad 0x2222\n"
    ".popsection\n");

__attribute__((section(".fas_data"), used)) static char c_first = 7;
__attribute__((section(".fas_data"), used)) long c_value = 0x3333;
extern long asm_value;
void trampoline(void);

static void annotated(void) {
    asm volatile("987:\n.pushsection .discard.noendbr\n.long 987b\n.popsection\n"
                 ".pushsection .fas_data,\"aw\"\n.balign 8\n.quad 0x4444\n.popsection");
}

int main(void) {
    annotated();
    trampoline();
    printf("%lx %lx %d\n", asm_value, c_value, c_first);
    return 0;
}
