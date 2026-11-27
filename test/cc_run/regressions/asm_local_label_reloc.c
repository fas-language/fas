#include <stdio.h>

asm(".pushsection .rodata\n"
    ".long 0\n"
    "asm_local_template: .long 77\n"
    ".globl asm_global_template\n"
    "asm_global_template: .long 88\n"
    ".popsection\n");

extern const int asm_local_template;
extern const int asm_global_template;

__attribute__((used, section(".discard.func_stack_frame_non_standard")))
static const int *const template_annotation[] = {&asm_local_template, &asm_global_template};

int main(void)
{
    printf("%d %d\n", *template_annotation[0], *template_annotation[1]);
}
