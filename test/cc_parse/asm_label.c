// An empty first token must not swallow the assembler alias from a header.
extern int f(void) __asm__("" "actual_" "name");

int actual_name(void) { return 17; }
int main(void) { return f() != 17; }
