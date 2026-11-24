extern char anchor[];

__attribute__((section(".text.named"), noinline))
static char *get_anchor(void) {
    asm volatile("anchor:\n\tnop");
    return anchor;
}
int main(void) {
    return get_anchor() != anchor;
}
