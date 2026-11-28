typedef union {
    const int *ints;
    unsigned *uints;
} pointers __attribute__((transparent_union));

void consume(pointers p);
void consume(const int *p);

void use_pointer_member(int *p, unsigned *u) {
    consume(p);
    consume(u);
    consume(nullptr);
}

union __attribute__((transparent_union)) number { int first; unsigned second; };
void consume_number(union number n);

void use_number_member(void) {
    consume_number(1);
    consume_number(1u);
}
