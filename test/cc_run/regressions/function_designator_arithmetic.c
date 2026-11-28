static int f(int value) { return value + 1; }

int main(void) {
    // GNU C gives function pointer arithmetic a stride of one byte.
    volatile int offset = 3;
    if ((unsigned long)(f + offset) - (unsigned long)f != 3)
        return 1;
    return ((offset + f) - offset)(41) != 42 || (f - offset) + offset != f;
}
