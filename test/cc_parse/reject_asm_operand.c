void f(int value) {
    asm ("" : [same] "=r"(value) : [same] "r"(value));
}
