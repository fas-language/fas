struct Context {
    char tag;
    long data[] __attribute__((aligned(32)));
};
extern int values[];
static_assert(__alignof__(((struct Context *)0)->data) == 32);
static_assert(__alignof__(values) == __alignof__(int));
int main(void) { return 0; }
