typedef struct Node { struct Node *next; int field; } Outer;
void compare(Outer *a) {
    struct Node { struct Node *next; long field; } inner;
    *a = inner;
}
