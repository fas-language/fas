typedef struct Node { int field; struct Node *next; } Outer;
void compare(Outer *a) {
    struct Node { int field; struct Node *next; } inner;
    Outer copy = inner;
    typeof(*(true ? a : &inner)) joined = {.field = 4, .next = a};
    copy = joined;
}
typedef int Matrix[2][3];
extern const Matrix matrix;
extern const int matrix[2][3];
extern typeof_unqual(matrix) plain;
extern int plain[2][3];
extern const _Atomic int atom[2];
extern typeof_unqual(atom) atom_plain;
extern _Atomic int atom_plain[2];
enum values { FIRST = 0, SECOND = 1 };
enum values { SECOND = 1, FIRST = 0 };
enum __attribute__((packed)) compact { NEGATIVE = -1 };
extern signed char get_compact(void);
extern enum compact get_compact(void);
int (*array_link)[5];

void unseen_bound(int array_link, int (*different_bound)[10]) {
    {
        extern int (*array_link)[];
        array_link = different_bound;
    }
}
