typedef long Counter;
enum State { Idle, Active };
union Payload { long count; double fraction; };

struct Record {
    Counter value;
    enum State state;
    union Payload payload;
    int offsets[2];
    unsigned flag : 3;
    struct Record *next;
};

extern int extra(int);

static int inspect(Counter seed, int (*adjust)(int)) {
    struct Record rec = {seed + 2, Active, {.count = 13}, {4, 7}, 5, 0};
    Counter total = rec.value + rec.offsets[1];
    {
        Counter total = 99;
        rec.offsets[0] = (int)total; /* inner-break */
    }
    return adjust((int)total) + (int)rec.payload.count; /* outer-break */
}

int main(void) {
    return inspect(40, extra) != 63;
}
