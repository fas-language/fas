static const unsigned short table[] = (const unsigned short [4]){[2] = 42};
static_assert(sizeof table == 4 * sizeof(unsigned short));
static const int old_designator[] = {[3] 17};
static_assert(sizeof old_designator == 4 * sizeof(int));
static const char digits[] = "0123\x34" "56";
static const char digit = digits[4];
static const int literal_byte = "literal"[3];
static_assert(digits[4] == '4');
static_assert("\u00e9"[1] == (char)0xa9);

union word { short halves[2]; unsigned bits; };
static union word word = (union word)42u;

static int first, second;
static long distance = (&first + 1) - &first;
static int distinct_objects = &first != &second;
static int distinct_offsets = (&first + 1) != &first;
static_assert((&first + 1) - &first == 1);

struct letters { char a, b, rest[3]; } letters;
static long member_distance = letters.rest - &letters.b;
static long indexed_member_distance = (void *)&letters.rest[2] - (void *)&letters.b;
static long null_base_offset = &((struct letters *)0)->b - (char *)0;

void trailing_label(void) {
    goto done;
done: __attribute__((unused))
}

void label_table(int index) {
    static const long offsets[] = {&&done - &&entry, &&entry - &&entry};
entry:
    goto *(&&entry + offsets[index]);
done:
    return;
}

struct Absolute { char prefix[4]; int member; };
static_assert((char *)&((struct Absolute *)0)->member - (char *)0 == 4);
static_assert((__UINTPTR_TYPE__)&((struct Absolute *)8)->member == 12);
static_assert((void *)0x86 - (void *)0x80 == 6);
