static const unsigned short table[] = (const unsigned short [4]){[2] = 42};
static_assert(sizeof table == 4 * sizeof(unsigned short));

union word { short halves[2]; unsigned bits; };
static union word word = (union word)42u;

static int first, second;
static long distance = (&first + 1) - &first;
static int distinct_objects = &first != &second;
static int distinct_offsets = (&first + 1) != &first;
static_assert((&first + 1) - &first == 1);

struct letters { char a, b, rest[3]; } letters;
static long member_distance = letters.rest - &letters.b;
static long null_base_offset = &((struct letters *)0)->b - (char *)0;

void trailing_label(void) {
    goto done;
done: __attribute__((unused))
}
