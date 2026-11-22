/* GNU folded values can initialize static objects, including aggregate leaves. */
static const int base = 3;
static const int data[] = {4, 5, 6};
static const int combined = base + data[1];
static const int shortcircuit = (0.25 || (1 / 0)) ? combined : -1;
struct pair { int x, y; };
static struct pair result = (struct pair){ .x = combined, .y = shortcircuit };
static_assert((int)(1.0 + 1.0) == 2);
static_assert(1.25 * 2.0 == 2.5);
