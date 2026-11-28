struct entry {
    const char *name;
    unsigned length;
};

#define ENTRY(s) { (s), (s) ? sizeof(s) - 1 : 0 }

static const struct entry entries[] = {
    ENTRY((const char *)0),
    ENTRY("f"),
    ENTRY("prefix:"),
};
static const char *label = "" ? "selected" : "unselected";

int main(void)
{
    return entries[0].length != 0 || entries[1].length != 1 ||
           entries[2].length != 7 || label[0] != 's';
}
