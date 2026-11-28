#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "dict.h"
#include "listpack.h"
#include "sds.h"
#include "testhelp.h"
#include "util.h"
#include "ziplist.h"
#include "zmalloc.h"

int __failed_tests;
int __test_num;

static uint64_t hash_int(const void *key) {
    return (unsigned)*(const int *)key * UINT64_C(11400714819323198485);
}

static void test_dict(void) {
    dictType type = {.hashFunction = hash_int};
    dict *table = dictCreate(&type);
    int keys[128];
    for (int i = 0; i < 128; ++i) {
        keys[i] = i;
        assert(dictAdd(table, &keys[i], &keys[0]) == DICT_OK);
    }
    for (int i = 0; i < 128; ++i)
        assert(dictFetchValue(table, &keys[i]) == &keys[0]);
    for (int i = 0; i < 128; i += 2)
        assert(dictDelete(table, &keys[i]) == DICT_OK);
    assert(dictSize(table) == 64);
    dictRelease(table);
}

static void test_packed_lists(void) {
    unsigned char number[] = "1234567";
    unsigned char text[] = "hello";
    unsigned char *packed = lpNew(0);
    packed = lpAppend(packed, number, sizeof(number) - 1);
    packed = lpAppend(packed, text, sizeof(text) - 1);
    assert(lpLength(packed) == 2);
    int64_t count;
    assert(!lpGet(lpSeek(packed, 0), &count, NULL) && count == 1234567);
    unsigned char *entry = lpGet(lpSeek(packed, -1), &count, NULL);
    assert(count == sizeof(text) - 1 && memcmp(entry, text, count) == 0);
    lpFree(packed);

    unsigned char *zip = ziplistNew();
    zip = ziplistPush(zip, number, sizeof(number) - 1, ZIPLIST_TAIL);
    zip = ziplistPush(zip, text, sizeof(text) - 1, ZIPLIST_TAIL);
    assert(ziplistLen(zip) == 2);
    unsigned int len;
    long long value;
    assert(ziplistGet(ziplistIndex(zip, 0), &entry, &len, &value));
    assert(!entry && value == 1234567);
    assert(ziplistGet(ziplistIndex(zip, -1), &entry, &len, &value));
    assert(len == sizeof(text) - 1 && memcmp(entry, text, len) == 0);
    zfree(zip);
}

int main(int argc, char **argv) {
    if (sdsTest(argc, argv, 0) ||
        utilTest(argc, argv, REDIS_TEST_VALGRIND)) /* Avoid the fixed /tmp cache probe. */
        return 1;
    test_report();
    test_dict();
    test_packed_lists();
    puts("standalone collections passed");
}
