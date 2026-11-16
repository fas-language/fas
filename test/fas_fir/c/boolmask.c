/* Checks test/fas_fir/c/boolmask.fir: masked bool-vector accesses touch
   only the bytes with a selected lane (byte 1 is on a PROT_NONE page; a
   stray access faults), and keep the other bits. Exit 0 when all agree. */
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
unsigned short bm_load(unsigned char *, unsigned short);
void bm_store(unsigned char *, unsigned short, unsigned short);
static int bad;
#define CHECK(e, want) do { long g_ = (long)(e); if (g_ != (long)(want)) { \
    printf("%s = %ld, expected %ld\n", #e, g_, (long)(want)); bad = 1; } } while (0)
int main(void) {
  long pg = sysconf(_SC_PAGESIZE);
  unsigned char *m = mmap(0, 2 * pg, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (m == MAP_FAILED || mprotect(m + pg, pg, PROT_NONE)) return 2;
  unsigned char *p = m + pg - 1; /* byte 0 readable, byte 1 not */
  *p = 0xa5;
  CHECK(bm_load(p, 0x0ff), 0xa5);
  CHECK(bm_load(p, 0x00a), 0x000);
  CHECK(bm_load(p, 0x005), 0x005);
  CHECK(bm_load(p, 0x000), 0);
  bm_store(p, 0x0f0, 0x03c); /* bits 2,3 -> 0; 4,5 -> 1 */
  CHECK(*p, 0xb1);
  bm_store(p, 0xfff, 0x000); /* nothing selected: no access at all */
  CHECK(*p, 0xb1);
  /* both bytes, readable */
  unsigned char q[3] = {0xa5, 0x3c, 0x77};
  CHECK(bm_load(q, 0x607), 0x405);
  bm_store(q, 0x210, 0x21e);
  CHECK(q[0], 0xb1);
  CHECK(q[1], 0x3e);
  CHECK(q[2], 0x77);
  return bad;
}
