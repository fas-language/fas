/* Checks test/fas_fir/c/symbols.fir: aliases, weak definitions overridden
   here, weak undefined references; exit 0 when all agree */
#include <stdio.h>
long hook(void) { return 2; }          /* replaces the Fir weak @hook */
long walias(void) { return 3; }        /* replaces the weak alias @walias */
long wvar = 9;                         /* replaces the weak @wvar */
long twice_alias(long);
long wkeep_alias(void);
long dflt(void);
extern long table[3], table_mid;
long fir_call_hook(void);
long fir_call_aliases(void);
long fir_read_data(void);
long fir_maybe(void);
void *fir_addr(long);
int main(void) {
  int bad = 0;
#define CHECK(e, want) do { long g_ = (long)(e); if (g_ != (long)(want)) { \
    printf("%s = %ld, expected %ld\n", #e, g_, (long)(want)); bad = 1; } } while (0)
  CHECK(fir_call_hook(), 2);
  CHECK(twice_alias(21), 42);
  CHECK(wkeep_alias(), 5);
  CHECK(dflt(), 5);
  /* twice_alias(21) * 1e6 + walias (C's) * 1e4 + wkeep_alias * 100 + hidden_alias(1) */
  CHECK(fir_call_aliases(), 42 * 1000000L + 3 * 10000 + 5 * 100 + 2);
  /* wvar (C's 9), wkeep 5, table_mid 22, table_end_local 33, hidden_data 42 */
  CHECK(fir_read_data(), 9 * 100000000L + 5 * 1000000L + 22 * 1000 + 33 + 42);
  CHECK(table_mid, 22);
  CHECK(&table_mid, &table[1]);
  CHECK(fir_maybe(), 0);
  CHECK(fir_addr(0), &table[1]);
  CHECK(fir_addr(1), twice_alias);
  return bad;
}
