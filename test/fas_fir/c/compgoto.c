/* Checks test/fas_fir/c/compgoto.fir against the same computed-goto code in
   GNU C; exit 0 when they agree */
#include <stdio.h>
long fir_interp(const int *code);
long fir_offsets(int k);
void *fir_label(int k);
long fir_jump(void *p);
int fir_label_eq(void);
long fir_static(int k);

static long c_interp(const int *code) {
  static void *tbl[] = {&&halt, &&add, &&mul, &&decjnz, &&setcnt};
  long pc = 0, cnt = 0;
  unsigned long acc = 0;
  goto *tbl[code[pc]];
halt: return (long)acc;
add: acc += (long)code[pc + 1]; pc += 2; goto *tbl[code[pc]];
mul: acc *= (unsigned long)(long)code[pc + 1]; pc += 2; goto *tbl[code[pc]];
decjnz: cnt--; pc = cnt == 0 ? pc + 2 : code[pc + 1]; goto *tbl[code[pc]];
setcnt: cnt = code[pc + 1]; pc += 2; goto *tbl[code[pc]];
}
static long c_offsets(int k) {
  static const long offs[] = {0, &&lb - &&la, &&lc - &&la};
  goto *(&&la + offs[k]);
la: return 100;
lb: return 200;
lc: return 300;
}
int main(void) {
  static const int p1[] = {1, 5, 2, 3, 1, -4, 0};
  static const int p2[] = {4, 10, 1, 7, 2, 3, 3, 2, 0};          /* loop 10 times */
  static const int p3[] = {1, 1, 4, 70, 2, -3, 1, 9, 3, 4, 0};   /* wraps */
  static const int *ps[] = {p1, p2, p3};
  int bad = 0;
  for (int i = 0; i < 3; i++) {
    long a = fir_interp(ps[i]), b = c_interp(ps[i]);
    printf("interp %d: fir %ld c %ld\n", i, a, b);
    bad |= a != b;
  }
  for (int k = 0; k < 3; k++) {
    long a = fir_offsets(k), b = c_offsets(k);
    printf("offsets %d: fir %ld c %ld\n", k, a, b);
    bad |= a != b;
  }
  for (int k = 0; k < 3; k++) {
    long a = fir_static(k), want = 1000 * (k + 1) + (k > 0);
    printf("static %d: fir %ld want %ld\n", k, a, want);
    bad |= a != want;
  }
  long j0 = fir_jump(fir_label(0)), j1 = fir_jump(fir_label(1));
  int eq = fir_label_eq();
  printf("jump %ld %ld eq %d\n", j0, j1, eq);
  bad |= j0 != 11 || j1 != 22 || eq != 1;
  return bad;
}
