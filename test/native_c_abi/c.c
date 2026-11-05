double fas_scale(double x, float y, int k);
/* many mixed args: 7 doubles + 8 ints force stack args on both classes */
double mix(double a, int i, float b, long j, double c, short s, double d, signed char t,
           double e, int u, double f, int v, double g, double h, int w, double z) {
  return a + i + b + j + c + s + d + t + e + u + f + v + g + h + w + z;
}
float half(float x) { return x / 2; }
int sum3(char a, short b, unsigned char c) { return a + b + c; }
int check(void) { return (int)(fas_scale(1.5, 2.0f, 3) * 100); } /* (1.5*2)+3 = 6 -> 600 */
