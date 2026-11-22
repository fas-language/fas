#define MS __attribute__((ms_abi))

struct tiny { unsigned char byte; };
struct halfword { unsigned short value; };
union word { float f; unsigned bytes; };
struct small { float x, y; };
struct large { long values[3]; };
struct aligned { alignas(32) unsigned char byte; };
typedef int vec4 __attribute__((vector_size(16)));
typedef int vec2 __attribute__((vector_size(8)));
typedef double MS mixed_fn(int, double, int, float, int, double);

mixed_fn mixed;
MS struct tiny tiny(struct tiny);
MS struct halfword halfword(struct halfword);
MS union word word(union word);
MS struct small small(struct small, struct small, struct small, struct small, struct small);
MS struct large large(float, struct large, int, double, int);
MS struct aligned aligned(struct aligned);
MS long double extended(long double, int);
MS vec4 vector(vec4);
MS vec2 short_vector(vec2);
MS __int128 wide(__int128);
MS __float128 quad(__float128);
MS struct large varargs(int, ...);
MS double stack_varargs(int, int, int, int, int, ...);
MS int preserve(void);
void clobber_sysv(void);
int check_preservation(void);
