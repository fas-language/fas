#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
typedef struct { _Alignas(8) int32_t v; _Alignas(8) uint32_t e; } fi32;
typedef struct { _Alignas(8) uint8_t v; _Alignas(8) uint32_t e; } fu8;
typedef struct { _Alignas(8) int16_t v; _Alignas(8) uint32_t e; } fi16;
typedef struct { _Alignas(8) int64_t v; _Alignas(8) uint32_t e; } fi64;
typedef struct { _Alignas(8) const char *v; _Alignas(8) uint32_t e; } fptr;
typedef struct { _Alignas(8) double v; _Alignas(8) uint32_t e; } ff64;
typedef struct { _Alignas(8) float v; _Alignas(8) uint32_t e; } ff32;
typedef struct { _Alignas(8) bool v; _Alignas(8) uint32_t e; } fbool;

/* C side of Fas -> C: implementations Fas calls through call_addr */
fi32 c_i32(int32_t x) { return x < 0 ? (fi32){0, 0x80001234u} : (fi32){x * 2, 0}; }
fu8 c_u8(int32_t x) { return x < 0 ? (fu8){0, 5} : (fu8){(uint8_t)(x + 200), 0}; }
fi16 c_i16(int32_t x) { return x < 0 ? (fi16){0, 6} : (fi16){(int16_t)-x, 0}; }
fi64 c_i64(int32_t x) { return x < 0 ? (fi64){0, 7} : (fi64){(int64_t)x * -3000000000LL, 0}; }
fptr c_ptr(int32_t x) { return x < 0 ? (fptr){0, 8} : (fptr){"cstr", 0}; }
ff64 c_f64(int32_t x) { return x < 0 ? (ff64){0, 9} : (ff64){x + 0.25, 0}; }
ff32 c_f32(int32_t x) { return x < 0 ? (ff32){0, 10} : (ff32){x + 0.5f, 0}; }
fbool c_bool(int32_t x) { return x < 0 ? (fbool){0, 11} : (fbool){x > 5, 0}; }
uint32_t c_void(int32_t x) { return x < 0 ? 12 : 0; }

void *c_fns[] = { (void *)c_i32, (void *)c_u8, (void *)c_i16, (void *)c_i64, (void *)c_ptr,
                  (void *)c_f64, (void *)c_f32, (void *)c_bool, (void *)c_void };

/* C -> Fas: C calls Fas functions by address with the struct types */
int c_drive(void **f) {
    int bad = 0;
    fi32 a = ((fi32 (*)(int32_t))f[0])(21); bad |= a.e != 0 || a.v != 42;
    a = ((fi32 (*)(int32_t))f[0])(-1); bad |= (a.e != 2) << 1;
    fu8 b = ((fu8 (*)(int32_t))f[1])(3); bad |= (b.e != 0 || b.v != 203) << 2;
    fi64 c = ((fi64 (*)(int32_t))f[2])(2); bad |= (c.e != 0 || c.v != -6000000000LL) << 3;
    c = ((fi64 (*)(int32_t))f[2])(-2); bad |= (c.e != 2) << 4;
    ff64 d = ((ff64 (*)(int32_t))f[3])(1); bad |= (d.e != 0 || d.v != 1.25) << 5;
    d = ((ff64 (*)(int32_t))f[3])(-1); bad |= (d.e != 2) << 6;
    fptr p = ((fptr (*)(int32_t))f[4])(1); bad |= (p.e != 0 || p.v[0] != 'f') << 7;
    fbool q = ((fbool (*)(int32_t))f[5])(9); bad |= (q.e != 0 || !q.v) << 8;
    uint32_t v = ((uint32_t (*)(int32_t))f[6])(-1); bad |= (v != 2) << 9;
    v = ((uint32_t (*)(int32_t))f[6])(1); bad |= (v != 0) << 10;
    ff32 r = ((ff32 (*)(int32_t))f[7])(2); bad |= (r.e != 0 || r.v != 2.5f) << 11;
    return bad;
}
