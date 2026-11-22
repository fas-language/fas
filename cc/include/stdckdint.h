#ifndef __FCC_STDCKDINT_H
#define __FCC_STDCKDINT_H
#define __STDC_VERSION_STDCKDINT_H__ 202311L
#define ckd_add(result, a, b) __builtin_ckd_add((a), (b), (result))
#define ckd_sub(result, a, b) __builtin_ckd_sub((a), (b), (result))
#define ckd_mul(result, a, b) __builtin_ckd_mul((a), (b), (result))
#endif
