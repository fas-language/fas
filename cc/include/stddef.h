/* Some system headers request only a subset of the declarations. */
#if !defined(__need_ptrdiff_t) && !defined(__need_size_t) && \
    !defined(__need_rsize_t) && !defined(__need_wchar_t) && \
    !defined(__need_NULL) && !defined(__need_wint_t) && \
    !defined(__need_nullptr_t) && !defined(__need_max_align_t) && \
    !defined(__need_offsetof) && !defined(__need_unreachable)
#define __need_ptrdiff_t
#define __need_size_t
#define __need_wchar_t
#define __need_nullptr_t
#define __need_max_align_t
#define __need_offsetof
#define __need_unreachable
#ifndef __FCC_STDDEF_H
#define __need_NULL
#define __FCC_STDDEF_H
#endif
#if defined(__STDC_WANT_LIB_EXT1__) && __STDC_WANT_LIB_EXT1__ >= 1
#define __need_rsize_t
#endif
#endif

#ifdef __need_ptrdiff_t
#ifndef _PTRDIFF_T
#define _PTRDIFF_T
typedef __PTRDIFF_TYPE__ ptrdiff_t;
#endif
#undef __need_ptrdiff_t
#endif
#ifdef __need_size_t
#ifndef _SIZE_T
#define _SIZE_T
typedef __SIZE_TYPE__ size_t;
#endif
#undef __need_size_t
#endif
#ifdef __need_rsize_t
#ifndef _RSIZE_T
#define _RSIZE_T
typedef __SIZE_TYPE__ rsize_t;
#endif
#undef __need_rsize_t
#endif
#ifdef __need_wchar_t
#ifndef _WCHAR_T
#define _WCHAR_T
typedef __WCHAR_TYPE__ wchar_t;
#endif
#undef __need_wchar_t
#endif
#ifdef __need_NULL
#undef NULL
#define NULL ((void *)0)
#undef __need_NULL
#endif
#ifdef __need_nullptr_t
#ifndef _NULLPTR_T
#define _NULLPTR_T
typedef typeof(nullptr) nullptr_t;
#endif
#undef __need_nullptr_t
#endif
#ifdef __need_unreachable
#define unreachable() __builtin_unreachable()
#undef __need_unreachable
#endif
#ifdef __need_max_align_t
#ifndef __FCC_MAX_ALIGN_T
#define __FCC_MAX_ALIGN_T
typedef struct {
    long long __clang_max_align_nonce1
        __attribute__((__aligned__(__alignof__(long long))));
    long double __clang_max_align_nonce2
        __attribute__((__aligned__(__alignof__(long double))));
} max_align_t;
#endif
#undef __need_max_align_t
#endif
#ifdef __need_offsetof
#define offsetof(t, d) __builtin_offsetof(t, d)
#undef __need_offsetof
#endif
#ifdef __need_wint_t
#ifndef _WINT_T
#define _WINT_T
typedef __WINT_TYPE__ wint_t;
#endif
#undef __need_wint_t
#endif
