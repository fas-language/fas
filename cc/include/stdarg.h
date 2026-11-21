/* glibc's partial inclusion requests the private alias independently. */
#if !defined(__need___va_list) && !defined(__need_va_list) && \
    !defined(__need_va_arg) && !defined(__need___va_copy) && !defined(__need_va_copy)
#define __need___va_list
#define __need_va_list
#define __need_va_arg
#define __need_va_copy
#ifndef __STRICT_ANSI__
#define __need___va_copy
#endif
#endif

#ifdef __need___va_list
#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST
typedef __builtin_va_list __gnuc_va_list;
#endif
#undef __need___va_list
#endif
#ifdef __need_va_list
#ifndef _VA_LIST
#define _VA_LIST
typedef __builtin_va_list va_list;
#endif
#undef __need_va_list
#endif
#ifdef __need_va_arg
#define va_start(...) __builtin_va_start(__VA_ARGS__)
#define va_arg(ap, type) __builtin_va_arg(ap, type)
#define va_end(ap) __builtin_va_end(ap)
#undef __need_va_arg
#endif
#ifdef __need_va_copy
#define va_copy(dest, src) __builtin_va_copy(dest, src)
#undef __need_va_copy
#endif
#ifdef __need___va_copy
#define __va_copy(dest, src) __builtin_va_copy(dest, src)
#undef __need___va_copy
#endif
