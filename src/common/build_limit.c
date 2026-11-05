#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>

#ifdef _WIN32

CAMLprim value fas_build_limit_get(value unit) {
  (void)unit;
  return Val_long(-1);
}

CAMLprim value fas_build_limit_set(value bytes) {
  (void)bytes;
  return Val_unit;
}

#else

#include <errno.h>
#include <string.h>
#include <sys/resource.h>

CAMLprim value fas_build_limit_get(value unit) {
  struct rlimit limit;
  (void)unit;
  if (getrlimit(RLIMIT_AS, &limit) != 0) caml_failwith(strerror(errno));
  if (limit.rlim_cur == RLIM_INFINITY || limit.rlim_cur > (rlim_t)Max_long)
    return Val_long(-1);
  return Val_long((intnat)limit.rlim_cur);
}

CAMLprim value fas_build_limit_set(value bytes) {
  struct rlimit limit;
  intnat wanted = Long_val(bytes);
  if (getrlimit(RLIMIT_AS, &limit) != 0) caml_failwith(strerror(errno));
  if (wanted < 0)
    limit.rlim_cur = limit.rlim_max;
  else if ((rlim_t)wanted > limit.rlim_max)
    limit.rlim_cur = limit.rlim_max;
  else
    limit.rlim_cur = (rlim_t)wanted;
  if (setrlimit(RLIMIT_AS, &limit) != 0) caml_failwith(strerror(errno));
  return Val_unit;
}

#endif
