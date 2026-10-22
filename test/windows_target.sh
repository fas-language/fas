#!/bin/sh
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
case "$OCAML_FAS" in /*) ;; *) OCAML_FAS=$(pwd)/$OCAML_FAS ;; esac
WINE=${WINE:-wine}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

fail() {
  echo "windows_target: $*" >&2
  exit 1
}

cat >"$TMP/plain.fas" <<'FAS'
fn twice(x i64) i64 { return x * 2 }
fn main() i32 { return trunc[i32](twice(21)) }
FAS

"$OCAML_FAS" --os windows --emit-llvm "$TMP/plain.fas" >"$TMP/plain.ll" \
  || fail "--os windows --emit-llvm failed"
grep -F 'target triple = "x86_64-w64-windows-gnu"' "$TMP/plain.ll" >/dev/null \
  || fail "Windows triple missing"
grep -F 'target datalayout = "e-m:w-' "$TMP/plain.ll" >/dev/null \
  || fail "Windows data layout missing"
"$LLVM_OPT" -passes=verify -disable-output "$TMP/plain.ll" || fail "Windows IR does not verify"
"$OCAML_FAS" --os=linux --emit-llvm "$TMP/plain.fas" | grep -F 'x86_64-unknown-linux-gnu' >/dev/null \
  || fail "--os=linux did not select Linux"

cat >"$TMP/ext.fas" <<'FAS'
extern "C" {
    fn small(a i8, b u16, c bool) i32
    fn cb(a i8, b u16, c bool) i8 { return a }
}
fn main() i32 { return small(1, 2, true) + sext[i32](cb(1, 2, false)) }
FAS
"$OCAML_FAS" --os windows --emit-llvm "$TMP/ext.fas" >"$TMP/ext.ll" || fail "extension probe failed"
grep -F 'define i8 @cb(i8 %a0, i16 %a1, i1 zeroext %a2)' "$TMP/ext.ll" >/dev/null \
  || { cat "$TMP/ext.ll" >&2; fail "Windows C callee extends small integers"; }
grep -F 'declare i32 @small(i8, i16, i1 zeroext)' "$TMP/ext.ll" >/dev/null \
  || fail "Windows C call extends small integers"
"$OCAML_FAS" --emit-llvm --os linux "$TMP/ext.fas" \
  | grep -F 'declare i32 @small(i8 signext, i16 zeroext, i1 zeroext)' >/dev/null \
  || fail "Linux C call lost small integer extension"

expect_error() {
  expected=$1
  shift
  if "$OCAML_FAS" "$@" >"$TMP/error.out" 2>&1; then fail "accepted: $*"; fi
  grep -F -- "$expected" "$TMP/error.out" >/dev/null \
    || { cat "$TMP/error.out" >&2; fail "missing error: $expected"; }
}
expect_error 'unknown --os value `macos`; accepted values are `linux` and `windows`' \
  --os macos "$TMP/plain.fas"
expect_error '--os requires an argument' "$TMP/plain.fas" --os
expect_error 'fas perf measures Linux builds only' perf --os windows "$TMP/plain.fas"
expect_error 'fas perf measures Linux builds only' perf --tune --os=windows "$TMP/plain.fas"

MINGW_CC=${MINGW_CC:-$(command -v x86_64-w64-mingw32-clang || true)}
if [ -z "$MINGW_CC" ] || ! command -v "$WINE" >/dev/null 2>&1; then
  echo "windows_target: llvm-mingw or wine unavailable; Windows execution skipped"
  exit 0
fi
export WINEDEBUG=-all

cat >"$TMP/lib.h" <<'C'
#include <stddef.h>
typedef struct { int a, b, c; } S12;
typedef struct { float x, y; } V2;
S12 mk12(int a);
int sum12(S12 s);
V2 mkv(float x);
float sumv(V2 v);
long neg_long(long x);
unsigned long ulmax(void);
long long big(void);
int small(signed char a, unsigned char b, short c, unsigned short d);
signed char ret_i8(void);
unsigned short ret_u16(void);
int call_back(int (*f)(signed char, unsigned short));
unsigned wide_size(void);
C
cat >"$TMP/lib.c" <<'C'
#include "lib.h"
S12 mk12(int a) { S12 s = {a, a + 1, a + 2}; return s; }
int sum12(S12 s) { return s.a + s.b + s.c; }
V2 mkv(float x) { V2 v = {x, x * 2}; return v; }
float sumv(V2 v) { return v.x + v.y; }
long neg_long(long x) { return -x; }
unsigned long ulmax(void) { return (unsigned long)-1; }
long long big(void) { return 1LL << 40; }
int small(signed char a, unsigned char b, short c, unsigned short d) { return a + b + c + d; }
signed char ret_i8(void) { return -5; }
unsigned short ret_u16(void) { return 65000; }
int call_back(int (*f)(signed char, unsigned short)) { return f(-5, 65000) + f(127, 1); }
unsigned wide_size(void) { return sizeof(wchar_t); }
C
cat >"$TMP/oracle.c" <<'C'
#include <stdio.h>
#include "lib.h"
static int cb(signed char a, unsigned short b) { return a + b; }
int main(void) {
  S12 s = mk12(10);
  V2 v = mkv(1.5f);
  long l = neg_long(7);
  unsigned long u = ulmax();
  printf("%d %d %d %d\n", s.a, s.c, sum12(s), small(-3, 200, -300, 60000));
  printf("%g %ld %lu %lld %d %d\n", (double)sumv(v), l, u, big(), ret_i8() + 0, ret_u16() + 0);
  printf("%d %u %u %u %u\n", call_back(&cb), wide_size(), (unsigned)sizeof(wchar_t),
         (unsigned)sizeof(long), (unsigned)sizeof(void *));
  return 0;
}
C
cat >"$TMP/abi.fas" <<'FAS'
use "C" <stdio.h>
use "C" "lib.h"

extern "C" {
    fn cb(a i8, b u16) i32 { return sext[i32](a) + zext[i32](b) }
}

fn main() i32 {
    s S12 = mk12(10)
    v V2 = mkv(1.5)
    l i32 = neg_long(7)
    u u32 = ulmax()
    w wchar_t = 65535
    printf(c"%d %d %d %d\n", s.a, s.c, sum12(s), small(-3, 200, -300, 60000))
    printf(c"%g %ld %lu %lld %d %d\n", cvt[f64](sumv(v)), l, u, big(), sext[i32](ret_i8()),
        zext[i32](ret_u16()))
    printf(c"%d %u %u %u %u\n", call_back(&cb), wide_size(), trunc[u32](sizeof[wchar_t]),
        4, trunc[u32](sizeof[usize]))
    return zext[i32](w) - 65535
}
FAS

(cd "$TMP" && "$MINGW_CC" -O2 oracle.c lib.c -o oracle.exe) || fail "oracle build failed"
"$WINE" "$TMP/oracle.exe" >"$TMP/expected" || fail "oracle did not run"
grep -F '1099511627776 -5 65000' "$TMP/expected" >/dev/null || fail "unexpected oracle output"

for level in -O0 -O2; do
  (
    cd "$TMP"
    env FAS_CC= PATH="$(dirname "$MINGW_CC"):$PATH" TMPDIR="$TMP" \
      "$OCAML_FAS" --os windows "$level" abi.fas lib.c -o abi.exe
  ) || fail "Windows build failed at $level"
  "$WINE" "$TMP/abi.exe" >"$TMP/actual" || fail "Windows program failed at $level"
  cmp "$TMP/expected" "$TMP/actual" || fail "Windows C ABI mismatch at $level"
  FAS_CC="$MINGW_CC" "$OCAML_FAS" --os windows "$level" --emit-llvm=optimized "$TMP/abi.fas" \
    -I "$TMP" >"$TMP/abi.ll" || fail "optimized Windows IR failed at $level"
  "$LLVM_OPT" -passes=verify -disable-output "$TMP/abi.ll" \
    || fail "optimized Windows IR does not verify at $level"
done

(
  cd "$TMP"
  FAS_CC="$MINGW_CC" "$OCAML_FAS" --os windows plain.fas
) || fail "default Windows output failed"
test -f "$TMP/a.exe" || fail "default Windows output is not a.exe"
status=0
"$WINE" "$TMP/a.exe" || status=$?
test "$status" = 42 || fail "a.exe returned $status"

cat >"$TMP/long.fas" <<'FAS'
use "C" <<C
long f(void) { return -1; }
C
fn main() i32 { a i32 = f(); return a + 1 }
FAS
FAS_CC="$MINGW_CC" "$OCAML_FAS" check --os windows "$TMP/long.fas" \
  || fail "C long is not i32 for Windows"
if "$OCAML_FAS" check --os linux "$TMP/long.fas" >"$TMP/linux.out" 2>&1; then
  fail "C long narrowed on Linux"
fi
grep -F 'cannot assign `i64` to `i32`' "$TMP/linux.out" >/dev/null || fail "Linux long error changed"

echo 'windows_target: Windows triple, C ABI under Wine at O0/O2, LLP64 long, a.exe, perf refusal: ok'
