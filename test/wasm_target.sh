#!/bin/sh
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
case "$OCAML_FAS" in /*) ;; *) OCAML_FAS=$(pwd)/$OCAML_FAS ;; esac
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

fail() {
  echo "wasm_target: $*" >&2
  exit 1
}

cat >"$TMP/plain.fas" <<'FAS'
fn twice(x i64) i64 { return x * 2 }
fn main() i32 { return trunc[i32](twice(21)) }
FAS

"$OCAML_FAS" --os wasi --emit-llvm "$TMP/plain.fas" >"$TMP/plain.ll" \
  || fail "--os wasi --emit-llvm failed"
grep -F 'target triple = "wasm32-unknown-wasip1"' "$TMP/plain.ll" >/dev/null \
  || fail "WASI triple missing"
grep -F 'target datalayout = "e-m:e-p:32:32-' "$TMP/plain.ll" >/dev/null \
  || fail "wasm32 data layout missing"
grep -F '"target-cpu"="generic"' "$TMP/plain.ll" >/dev/null || fail "WASI target-cpu is not generic"
if grep -F -e 'x86-64' -e 'tune-cpu' "$TMP/plain.ll" >/dev/null; then
  fail "x86 attributes in WASI IR"
fi
"$LLVM_OPT" -passes=verify -disable-output "$TMP/plain.ll" || fail "WASI IR does not verify"

cat >"$TMP/size.fas" <<'FAS'
fn main() i32 { return trunc[i32](sizeof[usize]) }
FAS
if "$OCAML_FAS" check --os wasi "$TMP/size.fas" >"$TMP/size.out" 2>&1; then
  fail "usize is wider than 32 bits for WASI"
fi
grep -F 'illegal `trunc` from `usize` to `i32`' "$TMP/size.out" >/dev/null \
  || { cat "$TMP/size.out" >&2; fail "usize width error changed"; }
"$OCAML_FAS" check --os linux "$TMP/size.fas" || fail "usize is not 64 bits on Linux"

cat >"$TMP/ext.fas" <<'FAS'
extern "C" {
    fn small(a i8, b u16, c bool) i32
    fn cb(a i8, b u16, c bool) i8 { return a }
}
fn main(argc i32, argv addr) i32 { return small(1, 2, true) + sext[i32](cb(1, 2, false)) }
FAS
"$OCAML_FAS" --os wasi --emit-llvm "$TMP/ext.fas" >"$TMP/ext.ll" || fail "extension probe failed"
grep -F 'declare i32 @small(i8 signext, i16 zeroext, i1 zeroext)' "$TMP/ext.ll" >/dev/null \
  || { cat "$TMP/ext.ll" >&2; fail "WASI C call lost small integer extension"; }
grep -F 'define i32 @__main_argc_argv(i32 %a0, ptr %a1)' "$TMP/ext.ll" >/dev/null \
  || fail "WASI main with arguments is not __main_argc_argv"
"$OCAML_FAS" --os linux --emit-llvm "$TMP/ext.fas" | grep -F 'define i32 @main(' >/dev/null \
  || fail "Linux main renamed"
"$OCAML_FAS" --os wasi --emit-llvm "$TMP/plain.fas" | grep -F 'define i32 @__main_void()' >/dev/null \
  || fail "WASI main without arguments is not __main_void"

"$OCAML_FAS" --os wasi --target=force-native --emit-llvm "$TMP/plain.fas" \
  | grep -F '"target-features"="+simd128"' >/dev/null || fail "force-native lacks simd128"

expect_error() {
  expected=$1
  shift
  if "$OCAML_FAS" "$@" >"$TMP/error.out" 2>&1; then fail "accepted: $*"; fi
  grep -F -- "$expected" "$TMP/error.out" >/dev/null \
    || { cat "$TMP/error.out" >&2; fail "missing error: $expected"; }
}
expect_error 'fas perf measures Linux builds only; this build is for WASI' \
  perf --os wasi "$TMP/plain.fas"
expect_error 'fas perf measures Linux builds only; this build is for WASI' \
  perf --tune --os=wasi "$TMP/plain.fas"
expect_error 'sanitizers are not available for WASI builds' \
  --os wasi --sanitize=address "$TMP/plain.fas"
expect_error '`--profile` is not available for WASI builds' \
  --os wasi --profile "$TMP/none.fasprof" "$TMP/plain.fas"

WASI_SDK_PATH=${WASI_SDK_PATH:-/opt/wasi-sdk}
WASI_CC=$WASI_SDK_PATH/bin/clang
REAL_WASMTIME=$(command -v "${FAS_WASMTIME:-wasmtime}" || true)
if [ ! -x "$WASI_CC" ] || [ -z "$REAL_WASMTIME" ]; then
  echo "wasm_target: wasi-sdk or wasmtime unavailable; WASI execution skipped"
  exit 0
fi
WASMTIME=$TMP/wasmtime
cat >"$WASMTIME" <<SH
#!/bin/sh
command=\$1
shift
exec "$REAL_WASMTIME" "\$command" -O memory-reservation=67108864 -O memory-guard-size=65536 "\$@"
SH
chmod +x "$WASMTIME"
export WASI_SDK_PATH
unset FAS_CC

for source in address_constants address_reconstruction bool_memory c_records float_vectors \
  if_operand_offsets kernels lowering_probes simd_memory volatile_vectors; do
  (cd "$ROOT/test" && "$OCAML_FAS" --os wasi -O2 -c "$source.fas" -o "$TMP/$source.o") \
    || fail "$source.fas does not build for WASI"
done

cat >"$TMP/lib.h" <<'C'
#include <stddef.h>
typedef struct { int a, b, c; } S12;
typedef struct { float x, y; } V2;
typedef struct { long long q; } Q;
S12 mk12(int a);
int sum12(S12 s);
V2 mkv(float x);
float sumv(V2 v);
Q mkq(long long v);
long long getq(Q q);
long neg_long(long x);
unsigned long ulmax(void);
long long big(void);
int small(signed char a, unsigned char b, short c, unsigned short d);
signed char ret_i8(void);
unsigned short ret_u16(void);
int call_back(int (*f)(signed char, unsigned short));
size_t fold(const unsigned *p, size_t n);
C
cat >"$TMP/lib.c" <<'C'
#include "lib.h"
S12 mk12(int a) { S12 s = {a, a + 1, a + 2}; return s; }
int sum12(S12 s) { return s.a + s.b + s.c; }
V2 mkv(float x) { V2 v = {x, x * 2}; return v; }
float sumv(V2 v) { return v.x + v.y; }
Q mkq(long long v) { Q q = {v}; return q; }
long long getq(Q q) { return q.q; }
long neg_long(long x) { return -x; }
unsigned long ulmax(void) { return (unsigned long)-1; }
long long big(void) { return 1LL << 40; }
int small(signed char a, unsigned char b, short c, unsigned short d) { return a + b + c + d; }
signed char ret_i8(void) { return -5; }
unsigned short ret_u16(void) { return 65000; }
int call_back(int (*f)(signed char, unsigned short)) { return f(-5, 65000) + f(127, 1); }
size_t fold(const unsigned *p, size_t n) {
  size_t h = 0;
  for (size_t i = 0; i < n; i++) h = h * 31 + p[i];
  return h;
}
C
cat >"$TMP/oracle.c" <<'C'
#include <stdio.h>
#include <stdlib.h>
#include "lib.h"
static int cb(signed char a, unsigned short b) { return a + b; }
int main(int argc, char **argv) {
  S12 s = mk12(10);
  V2 v = mkv(1.5f);
  long l = neg_long(7);
  unsigned long u = ulmax();
  unsigned *p = malloc(100 * sizeof(unsigned));
  for (unsigned i = 0; i < 100; i++) p[i] = i * 2654435761u;
  printf("%d %d %d %d\n", s.a, s.c, sum12(s), small(-3, 200, -300, 60000));
  printf("%g %ld %lu %lld %d %d %lld\n", (double)sumv(v), l, u, big(), ret_i8() + 0,
         ret_u16() + 0, getq(mkq(-1234567890123LL)));
  printf("%d %zu %zu %zu %d %s\n", call_back(&cb), fold(p, 100), sizeof(long),
         sizeof(void *), argc, argv[1]);
  free(p);
  return 5;
}
C
cat >"$TMP/abi.fas" <<'FAS'
use "C" <stdio.h>
use "C" <stdlib.h>
use "C" "lib.h"

extern "C" {
    fn cb(a i8, b u16) i32 { return sext[i32](a) + zext[i32](b) }
}

fn main(argc i32, argv addr) i32 {
    s S12 = mk12(10)
    v V2 = mkv(1.5)
    l i32 = neg_long(7)
    u u32 = ulmax()
    q Q = mkq(-1234567890123)
    p addr = malloc(100 * sizeof[u32])
    for i u32 = 0; i < 100; i += 1 { p[u32, i] = i * 2654435761 }
    printf(c"%d %d %d %d\n", s.a, s.c, sum12(s), small(-3, 200, -300, 60000))
    printf(c"%g %ld %lu %lld %d %d %lld\n", cvt[f64](sumv(v)), l, u, big(), sext[i32](ret_i8()),
        zext[i32](ret_u16()), getq(q))
    printf(c"%d %zu %zu %zu %d %s\n", call_back(&cb), fold(p, 100), 4, sizeof[usize], argc,
        argv[addr, 1])
    free(p)
    return 5
}
FAS

run() {
  status=0
  (cd "$TMP" && "$WASMTIME" run "$@") || status=$?
}

(cd "$TMP" && "$WASI_CC" -O2 oracle.c lib.c -o oracle.wasm) || fail "oracle build failed"
run oracle.wasm arg >"$TMP/expected"
test "$status" = 5 || fail "oracle returned $status"
grep -F -- '-7 4294967295 1099511627776 -5 65000 -1234567890123' "$TMP/expected" >/dev/null \
  || { cat "$TMP/expected" >&2; fail "unexpected oracle output"; }
grep -F ' 4 4 2 arg' "$TMP/expected" >/dev/null || fail "oracle is not ILP32"

for level in -O0 -O2; do
  (cd "$TMP" && TMPDIR="$TMP" "$OCAML_FAS" --os wasi "$level" abi.fas lib.c -o abi.wasm) \
    || fail "WASI build failed at $level"
  run abi.wasm arg >"$TMP/actual"
  test "$status" = 5 || fail "WASI program returned $status at $level"
  cmp "$TMP/expected" "$TMP/actual" || fail "WASI C ABI mismatch at $level"
  "$OCAML_FAS" --os wasi "$level" --emit-llvm=optimized "$TMP/abi.fas" -I "$TMP" >"$TMP/abi.ll" \
    || fail "optimized WASI IR failed at $level"
  "$LLVM_OPT" -passes=verify -disable-output "$TMP/abi.ll" \
    || fail "optimized WASI IR does not verify at $level"
  (cd "$TMP" && "$OCAML_FAS" --os wasi "$level" -c abi.fas -o abi.o && "$WASI_CC" abi.o lib.c -o linked.wasm) \
    || fail "WASI object link failed at $level"
  run linked.wasm arg >"$TMP/actual"
  cmp "$TMP/expected" "$TMP/actual" || fail "WASI object mismatch at $level"
done

cat >"$TMP/simd.fas" <<'FAS'
use "C" <stdio.h>
fn main(argc i32, argv addr) i32 {
    k u32 = bitcast[u32](argc)
    a vec[4, f32] = {1.5, 2.0, -3.0, 4.25}
    b vec[4, u32] = {7, 4000000000, 3, 9}
    a *= splat(cvt[f32](k))
    b *= splat(k)
    c vec[4, f32] = a * a + splat(1.0)
    d vec[4, u32] = b * splat(3) + splat(1)
    printf(c"%g %g %g %g %u %u %u %u\n", cvt[f64](c[0]), cvt[f64](c[1]), cvt[f64](c[2]),
        cvt[f64](c[3]), d[0], d[1], d[2], d[3])
    return 0
}
FAS
printf '3.25 5 10 19.0625 22 3410065409 10 28\n' >"$TMP/simd.expected"
for target in portable force-native; do
  for level in -O0 -O2; do
    (cd "$TMP" && "$OCAML_FAS" --os wasi --target="$target" "$level" simd.fas -o simd.wasm) \
      || fail "vector build failed for $target at $level"
    run simd.wasm >"$TMP/simd.out"
    cmp "$TMP/simd.expected" "$TMP/simd.out" || fail "vector result wrong for $target at $level"
  done
done
(cd "$TMP" && "$OCAML_FAS" --os wasi -O2 -S simd.fas -o portable.s \
  && "$OCAML_FAS" --os wasi --target=force-native -O2 -S simd.fas -o native.s) \
  || fail "vector assembly failed"
if grep -F 'v128' "$TMP/portable.s" >/dev/null; then fail "portable WASI build uses simd128"; fi
grep -F 'v128' "$TMP/native.s" >/dev/null || fail "force-native WASI build lacks simd128"

cat >"$TMP/compress.fas" <<'FAS'
use "C" <stdio.h>

fn bytes(x i32) void {
    v vec[256, u8] = splat(0)
    m vec[256, bool] = splat(false)
    for i i32 = 0; i < 256; i += 1 {
        v[i] = trunc[u8](i * 37 + x)
        m[i] = (i + x) % 3 != 0
    }
    r vec[256, u8] = compress(v, m)
    for i i32 = 0; i < 256; i += 1 { printf(c"%d ", zext[i32](r[i])) }
    printf(c"\n")
}

fn odd(x i32) void {
    v vec[3, i32] = {x * 5, x * 7, x * 11}
    m vec[3, bool] = {x > 1, true, x < 2}
    r vec[3, i32] = compress(v, m)
    printf(c"%d %d %d\n", r[0], r[1], r[2])
}

fn wide(x i32) void {
    v vec[32, i64] = splat(0)
    m vec[32, bool] = splat(false)
    for i i32 = 0; i < 32; i += 1 {
        v[i] = sext[i64](i - x) * 1099511627776
        m[i] = (i * x) % 5 < 2
    }
    r vec[32, i64] = compress(v, m)
    for i i32 = 0; i < 32; i += 1 { printf(c"%lld ", r[i]) }
    printf(c"\n")
}

fn floats(x i32) void {
    v vec[64, f32] = splat(0.0)
    d vec[2, f64] = {cvt[f64](x) * 0.25, -0.0}
    m vec[64, bool] = splat(false)
    for i i32 = 0; i < 64; i += 1 {
        v[i] = cvt[f32](i - x) * 0.5
        m[i] = (i + x) % 4 != 1
    }
    r vec[64, f32] = compress(v, m)
    for i i32 = 0; i < 64; i += 1 { printf(c"%g ", cvt[f64](r[i])) }
    k vec[2, bool] = {x > 1, true}
    e vec[2, f64] = compress(d, k)
    printf(c"%g %g\n", e[0], e[1])
}

fn flags(x i32) void {
    v vec[256, bool] = splat(false)
    m vec[256, bool] = splat(false)
    for i i32 = 0; i < 256; i += 1 {
        v[i] = (i + x) % 2 == 0
        m[i] = (i * 3 + x) % 7 > 2
    }
    r vec[256, bool] = compress(v, m)
    for i i32 = 0; i < 256; i += 1 { if r[i] { printf(c"1") } else { printf(c"0") } }
    printf(c"\n")
}

fn one(x i32) void {
    v vec[1, i16] = {trunc[i16](x * 300)}
    m vec[1, bool] = {x > 1}
    r vec[1, i16] = compress(v, m)
    printf(c"%d\n", sext[i32](r[0]))
}

fn main(argc i32, argv addr) i32 {
    one(argc)
    bytes(argc)
    odd(argc)
    wide(argc)
    floats(argc)
    flags(argc)
    return 0
}
FAS
cat >"$TMP/compress.c" <<'C'
#include <stdio.h>
#include <stdint.h>
#define COMPRESS(r, v, m, n) { int j = 0; for (int i = 0; i < n; i++) r[i] = 0; for (int i = 0; i < n; i++) if (m[i]) r[j++] = v[i]; }
static void one(int x) { int16_t v[1] = {(int16_t)(x * 300)}, r[1]; int m[1] = {x > 1}; COMPRESS(r, v, m, 1); printf("%d\n", r[0]); }
static void bytes(int x) { uint8_t v[256], r[256]; int m[256]; for (int i = 0; i < 256; i++) { v[i] = (uint8_t)(i * 37 + x); m[i] = (i + x) % 3 != 0; } COMPRESS(r, v, m, 256); for (int i = 0; i < 256; i++) printf("%d ", r[i]); printf("\n"); }
static void odd(int x) { int32_t v[3] = {x * 5, x * 7, x * 11}, r[3]; int m[3] = {x > 1, 1, x < 2}; COMPRESS(r, v, m, 3); printf("%d %d %d\n", r[0], r[1], r[2]); }
static void wide(int x) { int64_t v[32], r[32]; int m[32]; for (int i = 0; i < 32; i++) { v[i] = (int64_t)(i - x) * 1099511627776LL; m[i] = (i * x) % 5 < 2; } COMPRESS(r, v, m, 32); for (int i = 0; i < 32; i++) printf("%lld ", (long long)r[i]); printf("\n"); }
static void floats(int x) { float v[64], r[64]; int m[64]; for (int i = 0; i < 64; i++) { v[i] = (float)(i - x) * 0.5f; m[i] = (i + x) % 4 != 1; } COMPRESS(r, v, m, 64); for (int i = 0; i < 64; i++) printf("%g ", (double)r[i]); double d[2] = {(double)x * 0.25, -0.0}, e[2]; int k[2] = {x > 1, 1}; COMPRESS(e, d, k, 2); printf("%g %g\n", e[0], e[1]); }
static void flags(int x) { int v[256], r[256], m[256]; for (int i = 0; i < 256; i++) { v[i] = (i + x) % 2 == 0; m[i] = (i * 3 + x) % 7 > 2; } COMPRESS(r, v, m, 256); for (int i = 0; i < 256; i++) printf(r[i] ? "1" : "0"); printf("\n"); }
int main(int argc, char **argv) { (void)argv; one(argc); bytes(argc); odd(argc); wide(argc); floats(argc); flags(argc); return 0; }
C
(cd "$TMP" && "$WASI_CC" -O2 compress.c -o compress_oracle.wasm) || fail "compress oracle build failed"
run compress_oracle.wasm >"$TMP/compress.expected1"
run compress_oracle.wasm a b >"$TMP/compress.expected3"
for target in portable force-native; do
  for level in -O0 -O2; do
    (cd "$TMP" && "$OCAML_FAS" --os wasi --target="$target" "$level" compress.fas -o compress.wasm) \
      || fail "compress build failed for $target at $level"
    run compress.wasm >"$TMP/compress.out"
    cmp "$TMP/compress.expected1" "$TMP/compress.out" || fail "compress wrong for $target at $level"
    run compress.wasm a b >"$TMP/compress.out"
    cmp "$TMP/compress.expected3" "$TMP/compress.out" || fail "compress wrong for $target at $level"
  done
done

(cd "$TMP" && "$OCAML_FAS" --os wasi plain.fas) || fail "default WASI output failed"
test -f "$TMP/a.wasm" || fail "default WASI output is not a.wasm"
run a.wasm
test "$status" = 42 || fail "a.wasm returned $status"

cat >"$TMP/stack.fas" <<'FAS'
fn main() i32 {
    a arr[1000000, u32] = {}
    for i u32 = 0; i < 1000000; i += 1 { a[i] = i }
    return bitcast[i32](a[999999] & 255)
}
FAS
(cd "$TMP" && "$OCAML_FAS" --os wasi stack.fas -o stack.wasm) || fail "stack build failed"
run stack.wasm
test "$status" = 63 || fail "4 MB of locals failed on the WASI stack ($status)"

status=0
(cd "$TMP" && FAS_WASMTIME="$WASMTIME" "$OCAML_FAS" run --os wasi abi.fas lib.c -- arg >"$TMP/actual") \
  || status=$?
test "$status" = 5 || fail "fas run returned $status"
tail -n 1 "$TMP/actual" | grep -F ' 2 arg' >/dev/null || fail "fas run lost the program arguments"

cat >"$TMP/long.fas" <<'FAS'
use "C" <<C
long f(void) { return -1; }
C
fn main() i32 { a i32 = f(); return a + 1 }
FAS
"$OCAML_FAS" check --os wasi "$TMP/long.fas" || fail "C long is not i32 for WASI"

cat >"$TMP/missing.fas" <<'FAS'
use "C" <no_such_header_for_fas.h>
fn main() i32 { return 0 }
FAS
expect_error 'building for WASI needs wasi-sdk' check --os wasi "$TMP/missing.fas"

echo 'wasm_target: WASI triple, ILP32 C ABI under wasmtime at O0/O2, simd128 opt-in, compress, a.wasm, fas run, perf refusal: ok'
