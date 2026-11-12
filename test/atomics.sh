#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
# Atomic builtins (atomic_load/store/add/sub/and/or/xor/xchg/cas, atomic_fence):
# - single-thread semantics through LLVM at -O0/-O2, the native backend, and
#   the native backend after a Fir round trip;
# - the LLVM text has the real atomic instructions with natural alignment;
# - a pthread driver hammers exported Fas functions built by both backends;
# - misuse is diagnosed.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT HUP INT TERM
D=$ROOT/test/atomics

run() {
  label=$1
  shift
  "$OCAML_FAS" "$@" "$D/single.fas" -o "$T/single"
  if ! "$T/single"; then
    echo "atomics: $label: single.fas failed with $?"
    exit 1
  fi
}
run "llvm -O0" -O0
run "llvm -O2" -O2
run native --backend=native
FAS_FIR=check run "native, Fir checked" --backend=native

"$OCAML_FAS" --emit-llvm "$D/single.fas" > "$T/single.ll"
for pattern in \
  'store atomic i64 10, ptr %v[0-9]* release, align 8' \
  'atomicrmw add ptr %v[0-9]*, i64 5 seq_cst, align 8' \
  'load atomic i16, ptr %v[0-9]* seq_cst, align 2' \
  'cmpxchg ptr %v[0-9]*, i32 13, i32 99 acq_rel acquire, align 4' \
  'atomicrmw or ptr %v[0-9]*, i32 8 monotonic, align 4' \
  'fence acq_rel' \
  'atomicrmw xchg ptr %v[0-9]*, ptr %v[0-9]* seq_cst, align 8'; do
  if ! grep -q "$pattern" "$T/single.ll"; then
    echo "atomics: LLVM text lacks: $pattern"
    exit 1
  fi
done

"$CC" -O2 -c "$D/threads.c" -o "$T/driver.o"
for backend in llvm native; do
  "$OCAML_FAS" --backend=$backend -c "$D/threads.fas" -o "$T/threads.o"
  "$CC" "$T/driver.o" "$T/threads.o" -pthread -o "$T/threads"
  if ! "$T/threads"; then
    echo "atomics: $backend: threaded driver failed"
    exit 1
  fi
done

expect_error() {
  name=$1
  message=$2
  body=$3
  printf 'var g u32 = 0\nfn main() i32 {\n    p addr = &g\n%s\n    return 0\n}\n' "$body" \
    > "$T/$name.fas"
  if "$OCAML_FAS" check "$T/$name.fas" > "$T/$name.out" 2>&1; then
    echo "atomics: $name: expected an error"
    exit 1
  fi
  if ! grep -qF "$message" "$T/$name.out"; then
    echo "atomics: $name: wrong diagnostic:"
    cat "$T/$name.out"
    exit 1
  fi
}
expect_error release_load "an atomic load cannot have release or acq_rel ordering" \
  '    x u32 = atomic_load[u32, release](p)'
expect_error acquire_store "an atomic store cannot have acquire or acq_rel ordering" \
  '    atomic_store[u32, acquire](p, 1)'
expect_error relaxed_fence "a fence cannot be relaxed" '    atomic_fence[relaxed]()'
expect_error bad_order "unknown memory ordering \`sometimes\`" \
  '    x u32 = atomic_load[u32, sometimes](p)'
expect_error no_order "\`atomic_add\` expects a type and an ordering" \
  '    x u32 = atomic_add[u32](p, 1)'
expect_error float_add "\`atomic_add\` needs an integer type, got \`f64\`" \
  '    x f64 = atomic_add[f64, relaxed](p, 1.0)'
expect_error addr_or "\`atomic_or\` needs an integer type, got \`addr\`" \
  '    x addr = atomic_or[addr, relaxed](p, p)'
expect_error arity "builtin \`atomic_cas\` expects 3 arguments" \
  '    x u32 = atomic_cas[u32, relaxed](p, 1)'

echo "atomics: ok"
