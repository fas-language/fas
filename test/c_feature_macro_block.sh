#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
ulimit -c 0 || true

fail() {
  echo "c_feature_macro_block: $*" >&2
  exit 1
}

cat >"$TMP/late.fas" <<'FAS'
use "C" <stdio.h>
use "C" <<C
#define _GNU_SOURCE
#include <pthread.h>
#include <sched.h>
static inline void cpu_only(cpu_set_t *s, int cpu) { CPU_ZERO(s); CPU_SET(cpu, s); }
C
fn main() i32 { return 0 }
FAS
if "$OCAML_FAS" --emit-llvm "$TMP/late.fas" >"$TMP/late.out" 2>&1; then
  fail "a feature-test macro after an earlier system header was accepted"
fi
grep -F 'late.fas:3:1: error: C block defines `_GNU_SOURCE` after earlier C imports already read system headers' \
  "$TMP/late.out" >/dev/null || fail "late feature-test macro diagnostic changed"
grep -F 'help: remove this line and pass `-D_GNU_SOURCE` on the command line, or move this block before the other C imports' \
  "$TMP/late.out" >/dev/null || fail "late feature-test macro help changed"
! grep -F "ModuleID" "$TMP/late.out" >/dev/null || fail "rejected program produced LLVM"

cat >"$TMP/affinity.fas" <<'FAS'
use "C" <stdio.h>
use "C" <<C
#include <pthread.h>
#include <sched.h>
static inline void cpu_only(cpu_set_t *s, int cpu) { CPU_ZERO(s); CPU_SET(cpu, s); }
static inline int only_cpu(cpu_set_t *s, int cpu) { return CPU_COUNT(s) == 1 && CPU_ISSET(cpu, s); }
C
fn main() i32 {
  set cpu_set_t = {}
  cpu_only(&set, 0)
  r i32 = pthread_setaffinity_np(pthread_self(), sizeof[cpu_set_t], &set)
  if r != 0 { return 1 }
  got cpu_set_t = {}
  g i32 = pthread_getaffinity_np(pthread_self(), sizeof[cpu_set_t], &got)
  if g != 0 { return 2 }
  if only_cpu(&got, 0) == 0 { return 3 }
  if sched_getcpu() != 0 { return 4 }
  return 0
}
FAS
if "$OCAML_FAS" --emit-llvm "$TMP/affinity.fas" >"$TMP/no-flag.out" 2>&1; then
  fail "affinity probe compiled without -D_GNU_SOURCE"
fi
{
  printf '%s\n' 'use "C" <stdio.h>' 'use "C" <<C' '#define _GNU_SOURCE'
  sed -n '3,$p' "$TMP/affinity.fas"
} >"$TMP/keep.fas"
for case in affinity keep; do
  for level in 0 2; do
    "$OCAML_FAS" -D_GNU_SOURCE --emit-llvm "$TMP/$case.fas" >"$TMP/$case.ll"
    "$LLVM_OPT" -passes=verify "$TMP/$case.ll" -disable-output
    "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/$case.ll" -o "$TMP/$case-O$level.ll"
    "$LLVM_OPT" -passes=verify "$TMP/$case-O$level.ll" -disable-output
    "$OCAML_FAS" -O"$level" -D_GNU_SOURCE "$TMP/$case.fas" -o "$TMP/$case-O$level"
    timeout 30 "$TMP/$case-O$level" || fail "-D_GNU_SOURCE $case affinity probe failed at O$level (exit $?)"
  done
done

echo 'c_feature_macro_block: late feature-test macro rejected; -D_GNU_SOURCE affinity probes passed at O0/O2'
