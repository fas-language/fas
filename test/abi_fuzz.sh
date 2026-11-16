#!/bin/sh
# ABI fuzzing, Fir <-> C both ways (test/abi_fuzz.ml).
#
# Runs ABI_FUZZ_COUNT random seeds (default 100) starting at ABI_FUZZ_SEED
# (default 1). Each seed is a C file and a Fir module calling each other
# with random signatures; the C side is built by ABI_FUZZ_CC (default gcc,
# whose ABI the native backend follows where gcc and clang disagree), the
# Fir side natively and through LLVM (FAS_CC, default clang-22). Failing
# pairs, with a reduced abi_fuzz_SEED_min pair, are kept in ABI_FUZZ_KEEP
# (default: a temporary directory, printed on failure).
#
# Not part of `dune runtest`: it needs gcc and clang and takes a while.
# Usage:
#   ABI_FUZZ=_build/default/test/abi_fuzz.exe sh test/abi_fuzz.sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
ABI_FUZZ=${ABI_FUZZ:-$ROOT/_build/default/test/abi_fuzz.exe}
CC=${ABI_FUZZ_CC:-gcc}
LLVM_CC=${FAS_CC:-clang-22}
COUNT=${ABI_FUZZ_COUNT:-100}
SEED=${ABI_FUZZ_SEED:-1}
KEEP=${ABI_FUZZ_KEEP:-$(mktemp -d)}
mkdir -p "$KEEP"

if ! command -v "$CC" >/dev/null 2>&1 || ! command -v "$LLVM_CC" >/dev/null 2>&1; then
  echo "abi fuzz: $CC or $LLVM_CC not found, skipped"
  exit 0
fi

if ! "$ABI_FUZZ" -cc "$CC" -llvm-cc "$LLVM_CC" -keep "$KEEP" -seed "$SEED" -n "$COUNT" -reduce; then
  echo "abi fuzz: failing pairs kept in $KEEP"
  exit 1
fi
[ -n "${ABI_FUZZ_KEEP:-}" ] || rm -rf "$KEEP"
echo "abi fuzz: ok"
