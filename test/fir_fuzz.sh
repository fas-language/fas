#!/bin/sh
# Differential fuzzing of the native backend against LLVM (test/fir_fuzz.ml).
#
# First replays the reduced past finds in test/fir_fuzz/*.fir, then runs
# FIR_FUZZ_COUNT random modules (default 200) starting at FIR_FUZZ_SEED
# (default 1). Mismatching modules are kept in FIR_FUZZ_KEEP (default: a
# temporary directory, printed on failure).
#
# Not part of `dune runtest`: it needs clang and takes a while. Usage:
#   FIR_FUZZ=_build/default/test/fir_fuzz.exe sh test/fir_fuzz.sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
FIR_FUZZ=${FIR_FUZZ:-$ROOT/_build/default/test/fir_fuzz.exe}
CC=${CC:-clang-22}
export CC
COUNT=${FIR_FUZZ_COUNT:-200}
SEED=${FIR_FUZZ_SEED:-1}
KEEP=${FIR_FUZZ_KEEP:-$(mktemp -d)}

status=0
for f in "$ROOT"/test/fir_fuzz/*.fir; do
  if ! "$FIR_FUZZ" -keep "$KEEP" -file "$f" >/dev/null 2>&1; then
    echo "fir fuzz: regression $(basename "$f") failed"
    "$FIR_FUZZ" -keep "$KEEP" -file "$f" -v 2>&1 | tail -5
    status=1
  fi
done

"$FIR_FUZZ" -keep "$KEEP" -seed "$SEED" -n "$COUNT" || status=1

if [ "$status" != 0 ]; then
  echo "fir fuzz: failing modules kept in $KEEP"
  exit 1
fi
[ -n "${FIR_FUZZ_KEEP:-}" ] || rm -rf "$KEEP"
echo "fir fuzz: ok"
