#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/machine_integers.fas" >"$TMP/input.ll"
"$LLVM_OPT" -passes=verify "$TMP/input.ll" -disable-output
if grep -Eq '(zext|sext|trunc) i64 ' "$TMP/input.ll"; then
  echo "machine integers: equal-width i64 cast emitted" >&2
  exit 1
fi

cat >"$TMP/implicit.fas" <<'EOF'
fn take_usize(value usize) usize { return value }
fn take_isize(value isize) isize { return value }
fn pass_unsigned(value u64) usize { return take_usize(value) }
fn pass_signed(value i64) isize { return take_isize(value) }
fn mixed_unsigned(a u64, b usize) usize { return a + b }
fn mixed_signed(a i64, b isize) isize { return a - b }
fn less_unsigned(a u64, b usize) bool { return a < b }
fn less_signed(a i64, b isize) bool { return a < b }
EOF
cat >"$TMP/explicit.fas" <<'EOF'
fn take_usize(value usize) usize { return value }
fn take_isize(value isize) isize { return value }
fn pass_unsigned(value u64) usize { return take_usize(bitcast[usize](value)) }
fn pass_signed(value i64) isize { return take_isize(bitcast[isize](value)) }
fn mixed_unsigned(a u64, b usize) usize { return bitcast[usize](a) + b }
fn mixed_signed(a i64, b isize) isize { return bitcast[isize](a) - b }
fn less_unsigned(a u64, b usize) bool { return bitcast[usize](a) < b }
fn less_signed(a i64, b isize) bool { return bitcast[isize](a) < b }
EOF
"$OCAML_FAS" --emit-llvm "$TMP/implicit.fas" >"$TMP/implicit.ll"
"$OCAML_FAS" --emit-llvm "$TMP/explicit.fas" >"$TMP/explicit.ll"
"$LLVM_OPT" -passes=verify "$TMP/implicit.ll" -disable-output
"$LLVM_OPT" -passes=verify "$TMP/explicit.ll" -disable-output
cmp -s "$TMP/implicit.ll" "$TMP/explicit.ll" || {
  echo "machine integers: implicit and explicit bitcast LLVM differ" >&2
  exit 1
}

for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/input.ll" \
    -o "$TMP/input.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/input.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$TMP/fas.O$level" \
    "$ROOT/test/machine_integers.fas" "$ROOT/test/machine_integers_oracle.c"
  "$CC" -Werror -std=c17 -O"$level" -DMACHINE_INTEGERS_ORACLE \
    "$ROOT/test/machine_integers_oracle.c" -o "$TMP/oracle.O$level"
  timeout 30 "$TMP/fas.O$level" >"$TMP/fas.O$level.out"
  timeout 30 "$TMP/oracle.O$level" >"$TMP/oracle.O$level.out"
  cmp -s "$TMP/fas.O$level.out" "$TMP/oracle.O$level.out" || {
    echo "machine integers: Fas and C results differ at O$level" >&2
    exit 1
  }
done

echo "machine integers: equal-range u64/usize and i64/isize conversions, C oracle and LLVM equivalence at O0/O2: ok"
