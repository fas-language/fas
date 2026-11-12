#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
COMPONENT_TMP=$(mktemp -d)
trap 'rm -rf "$COMPONENT_TMP"' EXIT HUP INT TERM
ulimit -c 0 || true

"$CC" -std=c11 -Wall -Wextra -Werror -pedantic \
  "$ROOT/test/entry_oracle.c" -o "$COMPONENT_TMP/oracle"
"$COMPONENT_TMP/oracle"

for level in 0 2; do
  for fixture in entry_empty entry_args; do
    source="$ROOT/test/$fixture.fas"
    "$OCAML_FAS" --emit-llvm -O"$level" "$source" >"$COMPONENT_TMP/$fixture-O$level.ll"
    "$LLVM_OPT" -passes=verify "$COMPONENT_TMP/$fixture-O$level.ll" -disable-output
    "$LLVM_OPT" -S "-passes=default<O$level>" "$COMPONENT_TMP/$fixture-O$level.ll" \
      -o "$COMPONENT_TMP/$fixture-O$level.opt.ll"
    "$LLVM_OPT" -passes=verify "$COMPONENT_TMP/$fixture-O$level.opt.ll" -disable-output
    "$OCAML_FAS" -O"$level" "$source" -o "$COMPONENT_TMP/$fixture-O$level"
    "$COMPONENT_TMP/$fixture-O$level"
  done
done

"$OCAML_FAS" -c "$ROOT/test/entry_no_entry.fas" \
  -o "$COMPONENT_TMP/no-entry.o"
[ -s "$COMPONENT_TMP/no-entry.o" ]

if "$OCAML_FAS" "$ROOT/test/entry_no_entry.fas" -o "$COMPONENT_TMP/no-main" \
  2>"$COMPONENT_TMP/no-main.err"; then
  echo 'entry_signatures: executable without fn main was accepted' >&2
  exit 1
fi
grep -Fx "error: \`$ROOT/test/entry_no_entry.fas\` has no \`fn main\`" \
  "$COMPONENT_TMP/no-main.err" >/dev/null
[ ! -e "$COMPONENT_TMP/no-main" ]

echo 'entry_signatures: both documented entry signatures at O0/O2; no-main object: ok; no-main executable: rejected'
