#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
export DEBUGINFOD_URLS=
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
fail() { echo "const_loops: $*" >&2; exit 1; }

"$OCAML_FAS" --emit-llvm "$ROOT/test/const_loops.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
"$LLVM_OPT" -passes='print<loops>' -disable-output "$TMP/program.ll" 2>"$TMP/loops.txt"
awk '
  /^Loop info for function / { name = substr($5, 2, length($5) - 3); next }
  /^Loop at depth/ { count[name] += 1; if ($4 != "1") deep[name] = 1 }
  END {
    split("mix4 skip4 sizes pruned triangle signed_range narrow first_match deferred", none, " ")
    for (i in none) if (count[none[i]] > 0) { print none[i] " has a loop"; bad = 1 }
    for (name in count) if (name ~ /dense_loop|window/) { print name " has a loop"; bad = 1 }
    if (count["labeled"] != 1 || deep["labeled"]) { print "labeled is not one runtime loop"; bad = 1 }
    exit bad
  }
' "$TMP/loops.txt" || fail "expanded copies contain a loop"
loop_count=$(grep -c '^define .*@"dense_loop\$spec' "$TMP/program.ll")
recursive_count=$(grep -c '^define .*@"dense_for\$spec' "$TMP/program.ll")
dense_count=$(grep -c '^define .*@"dense\$spec' "$TMP/program.ll")
[ "$loop_count" = 1 ] || fail "dense_loop has $loop_count instances"
[ "$recursive_count" = 63 ] || fail "dense_for has $recursive_count instances"
[ "$dense_count" = 32 ] || fail "dense has $dense_count instances"
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" \
    -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$TMP/fas.O$level" "$ROOT/test/const_loops.fas"
  "$CC" -Werror -Wall -std=c17 -O"$level" \
    "$ROOT/test/const_loops_oracle.c" -o "$TMP/oracle.O$level"
  timeout 30 "$TMP/fas.O$level" >"$TMP/fas.O$level.out" \
    || fail "Fas O$level behavior failed"
  timeout 30 "$TMP/oracle.O$level" >"$TMP/oracle.O$level.out" \
    || fail "C O$level oracle failed"
  cmp -s "$TMP/fas.O$level.out" "$TMP/oracle.O$level.out" \
    || fail "O$level output differs from the C oracle"
done
echo "const_loops: expanded copies match C at O0/O2, no back edges, 1 vs 63 dispatch instances; LLVM: ok"
