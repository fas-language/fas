#!/bin/sh
# assert and unreachable through the built-in backend and the LLVM oracle at
# O0 and O2: a holding assert does nothing; a failing one writes
# "file:line: message" to stderr and traps (any signal: it is unspecified).
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ASSERT_TMP=$(mktemp -d)
trap 'rm -rf "$ASSERT_TMP"' EXIT HUP INT TERM

# check <oracle-level> <want-status> <want-stderr> <args...>
check() {
  what=$1 want=$2 want_err=$3
  shift 3
  status=0
  "$ASSERT_TMP/a" "$@" 2>"$ASSERT_TMP/err" || status=$?
  # the shell may add its own "Illegal instruction" line after the program's
  err=$(head -n 1 "$ASSERT_TMP/err")
  if [ "$want" = trap ]; then
    [ "$status" -gt 128 ] || { echo "assert: $what ($#): status $status, want a trap" >&2; exit 1; }
  else
    [ "$status" = "$want" ] || { echo "assert: $what ($#): status $status, want $want" >&2; exit 1; }
  fi
  [ "$err" = "$want_err" ] || { echo "assert: $what ($#): stderr '$err', want '$want_err'" >&2; exit 1; }
}

ulimit -c 0
cp "$ROOT/test/assert.fas" "$ASSERT_TMP/assert.fas"
for oracle in native llvm; do
  for level in 0 2; do
    if [ "$oracle" = llvm ]; then
      (cd "$ASSERT_TMP" && FAS_ORACLE=llvm "$OCAML_FAS" -O"$level" assert.fas -o a)
    else
      (cd "$ASSERT_TMP" && env -u FAS_ORACLE "$OCAML_FAS" -O"$level" assert.fas -o a)
    fi
    w="$oracle -O$level"
    check "$w" 0 ""
    check "$w" 10 "" x
    check "$w" trap "assert.fas:12: assertion failed" x x
    check "$w" trap "assert.fas:6: reached unreachable()" x x x
    check "$w" trap "assert.fas:11: assertion failed: too many arguments" x x x x
  done
done

echo "assert: ok"
