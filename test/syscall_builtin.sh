#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
SYSCALL_TMP=$(mktemp -d)
trap 'rm -rf "$SYSCALL_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/syscall_builtin.fas" \
  >"$SYSCALL_TMP/sc.ll"
"$LLVM_OPT" -passes=verify "$SYSCALL_TMP/sc.ll" -disable-output
grep -q 'asm sideeffect "syscall"' "$SYSCALL_TMP/sc.ll"
if "$OCAML_FAS" --os windows --emit-llvm "$ROOT/test/syscall_builtin.fas" >/dev/null 2>&1; then
  echo "syscall accepted on windows" >&2; exit 1
fi

for level in 0 2; do
  FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
    "$OCAML_FAS" -O"$level" "$ROOT/test/syscall_builtin.fas" \
    -o "$SYSCALL_TMP/sc-$level"
  test "$("$SYSCALL_TMP/sc-$level")" = "hello from syscall"
done

echo "syscall builtin: ok"
