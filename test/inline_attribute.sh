#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
INLINE_TMP=$(mktemp -d)
trap 'rm -rf "$INLINE_TMP"' EXIT HUP INT TERM

SRC="$ROOT/test/inline_attribute.fas"
"$OCAML_FAS" --emit-llvm "$SRC" >"$INLINE_TMP/inline.ll"
"$LLVM_OPT" -passes=verify "$INLINE_TMP/inline.ll" -disable-output
test "$(grep -c '^define.* alwaysinline ' "$INLINE_TMP/inline.ll")" = 3
grep -q '^define internal i64 @plain(i64 %a0) #' "$INLINE_TMP/inline.ll"

# --no-inline wins over @inline
"$OCAML_FAS" --no-inline add3 --emit-llvm "$SRC" >"$INLINE_TMP/no.ll"
grep -q '^define internal i64 @add3(.*) noinline ' "$INLINE_TMP/no.ll"

for level in 0 2; do
  FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
    "$OCAML_FAS" -O"$level" "$SRC" -o "$INLINE_TMP/inline-$level"
  "$INLINE_TMP/inline-$level"
done

printf 'fn f() i32 @inlin {\n  return 0\n}\nfn main() i32 { return f() }\n' >"$INLINE_TMP/bad.fas"
if "$OCAML_FAS" --emit-llvm "$INLINE_TMP/bad.fas" >/dev/null 2>&1; then
  echo "unknown attribute accepted" >&2
  exit 1
fi

echo "inline attribute: ok"
