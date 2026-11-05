#!/bin/sh
# OS-conditional `use linux|windows|wasi "file.fas"`, selected by --os.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
OS_TMP=$(mktemp -d)
trap 'rm -rf "$OS_TMP"' EXIT HUP INT TERM
DIR="$ROOT/test/os_use"

# each --os takes its own file
for pair in linux:11 windows:22 wasi:33; do
  os=${pair%:*}
  id=${pair#*:}
  "$OCAML_FAS" --os "$os" --emit-llvm "$DIR/main.fas" >"$OS_TMP/$os.ll"
  grep -q "ret i32 $id" "$OS_TMP/$os.ll"
  "$OCAML_FAS" check --os "$os" "$DIR/main.fas"
done

# and the program really runs with the Linux one
FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
  "$OCAML_FAS" --os linux "$DIR/main.fas" -o "$OS_TMP/main"
set +e
"$OS_TMP/main"
status=$?
set -e
test "$status" = 11

# a file for another OS is not read
"$OCAML_FAS" check --os linux "$DIR/missing.fas"
if "$OCAML_FAS" check --os wasi "$DIR/missing.fas" >/dev/null 2>&1; then
  echo "missing wasi file accepted under --os wasi" >&2
  exit 1
fi

# only Fas files can be conditional
printf 'use linux "C" <stdio.h>\n' >"$OS_TMP/c.fas"
if "$OCAML_FAS" check "$OS_TMP/c.fas" 2>"$OS_TMP/c.err"; then
  echo "conditional C use accepted" >&2
  exit 1
fi
grep -q "an OS-conditional use takes a Fas file" "$OS_TMP/c.err"

echo "os use: ok"
