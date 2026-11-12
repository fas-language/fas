#!/bin/sh
# OS-conditional `use linux "file.fas"`, selected by --os (linux only today).
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
OS_TMP=$(mktemp -d)
trap 'rm -rf "$OS_TMP"' EXIT HUP INT TERM
DIR="$ROOT/test/os_use"

"$OCAML_FAS" --os linux --emit-llvm "$DIR/main.fas" >"$OS_TMP/linux.ll"
grep -q "ret i32 11" "$OS_TMP/linux.ll"
"$OCAML_FAS" check --os linux "$DIR/main.fas"

# and the program really runs with the Linux one
FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
  "$OCAML_FAS" --os linux "$DIR/main.fas" -o "$OS_TMP/main"
set +e
"$OS_TMP/main"
status=$?
set -e
test "$status" = 11

# dropped OSes are named in the error, and --os rejects them
if "$OCAML_FAS" check "$DIR/missing.fas" 2>"$OS_TMP/m.err"; then
  echo "use wasi accepted" >&2
  exit 1
fi
grep -q "\`wasi\` is no longer a supported OS" "$OS_TMP/m.err"
for os in windows wasi; do
  if "$OCAML_FAS" check --os "$os" "$DIR/main.fas" 2>"$OS_TMP/o.err"; then
    echo "--os $os accepted" >&2
    exit 1
  fi
  grep -q "no longer supported" "$OS_TMP/o.err"
done

# only Fas files can be conditional
printf 'use linux "C" <stdio.h>\n' >"$OS_TMP/c.fas"
if "$OCAML_FAS" check "$OS_TMP/c.fas" 2>"$OS_TMP/c.err"; then
  echo "conditional C use accepted" >&2
  exit 1
fi
grep -q "an OS-conditional use takes a Fas file" "$OS_TMP/c.err"

echo "os use: ok"
