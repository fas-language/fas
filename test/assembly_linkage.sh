#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt}
LLVM_LLC=${LLVM_LLC:-llc}
CC=${CC:-clang}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ASSEMBLY_TMP=$(mktemp -d)
trap 'rm -rf "$ASSEMBLY_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" -DASSEMBLY_TABLE_VALUE=42 --emit-header "$ROOT/test/assembly_linkage.fas" >"$ASSEMBLY_TMP/assembly_linkage.h"
! grep -F 'int32_t add3(' "$ASSEMBLY_TMP/assembly_linkage.h" >/dev/null
cat >"$ASSEMBLY_TMP/header-c.c" <<'EOF'
#include "assembly_linkage.h"
int header_table(void) { return verify_assembly_table(); }
EOF
"$CC" -Werror -std=c11 -pedantic -I"$ASSEMBLY_TMP" -c "$ASSEMBLY_TMP/header-c.c" -o "$ASSEMBLY_TMP/header-c.o"
CXX=${CXX:-clang++}
"$CXX" -Werror -std=c++17 -pedantic -I"$ASSEMBLY_TMP" -x c++ -c "$ASSEMBLY_TMP/header-c.c" -o "$ASSEMBLY_TMP/header-cpp.o"

"$OCAML_FAS" -DASSEMBLY_TABLE_VALUE=42 --emit-llvm "$ROOT/test/assembly_linkage.fas" >"$ASSEMBLY_TMP/linkage.ll"
"$LLVM_OPT" -passes=verify "$ASSEMBLY_TMP/linkage.ll" -disable-output
"$LLVM_OPT" -passes='default<O2>' -verify-each "$ASSEMBLY_TMP/linkage.ll" -S -o "$ASSEMBLY_TMP/linkage-o2.ll"
"$LLVM_OPT" -passes=verify "$ASSEMBLY_TMP/linkage-o2.ll" -disable-output
"$CC" -Werror -Wno-override-module -std=c17 -S -emit-llvm -O0 "$ROOT/test/assembly_linkage.c" -o "$ASSEMBLY_TMP/clang.ll"
PYTHONDONTWRITEBYTECODE=1 python3 "$ROOT/test/assembly_abi_parity.py" "$ASSEMBLY_TMP/linkage.ll" "$ASSEMBLY_TMP/clang.ll"

for level in 0 2; do
    FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
        "$OCAML_FAS" -DASSEMBLY_TABLE_VALUE=42 -O"$level" -c "$ROOT/test/assembly_linkage.fas" \
        -o "$ASSEMBLY_TMP/linkage-$level.o"
    "$CC" -Werror -std=c17 -O"$level" "$ASSEMBLY_TMP/linkage-$level.o" \
        "$ROOT/test/assembly_linkage.c" -o "$ASSEMBLY_TMP/linkage-$level"
    "$ASSEMBLY_TMP/linkage-$level"
done

echo "assembly linkage: ok"
