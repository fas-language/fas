#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
CXX=${CXX:-clang++-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
EXPORT_TMP=$(mktemp -d)
trap 'rm -rf "$EXPORT_TMP"' EXIT HUP INT TERM
export CC LLVM_OPT LLVM_LLC
export FAS_CC="$CC" FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC"
mkdir "$EXPORT_TMP/headers"
"$OCAML_FAS" --emit-header -o "$EXPORT_TMP/headers/api.h" "$ROOT/test/c_exports/library.fas"
TMPDIR="$EXPORT_TMP" "$OCAML_FAS" --emit-header --keep -o "$EXPORT_TMP/headers/kept.h" "$ROOT/test/c_exports/library.fas" >"$EXPORT_TMP/header-keep.log" 2>&1
if grep -E 'kept C object|kept intermediate' "$EXPORT_TMP/header-keep.log"; then exit 1; fi
grep -F -- '-fsyntax-only' "$EXPORT_TMP/header-keep.log" >/dev/null
"$OCAML_FAS" --emit-header -o "$EXPORT_TMP/headers/scalars.h" "$ROOT/test/c_exports/scalars.fas"
"$OCAML_FAS" --emit-header -o "$EXPORT_TMP/headers/plain.h" "$ROOT/test/c_exports/plain.fas"
"$OCAML_FAS" --emit-header -o "$EXPORT_TMP/headers/layout.h" "$ROOT/test/c_exports/layout.fas"
printf '#include "scalars.h"\n#include "scalars.h"\n' >"$EXPORT_TMP/scalars.c"
printf '#include "api.h"\n#include "api.h"\n' >"$EXPORT_TMP/header.c"
printf '#include "kept.h"\n#include "kept.h"\n' >"$EXPORT_TMP/kept.c"
printf '#include "plain.h"\n#include "layout.h"\n' >"$EXPORT_TMP/standard.c"
"$CC" -std=c11 -Wall -Wextra -Werror -pedantic -I"$EXPORT_TMP/headers" -fsyntax-only "$EXPORT_TMP/scalars.c"
"$CXX" -x c++ -std=c++17 -Wall -Wextra -Werror -I"$EXPORT_TMP/headers" -fsyntax-only "$EXPORT_TMP/scalars.c"
"$CC" -std=c11 -Wall -Wextra -Werror -pedantic -I"$EXPORT_TMP/headers" -fsyntax-only "$EXPORT_TMP/standard.c"
"$CXX" -x c++ -std=c++17 -Wall -Wextra -Werror -I"$EXPORT_TMP/headers" -fsyntax-only "$EXPORT_TMP/standard.c"
VECTOR_PEDANTIC=-pedantic
for header_source in header.c kept.c; do
    if ! "$CC" -std=c11 -Wall -Wextra -Werror -pedantic -I"$EXPORT_TMP/headers" -fsyntax-only "$EXPORT_TMP/$header_source" >"$EXPORT_TMP/vector-gate.log" 2>&1; then
        cat "$EXPORT_TMP/vector-gate.log"
        if ! grep -E 'ext_vector_type|vector.*extension' "$EXPORT_TMP/vector-gate.log" >/dev/null; then exit 1; fi
        "$CC" -std=c11 -Wall -Wextra -Werror -I"$EXPORT_TMP/headers" -fsyntax-only "$EXPORT_TMP/$header_source"
        VECTOR_PEDANTIC=
        echo 'c_exports: vector header requires the approved non-pedantic gate'
    fi
    "$CXX" -x c++ -std=c++17 -Wall -Wextra -Werror -I"$EXPORT_TMP/headers" -fsyntax-only "$EXPORT_TMP/$header_source"
done
"$OCAML_FAS" --emit-llvm "$ROOT/test/c_exports/library.fas" >"$EXPORT_TMP/library.ll"
grep -Fx '#ifndef FAS_API_H' "$EXPORT_TMP/headers/api.h" >/dev/null
"$LLVM_OPT" -passes=verify "$EXPORT_TMP/library.ll" -disable-output
"$LLVM_OPT" -passes='default<O2>' -verify-each "$EXPORT_TMP/library.ll" -S -o "$EXPORT_TMP/library-o2.ll"
"$LLVM_OPT" -passes=verify "$EXPORT_TMP/library-o2.ll" -disable-output
"$CC" -std=c11 -Wall -Wextra -Werror $VECTOR_PEDANTIC -I"$EXPORT_TMP/headers" "$ROOT/test/c_exports/oracle.c" "$ROOT/test/c_exports/main.c" -o "$EXPORT_TMP/oracle"
"$EXPORT_TMP/oracle" >"$EXPORT_TMP/expected"
for level in 0 2; do
    "$OCAML_FAS" -O"$level" -c "$ROOT/test/c_exports/library.fas" -o "$EXPORT_TMP/library-$level.o"
    ar rcs "$EXPORT_TMP/libexports-$level.a" "$EXPORT_TMP/library-$level.o"
    "$CC" -shared "$EXPORT_TMP/library-$level.o" -o "$EXPORT_TMP/libexports-$level.so"
    readelf --dyn-syms --wide "$EXPORT_TMP/libexports-$level.so" >"$EXPORT_TMP/symbols"
    if grep '__fas_c_adapter_' "$EXPORT_TMP/symbols"; then echo 'adapter escaped' >&2; exit 1; fi
    for symbol in scalar table state fas_event fas_compare dependency_value sort_values run_events; do
        grep -E "GLOBAL +DEFAULT +[0-9]+ +$symbol$" "$EXPORT_TMP/symbols" >/dev/null
    done
    if readelf -d "$EXPORT_TMP/libexports-$level.so" | grep TEXTREL; then exit 1; fi
    for kind in a so; do
        "$CC" -std=c11 -Wall -Wextra -Werror $VECTOR_PEDANTIC -O"$level" -I"$EXPORT_TMP/headers" "$ROOT/test/c_exports/main.c" "$EXPORT_TMP/libexports-$level.$kind" -o "$EXPORT_TMP/main-$level-$kind"
        "$EXPORT_TMP/main-$level-$kind" >"$EXPORT_TMP/actual"
        diff -u "$EXPORT_TMP/expected" "$EXPORT_TMP/actual"
    done
done
cat >"$EXPORT_TMP/incompatible.fas" <<'FAS'
use "C" <<C
long exported(int x);
C
extern "C" { fn exported(x i32) i32 { return x } }
FAS
if "$OCAML_FAS" -c "$EXPORT_TMP/incompatible.fas" -o "$EXPORT_TMP/bad.o" >"$EXPORT_TMP/error" 2>&1; then exit 1; fi
grep -F "incompatible.fas:2:6: error: C compilation failed: conflicting types for 'exported'" "$EXPORT_TMP/error" >/dev/null
[ ! -e "$EXPORT_TMP/bad.o" ]
echo 'c_exports: C11/C++17 headers, callbacks, globals, static/shared libraries O0/O2: ok'
