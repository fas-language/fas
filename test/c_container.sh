#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
CONTAINER_TMP=$(mktemp -d)
trap 'rm -rf "$CONTAINER_TMP"' EXIT HUP INT TERM
TMPDIR=$CONTAINER_TMP/tmp
mkdir "$TMPDIR"
export TMPDIR

fail() {
  echo "c_container: $*" >&2
  exit 1
}

cleanup_kept() {
  sed -n 's/^fas: kept [^:]*: //p' "$1" | tr ', ' '\n' |
    while IFS= read -r path; do
      [ -n "$path" ] || continue
      case "$path" in
        "$TMPDIR"/*) rm -f "$path" ;;
        *) fail "kept path escaped the test temporary directory: $path" ;;
      esac
    done
}

expect_failure() {
  status=0
  "$@" >"$CONTAINER_TMP/stdout" 2>"$CONTAINER_TMP/stderr" || status=$?
  [ "$status" -ne 0 ] || fail "command unexpectedly succeeded: $*"
  [ ! -s "$CONTAINER_TMP/stdout" ] || fail "failure wrote to stdout: $*"
}

mkdir "$CONTAINER_TMP/deps"
cat >"$CONTAINER_TMP/deps/first.fas" <<'FAS'
use "C" <<FIRST
#define FIRST_VALUE 17
FIRST
use "C" <<SECOND
#include <stdio.h>
int fragment_value(void) { return FIRST_VALUE; }
int c_calls_fas(void) {
  extern int fas_exported_value(void);
  return fas_exported_value();
}
static inline int fas_static_value(int value) { return value + 2; }
static int fas_unused_static_helper(void) { return 99; }
int fas_container_global = 13;
void fas_report_result(int value) { printf("%d\n", value); }
SECOND
FAS
cat >"$CONTAINER_TMP/deps/second.fas" <<'FAS'
use "C" <<OTHER
#ifdef FIRST_VALUE
#error macro escaped across Fas translation units
#endif
#define SECOND_VALUE 23
int other_value(void) { return SECOND_VALUE; }
OTHER
FAS
cat >"$CONTAINER_TMP/program.fas" <<'FAS'
use "deps/first.fas"
use "deps/second.fas"
extern "C" {
  var fas_exported_global i32 = 10
  fn fas_exported_value() i32 { return 10 }
}
fn main() i32 {
  result i32 = fragment_value() + other_value()
  result += c_calls_fas()
  result += fas_static_value(1)
  result += fas_container_global
  fas_report_result(result)
  if result != 66 { return 1 }
  return 0
}
FAS
cat >"$CONTAINER_TMP/oracle.c" <<'C'
#include <stdio.h>
int main(void) {
  printf("66\n");
  return 0;
}
C
"$CC" "$CONTAINER_TMP/oracle.c" -o "$CONTAINER_TMP/oracle"
"$CONTAINER_TMP/oracle" >"$CONTAINER_TMP/oracle.out"

for level in 0 2; do
  "$OCAML_FAS" -O"$level" --emit-llvm "$CONTAINER_TMP/program.fas" \
    >"$CONTAINER_TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$CONTAINER_TMP/program.O$level.ll" -disable-output
  "$LLVM_OPT" "-passes=default<O$level>" -verify-each \
    "$CONTAINER_TMP/program.O$level.ll" -S -o "$CONTAINER_TMP/optimized.O$level.ll"
  "$LLVM_OPT" -passes=verify "$CONTAINER_TMP/optimized.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$CONTAINER_TMP/program.fas" \
    -o "$CONTAINER_TMP/program.O$level"
  "$CONTAINER_TMP/program.O$level" >"$CONTAINER_TMP/program.O$level.out" \
    || fail "container executable failed at O$level"
  cmp -s "$CONTAINER_TMP/oracle.out" "$CONTAINER_TMP/program.O$level.out" \
    || fail "container output differed from the C oracle at O$level"
  "$OCAML_FAS" -c -O"$level" "$CONTAINER_TMP/program.fas" -o "$CONTAINER_TMP/visibility.o"
  "$CC" -shared "$CONTAINER_TMP/visibility.o" -o "$CONTAINER_TMP/visibility.so"
  readelf --dyn-syms --wide "$CONTAINER_TMP/visibility.so" >"$CONTAINER_TMP/dynsym"
  for symbol in fas_exported_value fas_exported_global fragment_value; do
    grep -E "GLOBAL +DEFAULT +[0-9]+ +$symbol$" "$CONTAINER_TMP/dynsym" >/dev/null \
      || fail "O$level missing default-visible export $symbol"
  done
  if grep -F '__fas_c_adapter_' "$CONTAINER_TMP/dynsym"; then
    fail "O$level adapter escaped into the shared library"
  fi
  readelf -d "$CONTAINER_TMP/visibility.so" >"$CONTAINER_TMP/dynamic"
  if grep -F TEXTREL "$CONTAINER_TMP/dynamic"; then fail "O$level shared library has TEXTREL"; fi
done

"$OCAML_FAS" -c -O0 "$CONTAINER_TMP/program.fas" -o "$CONTAINER_TMP/container.o"
nm "$CONTAINER_TMP/container.o" >"$CONTAINER_TMP/container.nm"
grep -E ' [A-Za-z] __fas_c_adapter_.*_fas_static_value$' \
  "$CONTAINER_TMP/container.nm" >/dev/null || fail "used static adapter was absent from object"
if grep -E '__fas_c_adapter_.*_fas_unused_static_helper$' \
  "$CONTAINER_TMP/container.nm" >/dev/null; then
  fail "unused static function produced an adapter symbol"
fi
grep -E ' [BD] fas_container_global$' "$CONTAINER_TMP/container.nm" >/dev/null \
  || fail "container-defined C global was absent from object"

cat >"$CONTAINER_TMP/object.fas" <<'FAS'
use "C" <<END
int container_value(void) { return 11; }
END
extern "C" {
  fn fas_container_value() i32 { return container_value() + 31 }
}
FAS
cat >"$CONTAINER_TMP/oracle.c" <<'C'
extern int fas_container_value(void);
int main(void) { return fas_container_value() == 42 ? 0 : 1; }
C
for level in 0 2; do
  "$OCAML_FAS" -c -O"$level" "$CONTAINER_TMP/object.fas" \
    -o "$CONTAINER_TMP/fas.O$level.o"
  "$CC" "$CONTAINER_TMP/oracle.c" "$CONTAINER_TMP/fas.O$level.o" \
    -o "$CONTAINER_TMP/object.O$level"
  "$CONTAINER_TMP/object.O$level" || fail "merged object failed at O$level"
done

mkdir "$CONTAINER_TMP/include" "$CONTAINER_TMP/system"
cat >"$CONTAINER_TMP/include/fas_config.h" <<'C'
#if CONTAINER_VALUE != 25
#error CONTAINER_VALUE was not passed to the header importer
#endif
#define FAS_HEADER_VALUE 17
C
cat >"$CONTAINER_TMP/system/fas_system.h" <<'C'
#define FAS_SYSTEM_VALUE 8
C
cat >"$CONTAINER_TMP/quoted.h" <<'C'
int relative_header_value(void);
C
cat >"$CONTAINER_TMP/include/quoted.h" <<'C'
int relative_header_value(double value);
C
cat >"$CONTAINER_TMP/preprocessor.fas" <<'FAS'
use "C" "quoted.h"
use "C" <<END
#include <fas_config.h>
#include <fas_system.h>
int container_value(void) {
  return FAS_HEADER_VALUE + FAS_SYSTEM_VALUE + CONTAINER_VALUE;
}
int relative_header_value(void) { return 1; }
END
extern "C" {
  fn linked_c_value() i32
}
fn main() i32 {
  return container_value() + linked_c_value() + relative_header_value() - 101
}
FAS
cat >"$CONTAINER_TMP/link-input.c" <<'C'
#include <fas_config.h>
#include <fas_system.h>
int linked_c_value(void) {
  return FAS_HEADER_VALUE + FAS_SYSTEM_VALUE + CONTAINER_VALUE;
}
C
"$OCAML_FAS" --keep -O2 -g -I "$CONTAINER_TMP/include" \
  -isystem"$CONTAINER_TMP/system" -D CONTAINER_VALUE=25 \
  "$CONTAINER_TMP/preprocessor.fas" "$CONTAINER_TMP/link-input.c" \
  -o "$CONTAINER_TMP/preprocessor" >"$CONTAINER_TMP/stdout" \
  2>"$CONTAINER_TMP/stderr"
"$CONTAINER_TMP/preprocessor" || fail "preprocessor flags did not reach C sources"
grep -F -- "-I $CONTAINER_TMP/include -isystem$CONTAINER_TMP/system -D CONTAINER_VALUE=25" \
  "$CONTAINER_TMP/stderr" >/dev/null \
  || fail "Clang commands omitted ordered preprocessor flags"

cat >"$CONTAINER_TMP/header-only.fas" <<'FAS'
use "C" <stdint.h>
fn main() i32 { return 0 }
FAS
"$OCAML_FAS" --keep -g "$CONTAINER_TMP/header-only.fas" \
  -o "$CONTAINER_TMP/header-only" >"$CONTAINER_TMP/stdout" \
  2>"$CONTAINER_TMP/stderr"
"$CONTAINER_TMP/header-only" || fail "header-only program failed"
if grep -F "fas: CC command: $CC --target=x86_64-unknown-linux-gnu -fPIC " \
  "$CONTAINER_TMP/stderr" >/dev/null; then
  fail "header-only unit without adapters was compiled"
fi
cleanup_kept "$CONTAINER_TMP/stderr"

mkdir "$CONTAINER_TMP/duplicate"
cat >"$CONTAINER_TMP/duplicate/first.fas" <<'FAS'
use "C" <<END
int duplicate(void) { return 1; }
END
FAS
cat >"$CONTAINER_TMP/duplicate/second.fas" <<'FAS'
use "C" <<END
int duplicate(void) { return 2; }
END
FAS
cat >"$CONTAINER_TMP/duplicate/main.fas" <<'FAS'
use "first.fas"
use "second.fas"
fn main() i32 { return 0 }
FAS
expect_failure "$OCAML_FAS" "$CONTAINER_TMP/duplicate/main.fas" \
  -o "$CONTAINER_TMP/duplicate/program"
grep -F "multiple definition of" "$CONTAINER_TMP/stderr" \
  >/dev/null || fail "duplicate C definition omitted the linker's message"

"$OCAML_FAS" -S "$CONTAINER_TMP/program.fas" -o "$CONTAINER_TMP/program.s"
[ -s "$CONTAINER_TMP/program.s" ] || fail "-S omitted Fas assembly"
"$OCAML_FAS" --emit-ir "$CONTAINER_TMP/program.fas" >"$CONTAINER_TMP/program.ir"
[ -s "$CONTAINER_TMP/program.ir" ] || fail "--emit-ir omitted Fas IR"

cat >"$CONTAINER_TMP/broken.fas" <<'FAS'
use "C" <<END
int broken(void) { return missing_name; }
END
fn main() i32 { return 0 }
FAS
for mode in -S --emit-llvm --emit-ir; do
  expect_failure "$OCAML_FAS" "$mode" "$CONTAINER_TMP/broken.fas"
  grep -F "$CONTAINER_TMP/broken.fas:2:" "$CONTAINER_TMP/stderr" >/dev/null \
    || fail "$mode C diagnostic was not mapped to the Fas line"
  grep -F "undeclared identifier 'missing_name'" "$CONTAINER_TMP/stderr" \
    >/dev/null || fail "$mode omitted the first C error"
done

cat >"$CONTAINER_TMP/unbalanced.fas" <<'FAS'
use "C" <<IF
#if 1
int unbalanced(void) { return 1; }
IF
fn main() i32 { return 0 }
FAS
expect_failure "$OCAML_FAS" --emit-llvm "$CONTAINER_TMP/unbalanced.fas"
grep -F "$CONTAINER_TMP/unbalanced.fas:2:" "$CONTAINER_TMP/stderr" >/dev/null \
  || fail "unbalanced #if error was not mapped to the Fas line"
grep -F "unterminated conditional directive" "$CONTAINER_TMP/stderr" >/dev/null \
  || fail "unbalanced #if omitted Clang's diagnostic"

cat >"$CONTAINER_TMP/syntax-error.fas" <<'FAS'
use "C" <<ERR
int broken(void) { return (1 + ; }
ERR
fn main() i32 { return 0 }
FAS
expect_failure "$OCAML_FAS" --emit-llvm "$CONTAINER_TMP/syntax-error.fas"
grep -F "$CONTAINER_TMP/syntax-error.fas:2:" "$CONTAINER_TMP/stderr" >/dev/null \
  || fail "C syntax error was not mapped to the Fas line"
grep -F "$CONTAINER_TMP/syntax-error.fas:2:32: error: C compilation failed: expected expression" "$CONTAINER_TMP/stderr" >/dev/null \
  || fail "C syntax error has the wrong primary message"

cat >"$CONTAINER_TMP/broken.h" <<'C'
extern int broken = ;
C
cat >"$CONTAINER_TMP/header-error.fas" <<'FAS'
use "C" "broken.h"
fn main() i32 { return 0 }
FAS
expect_failure "$OCAML_FAS" --emit-llvm "$CONTAINER_TMP/header-error.fas"
grep -F "$CONTAINER_TMP/header-error.fas:1:9: error: C compilation failed: expected expression" "$CONTAINER_TMP/stderr" >/dev/null \
  || fail "header error has the wrong primary message"
grep -Fx "note: $CONTAINER_TMP/broken.h:1:21" "$CONTAINER_TMP/stderr" >/dev/null \
  || fail "header error omitted its foreign location"

"$OCAML_FAS" --keep -g "$CONTAINER_TMP/program.fas" \
  -o "$CONTAINER_TMP/kept" >"$CONTAINER_TMP/stdout" 2>"$CONTAINER_TMP/stderr"
grep -F "fas: kept C import unit:" "$CONTAINER_TMP/stderr" >/dev/null \
  || fail "--keep omitted the generated C unit"
grep -F "fas: kept C fragment:" "$CONTAINER_TMP/stderr" >/dev/null \
  || fail "--keep omitted the generated fragment"
grep -F "fas: kept C object:" "$CONTAINER_TMP/stderr" >/dev/null \
  || fail "--keep omitted the generated C object"
bindings=$(sed -n 's/^fas: kept C bindings: //p' "$CONTAINER_TMP/stderr")
[ -s "$bindings" ] || fail "--keep omitted the C bindings manifest"
grep -F "adapter for fas_static_value" "$bindings" >/dev/null \
  || fail "bindings manifest omitted the static adapter"
grep -F "fas: CC command: $CC --target=x86_64-unknown-linux-gnu -fPIC -O2 -Xclang -main-file-name -Xclang " \
  "$CONTAINER_TMP/stderr" | grep -E 'fas-c-import-[0-9a-f]{32}\.c -ffile-prefix-map=[^ ]+=fas-c-import-[0-9a-f]{32}\.c -c ' \
  >/dev/null || fail "-g omitted the reproducible C compile command"
cleanup_kept "$CONTAINER_TMP/stderr"

echo "c_container: automatic C build, object merge and mapped errors: ok"
