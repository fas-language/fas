#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
case $OCAML_FAS in
  /*) ;;
  *) OCAML_FAS=$(pwd)/$OCAML_FAS ;;
esac
C_PKG_TMP=$(mktemp -d)
trap 'rm -rf "$C_PKG_TMP"' EXIT HUP INT TERM
TMPDIR=$C_PKG_TMP
export TMPDIR

fail() {
  echo "c_pkg: $*" >&2
  exit 1
}

run_diag() {
  expected=$1
  shift
  status=0
  "$@" >"$C_PKG_TMP/diag.out" 2>&1 || status=$?
  if [ "$status" -ne "$expected" ]; then
    cat "$C_PKG_TMP/diag.out" >&2
    fail "exit $status, expected $expected: $*"
  fi
}

expect_lines() {
  for line in "$@"; do
    if ! grep -Fxq -- "$line" "$C_PKG_TMP/diag.out"; then
      cat "$C_PKG_TMP/diag.out" >&2
      fail "missing output line: $line"
    fi
  done
}

expect_exit() {
  expected=$1
  shift
  status=0
  timeout 30 "$@" || status=$?
  [ "$status" -eq "$expected" ] || fail "exit $status, expected $expected: $*"
}

link_oracle() {
  source=$1
  shift
  "$CC" -std=c17 -Werror "$source" "$@" -o "$C_PKG_TMP/oracle.bin" \
    || fail "C oracle $source did not compile"
  "$C_PKG_TMP/oracle.bin"
}

count_word() {
  printf '%s\n' "$1" | tr ' ' '\n' | grep -Fxc -- "$2" || true
}

oracle() {
  source=$1
  shift
  "$CC" -std=c17 -Werror "$@" "$source" -o "$C_PKG_TMP/oracle.bin" \
    || fail "C oracle $source did not compile"
  "$C_PKG_TMP/oracle.bin"
}

run_case() {
  name=$1
  expected=$2
  shift 2
  "$OCAML_FAS" --emit-llvm "$@" >"$C_PKG_TMP/$name.ll" || fail "$name: --emit-llvm failed"
  "$LLVM_OPT" -passes=verify "$C_PKG_TMP/$name.ll" -disable-output
  for level in 0 2; do
    "$LLVM_OPT" -S "-passes=default<O$level>" "$C_PKG_TMP/$name.ll" \
      -o "$C_PKG_TMP/$name.O$level.ll"
    "$LLVM_OPT" -passes=verify "$C_PKG_TMP/$name.O$level.ll" -disable-output
    "$OCAML_FAS" "-O$level" -o "$C_PKG_TMP/$name.O$level" "$@" \
      || fail "$name: -O$level build failed"
    status=0
    timeout 30 "$C_PKG_TMP/$name.O$level" || status=$?
    [ "$status" -eq "$expected" ] \
      || fail "$name: -O$level exited $status, C oracle printed $expected"
  done
}

PROG=$C_PKG_TMP/prog
INC=$C_PKG_TMP/inc
INC2=$C_PKG_TMP/inc2
CLI=$C_PKG_TMP/cli
mkdir -p "$PROG" "$INC" "$INC2" "$CLI" "$C_PKG_TMP/empty"

cat >"$INC/pkgonly.h" <<'C'
#define PKG_VALUE (PKG_MACRO + 1)
int pkg_value(void);
C
cat >"$CLI/pkgonly.h" <<'C'
#define PKG_VALUE 100
int pkg_value(void);
C
cat >"$INC2/second.h" <<'C'
#define SECOND_VALUE (SECOND_EXTRA * 10)
C
cat >"$PROG/local.h" <<'C'
#include <pkgonly.h>
#define LOCAL_VALUE PKG_VALUE
C
cat >"$PROG/oracle.c" <<'C'
#include <stdio.h>
#include <pkgonly.h>
int main(void) {
  printf("%d\n", PKG_VALUE);
  return 0;
}
C
cat >"$PROG/oracle_order.c" <<'C'
#include <stdio.h>
#include <pkgonly.h>
#include <second.h>
int main(void) {
  printf("%d\n", PKG_VALUE + SECOND_VALUE);
  return 0;
}
C
cat >"$PROG/oracle_local.c" <<'C'
#include <stdio.h>
#include "local.h"
int main(void) {
  printf("%d\n", LOCAL_VALUE);
  return 0;
}
C

cat >"$PROG/oracle_two.c" <<'C'
#include <stdio.h>
#include <pkgonly.h>
#include "local.h"
int main(void) {
  printf("%d\n", PKG_VALUE + LOCAL_VALUE);
  return 0;
}
C

cat >"$C_PKG_TMP/fake-pkg-config" <<'SH'
#!/bin/sh
case "$1" in
  --list-all)
    printf '%s\n' 'sdl2 SDL2 - Simple DirectMedia Layer' 'sdl SDL 1.2' 'zlib zlib'
    ;;
  --cflags)
    printf '%s\n' "$2" >>"$FAKE_PKG_LOG"
    case "$2" in
      good) printf '%s\n' "-I$FAKE_INC -DPKG_MACRO=42 -pthread" ;;
      second) printf '%s\n' "-I $FAKE_INC -isystem $FAKE_INC2 -U NOT_A_MACRO -DSECOND_EXTRA=2 -pthread" ;;
      bare) printf '\n' ;;
      weird) printf '%s\n' "-I$FAKE_INC -Wl,-rpath,/x" ;;
      smoke|sep) printf '%s\n' "-I$SMOKE_INC" ;;
      linkbad|sdl5) printf '%s\n' "-I$FAKE_INC" ;;
      *)
        printf '\nPackage %s was not found in the pkg-config search path.\nPackage search failed\n' "$2" >&2
        exit 1
        ;;
    esac
    ;;
  --libs)
    printf '%s\n' "$2" >>"$FAKE_LIBS_LOG"
    case "$2" in
      good|second) printf '%s\n' "-pthread" ;;
      smoke) printf '%s\n' "-L$SMOKE_LIB -lsmoke -pthread -Wl,-rpath,$SMOKE_LIB" ;;
      sep) printf '%s\n' "-L $SMOKE_LIB -l smoke" ;;
      bare) printf '\n' ;;
      linkbad) printf '%s\n' "-pthread -fopenmp" ;;
      *)
        printf '\nPackage %s was not found in the pkg-config search path.\nPackage search failed\n' "$2" >&2
        exit 1
        ;;
    esac
    ;;
  *) exit 2 ;;
esac
SH
chmod +x "$C_PKG_TMP/fake-pkg-config"
FAKE_INC=$INC
FAKE_INC2=$INC2
FAKE_PKG_LOG=$C_PKG_TMP/pkg.log
FAKE_LIBS_LOG=$C_PKG_TMP/libs.log
export FAKE_INC FAKE_INC2 FAKE_PKG_LOG FAKE_LIBS_LOG
FAS_PKG_CONFIG=$C_PKG_TMP/fake-pkg-config
export FAS_PKG_CONFIG

cat >"$C_PKG_TMP/log-clang" <<'SH'
#!/bin/sh
case " $* " in
  *" -ast-dump=json "*) printf '%s\n' "$*" >>"$CLANG_LOG" ;;
esac
exec "$REAL_CC" "$@"
SH
chmod +x "$C_PKG_TMP/log-clang"
REAL_CC=$(command -v "$CC") || fail "$CC not found"
CLANG_LOG=$C_PKG_TMP/clang.log
export REAL_CC CLANG_LOG
SMOKE=$C_PKG_TMP/smoke
SMOKE_LIB=$SMOKE/lib
SMOKE_INC=$SMOKE/inc
export SMOKE_LIB SMOKE_INC
mkdir -p "$SMOKE_LIB" "$SMOKE_INC"
cat >"$SMOKE/smoke.c" <<'C'
int smoke_value(void) { return 42; }
C
cat >"$SMOKE_INC/smoke.h" <<'C'
int smoke_value(void);
C
"$CC" -std=c17 -Werror -c "$SMOKE/smoke.c" -o "$SMOKE/smoke.o" || fail "smoke library did not compile"
ar rcs "$SMOKE_LIB/libsmoke.a" "$SMOKE/smoke.o" || fail "smoke archive was not built"

cat >"$PROG/value.fas" <<'FAS'
use pkg "good"
use "C" <pkgonly.h>
fn main() i32 { return PKG_VALUE }
FAS
cat >"$PROG/pkg-below.fas" <<'FAS'
use "C" <pkgonly.h>
use pkg "good"
fn main() i32 { return PKG_VALUE }
FAS
cat >"$PROG/two-headers.fas" <<'FAS'
use pkg "good"
use "C" <pkgonly.h>
use "C" "local.h"
fn main() i32 { return PKG_VALUE + LOCAL_VALUE }
FAS
cat >"$PROG/only-pkg.fas" <<'FAS'
use pkg "good"
fn main() i32 { return 0 }
FAS
cat >"$PROG/only-nope.fas" <<'FAS'
use pkg "nope"
fn main() i32 { return 0 }
FAS
cat >"$PROG/plain.fas" <<'FAS'
use "C" <pkgonly.h>
fn main() i32 { return PKG_VALUE }
FAS
cat >"$PROG/order.fas" <<'FAS'
use pkg "good"
use "C" <pkgonly.h>
use pkg "second"
use "C" <second.h>
fn main() i32 { return PKG_VALUE + SECOND_VALUE }
FAS
cat >"$PROG/quoted.fas" <<'FAS'
use pkg "good"
use "C" "local.h"
fn main() i32 { return LOCAL_VALUE }
FAS
cat >"$PROG/sdl3.fas" <<'FAS'
use pkg "sdl3"
use "C" <pkgonly.h>
fn main() i32 { return 0 }
FAS
cat >"$PROG/nope.fas" <<'FAS'
use pkg "nope"
use "C" <pkgonly.h>
fn main() i32 { return 0 }
FAS
cat >"$PROG/sdl.fas" <<'FAS'
use pkg "sdl"
use "C" <pkgonly.h>
fn main() i32 { return 0 }
FAS
cat >"$PROG/zlib.fas" <<'FAS'
use pkg "zlib"
use "C" <pkgonly.h>
fn main() i32 { return 0 }
FAS
cat >"$PROG/zlibb.fas" <<'FAS'
use pkg "zlibb"
use "C" <pkgonly.h>
fn main() i32 { return 0 }
FAS
cat >"$PROG/weird.fas" <<'FAS'
use pkg "weird"
use "C" <pkgonly.h>
fn main() i32 { return 0 }
FAS
cat >"$PROG/missing.fas" <<'FAS'
use pkg "good"
use "C" <missing.h>
fn main() i32 { return 0 }
FAS
cat >"$PROG/bare.fas" <<'FAS'
use pkg "bare"
use "C" <missing.h>
fn main() i32 { return 0 }
FAS
cat >"$PROG/inline.fas" <<'FAS'
use "C" pkg "good" <pkgonly.h>
fn main() i32 { return 0 }
FAS

cd "$PROG"

value=$(oracle oracle.c -I "$INC" -DPKG_MACRO=42)
[ -n "$value" ] || fail "C oracle printed nothing"
run_case value-pkg "$value" value.fas

cli_first=$(oracle oracle.c -I "$CLI" -I "$INC" -DPKG_MACRO=42)
run_case cli-include "$cli_first" -I "$CLI" value.fas

cli_define=$(oracle oracle.c -I "$INC" -DPKG_MACRO=7)
run_case cli-define "$cli_define" -D PKG_MACRO=7 value.fas

run_case pkg-below "$value" pkg-below.fas

two_headers=$(oracle oracle_two.c -I "$INC" -DPKG_MACRO=42)
run_case two-headers "$two_headers" two-headers.fas

order=$(oracle oracle_order.c -I "$INC" -isystem "$INC2" -DPKG_MACRO=42 -DSECOND_EXTRA=2)
run_case order "$order" order.fas

quoted=$(oracle oracle_local.c -I "$INC" -DPKG_MACRO=42)
run_case quoted "$quoted" quoted.fas

run_case only-pkg 0 only-pkg.fas
: >"$FAKE_PKG_LOG"
run_diag 0 "$OCAML_FAS" check only-pkg.fas
grep -Fxq -- good "$FAKE_PKG_LOG" || fail "a file with only use pkg did not run pkg-config --cflags"

run_diag 1 "$OCAML_FAS" check only-nope.fas
expect_lines 'only-nope.fas:1:9: error: pkg-config exited with status 1: Package nope was not found in the pkg-config search path.'

run_diag 1 "$OCAML_FAS" check plain.fas
grep -Fq -- 'pkgonly.h' "$C_PKG_TMP/diag.out" || fail "plain header without pkg flags was not reported"

run_diag 0 "$OCAML_FAS" check value.fas

: >"$CLANG_LOG"
run_diag 0 env FAS_CC="$C_PKG_TMP/log-clang" "$OCAML_FAS" check -I "$CLI" -D PKG_MACRO=7 value.fas
grep -Fq -- "-DPKG_MACRO=42 -I $CLI -D PKG_MACRO=7 -I$INC -pthread" "$CLANG_LOG" \
  || fail "clang argv is not pkg -D, then command-line flags, then pkg -I and -pthread"

: >"$CLANG_LOG"
run_diag 0 env FAS_CC="$C_PKG_TMP/log-clang" "$OCAML_FAS" check -I "$INC" value.fas
grep -Fq -- "-DPKG_MACRO=42 -I $INC -pthread" "$CLANG_LOG" \
  || fail "command-line -I did not replace the matching pkg -I"
! grep -Fq -- "-I$INC" "$CLANG_LOG" || fail "duplicate pkg -I was passed"

: >"$CLANG_LOG"
: >"$FAKE_PKG_LOG"
run_diag 0 env FAS_CC="$C_PKG_TMP/log-clang" "$OCAML_FAS" check order.fas
[ "$(cat "$FAKE_PKG_LOG")" = "$(printf 'good\nsecond')" ] \
  || fail "pkg lines were not run in source order"
awk -v want="-I$INC" '{ n = 0; for (i = 1; i <= NF; i++) if ($i == want) n++; if (n != 1) bad = 1 } END { exit (bad || NR == 0) }' "$CLANG_LOG" \
  || fail "pkg include flag was not passed exactly once per clang run"
awk '{ n = 0; for (i = 1; i <= NF; i++) if ($i == "-pthread") n++; if (n != 1) bad = 1 } END { exit (bad || NR == 0) }' "$CLANG_LOG" \
  || fail "pkg -pthread was not passed exactly once per clang run"
grep -Fq -- "-UNOT_A_MACRO" "$CLANG_LOG" || fail "pkg -U was not passed"

run_diag 1 env FAS_PKG_CONFIG=missing-pc "$OCAML_FAS" check value.fas
expect_lines 'value.fas:1:9: error: `missing-pc` not found; set FAS_PKG_CONFIG or install pkg-config'

run_diag 1 env -i PATH="$C_PKG_TMP/empty" "$OCAML_FAS" check value.fas
expect_lines 'value.fas:1:9: error: `pkg-config` not found; set FAS_PKG_CONFIG or install pkg-config'

run_diag 1 "$OCAML_FAS" check inline.fas
expect_lines 'inline.fas:1:9: error: `pkg` goes on its own line: `use pkg "MODULES"`'

run_diag 1 "$OCAML_FAS" check sdl3.fas
expect_lines 'sdl3.fas:1:9: error: pkg-config exited with status 1: Package sdl3 was not found in the pkg-config search path.' \
  'help: installed packages with a similar name: sdl, sdl2'

run_diag 1 "$OCAML_FAS" check nope.fas
expect_lines 'nope.fas:1:9: error: pkg-config exited with status 1: Package nope was not found in the pkg-config search path.'
! grep -Fq 'help: installed' "$C_PKG_TMP/diag.out" || fail "unexpected similar-package help"

run_diag 1 "$OCAML_FAS" check sdl.fas
expect_lines 'sdl.fas:1:9: error: pkg-config exited with status 1: Package sdl was not found in the pkg-config search path.' \
  'help: installed packages with a similar name: sdl2'

run_diag 1 "$OCAML_FAS" check zlib.fas
expect_lines 'zlib.fas:1:9: error: pkg-config exited with status 1: Package zlib was not found in the pkg-config search path.'
! grep -Fq 'help: installed' "$C_PKG_TMP/diag.out" || fail "requested package was suggested for itself"

run_diag 1 "$OCAML_FAS" check zlibb.fas
expect_lines 'zlibb.fas:1:9: error: pkg-config exited with status 1: Package zlibb was not found in the pkg-config search path.' \
  'help: installed packages with a similar name: zlib'

run_diag 1 "$OCAML_FAS" check weird.fas
expect_lines 'weird.fas:1:9: error: pkg-config flag `-Wl,-rpath,/x` from pkg "weird" is not supported' \
  'help: pass the flags you need with -I or -D'

run_diag 1 "$OCAML_FAS" check missing.fas
expect_lines "note: flags from pkg \"good\": -I$INC -DPKG_MACRO=42 -pthread"

run_diag 1 "$OCAML_FAS" check bare.fas
expect_lines 'note: flags from pkg "bare": (none)'

run_diag 0 "$OCAML_FAS" check value.fas --keep
expect_lines "fas: pkg-config: $C_PKG_TMP/fake-pkg-config --cflags \"good\" -> -I$INC -DPKG_MACRO=42 -pthread"

cat >"$PROG/smoke.fas" <<'FAS'
use pkg "smoke"
use "C" <smoke.h>
fn main() i32 { return smoke_value() }
FAS
cat >"$PROG/mixed.fas" <<'FAS'
use pkg "good"
use "C" <pkgonly.h>
use pkg "smoke"
use "C" <smoke.h>
fn main() i32 { return PKG_VALUE + smoke_value() }
FAS
cat >"$PROG/dedupe.fas" <<'FAS'
use pkg "smoke"
use pkg "sep"
use pkg "smoke"
use "C" <smoke.h>
fn main() i32 { return smoke_value() }
FAS
cat >"$PROG/dep-pkg.fas" <<'FAS'
use pkg "smoke"
fn dep_value() i32 { return 0 }
FAS
cat >"$PROG/two-files.fas" <<'FAS'
use "dep-pkg.fas"
use pkg "smoke"
use "C" <smoke.h>
fn main() i32 { return smoke_value() + dep_value() }
FAS
cat >"$PROG/linkbad.fas" <<'FAS'
use pkg "linkbad"
use "C" <pkgonly.h>
fn main() i32 { return 0 }
FAS
cat >"$PROG/sdl5.fas" <<'FAS'
use pkg "sdl5"
use "C" <pkgonly.h>
fn main() i32 { return 0 }
FAS
cat >"$PROG/pc-smoke.fas" <<'FAS'
use pkg "fassmoke"
use "C" <smoke.h>
fn main() i32 { return smoke_value() }
FAS
cat >"$PROG/oracle_smoke.c" <<'C'
#include <stdio.h>
#include <smoke.h>
int main(void) {
  printf("%d\n", smoke_value());
  return 0;
}
C
cat >"$PROG/oracle_mixed.c" <<'C'
#include <stdio.h>
#include <pkgonly.h>
#include <smoke.h>
int main(void) {
  printf("%d\n", PKG_VALUE + smoke_value());
  return 0;
}
C
cat >"$C_PKG_TMP/log-link" <<'SH'
#!/bin/sh
printf '%s\n' "$*" >>"$LINK_LOG"
exec "$REAL_CC" "$@"
SH
chmod +x "$C_PKG_TMP/log-link"
LINK_LOG=$C_PKG_TMP/link.log
export LINK_LOG

smoke_expected=$(link_oracle oracle_smoke.c -I "$SMOKE_INC" "$SMOKE_LIB/libsmoke.a")
[ -n "$smoke_expected" ] || fail "C oracle printed nothing for smoke"
run_case smoke "$smoke_expected" smoke.fas
for level in 0 2; do
  expect_exit "$smoke_expected" "$OCAML_FAS" run "-O$level" smoke.fas
done

mixed_expected=$(link_oracle oracle_mixed.c -I "$INC" -DPKG_MACRO=42 -I "$SMOKE_INC" "$SMOKE_LIB/libsmoke.a" -pthread)
run_case mixed "$mixed_expected" mixed.fas

: >"$FAKE_LIBS_LOG"
run_diag 0 "$OCAML_FAS" -c -o "$C_PKG_TMP/smoke.o" smoke.fas
run_diag 0 "$OCAML_FAS" --emit-llvm smoke.fas
run_diag 0 "$OCAML_FAS" --emit-header -o "$C_PKG_TMP/emitted.h" smoke.fas
run_diag 0 "$OCAML_FAS" check smoke.fas
[ ! -s "$FAKE_LIBS_LOG" ] || fail "--libs ran for an output that does not link an executable"

: >"$FAKE_LIBS_LOG"
run_diag 0 "$OCAML_FAS" -o "$C_PKG_TMP/order-bin" order.fas
[ "$(cat "$FAKE_LIBS_LOG")" = "$(printf 'good\nsecond')" ] \
  || fail "--libs was not called once per pkg in source order"

: >"$FAKE_LIBS_LOG"
run_diag 0 "$OCAML_FAS" -o "$C_PKG_TMP/two-files" two-files.fas
[ "$(cat "$FAKE_LIBS_LOG")" = smoke ] || fail "a pkg named in two files did not run --libs once"
expect_exit "$smoke_expected" "$C_PKG_TMP/two-files"

: >"$FAKE_LIBS_LOG"
: >"$LINK_LOG"
run_diag 0 env FAS_CC="$C_PKG_TMP/log-link" "$OCAML_FAS" -o "$C_PKG_TMP/dedupe" dedupe.fas
[ "$(cat "$FAKE_LIBS_LOG")" = "$(printf 'smoke\nsep')" ] \
  || fail "identical use pkg lines did not share one --libs call"
link_line=$(grep -F -- '-lsmoke' "$LINK_LOG" || true)
[ "$(printf '%s\n' "$link_line" | grep -c .)" -eq 1 ] || fail "expected one link command with -lsmoke"
case " $link_line " in
  *" -L$SMOKE_LIB -lsmoke "*) ;;
  *) fail "pkg -L flag does not precede its -l flag: $link_line" ;;
esac
[ "$(count_word "$link_line" "-L$SMOKE_LIB")" -eq 1 ] || fail "pkg -L flag was passed more than once"
[ "$(count_word "$link_line" -lsmoke)" -eq 1 ] || fail "pkg -l flag was passed more than once"
[ "$(count_word "$link_line" -pthread)" -eq 1 ] || fail "pkg -pthread was passed more than once"
[ "$(count_word "$link_line" "-Wl,-rpath,$SMOKE_LIB")" -eq 1 ] || fail "pkg -Wl,-rpath was not passed once"
expect_exit 42 "$C_PKG_TMP/dedupe"

run_diag 1 "$OCAML_FAS" -o "$C_PKG_TMP/linkbad" linkbad.fas
expect_lines 'linkbad.fas:1:9: error: pkg-config link flag `-fopenmp` from pkg "linkbad" is not supported' \
  'help: pass the flags you need with -l or -L'

run_diag 1 "$OCAML_FAS" -o "$C_PKG_TMP/sdl5" sdl5.fas
expect_lines 'sdl5.fas:1:9: error: pkg-config exited with status 1: Package sdl5 was not found in the pkg-config search path.' \
  'help: installed packages with a similar name: sdl, sdl2'

cat >"$C_PKG_TMP/vanish-pkg-config" <<SH
#!/bin/sh
"$C_PKG_TMP/fake-pkg-config" "\$@" || exit \$?
[ "\$1" = --cflags ] && rm -f "\$0"
exit 0
SH
chmod +x "$C_PKG_TMP/vanish-pkg-config"
run_diag 1 env FAS_PKG_CONFIG="$C_PKG_TMP/vanish-pkg-config" "$OCAML_FAS" -o "$C_PKG_TMP/libs-vanish" value.fas
expect_lines "value.fas:1:9: error: \`$C_PKG_TMP/vanish-pkg-config\` not found; set FAS_PKG_CONFIG or install pkg-config"
[ ! -e "$C_PKG_TMP/libs-vanish" ] || fail "missing --libs tool left an executable"

run_diag 0 "$OCAML_FAS" -o "$C_PKG_TMP/smoke-keep" smoke.fas --keep
expect_lines "fas: pkg-config: $C_PKG_TMP/fake-pkg-config --libs \"smoke\" -> -L$SMOKE_LIB -lsmoke -pthread -Wl,-rpath,$SMOKE_LIB"

PC_DIR=$C_PKG_TMP/pc
mkdir -p "$PC_DIR"
cat >"$PC_DIR/fassmoke.pc" <<PC
libdir=$SMOKE_LIB
includedir=$SMOKE_INC
Name: fassmoke
Description: smoke archive for the pkg test
Version: 1.0
Cflags: -I\${includedir}
Libs: -L\${libdir} -lsmoke -pthread
PC
if command -v pkg-config >/dev/null 2>&1; then
  run_diag 0 env FAS_PKG_CONFIG=pkg-config PKG_CONFIG_PATH="$PC_DIR" \
    "$OCAML_FAS" -o "$C_PKG_TMP/pc-smoke" pc-smoke.fas
  expect_exit "$smoke_expected" "$C_PKG_TMP/pc-smoke"
else
  echo "c_pkg: pkg-config not installed; real pkg-config link case skipped"
fi

if command -v pkg-config >/dev/null 2>&1 && pkg-config --exists sdl; then
  cat >"$PROG/system-sdl.fas" <<'FAS'
use pkg "sdl"
use "C" <SDL.h>
fn main() i32 { return 0 }
FAS
  run_diag 0 env FAS_PKG_CONFIG=pkg-config "$OCAML_FAS" check system-sdl.fas
else
  echo "c_pkg: system sdl not installed; system pkg-config case skipped"
fi

echo "c_pkg: flags, link flags, argv order, diagnostics and notes: ok"
