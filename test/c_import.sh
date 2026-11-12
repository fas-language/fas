#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
C_IMPORT_TMP=$(mktemp -d)
trap 'rm -rf "$C_IMPORT_TMP"' EXIT HUP INT TERM

fail() {
  echo "c_import: $*" >&2
  exit 1
}

"$OCAML_FAS" --emit-llvm "$ROOT/test/c_import/program.fas" >"$C_IMPORT_TMP/program.ll"
"$LLVM_OPT" -passes=verify "$C_IMPORT_TMP/program.ll" -disable-output
ulimit -c 0 || true
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$C_IMPORT_TMP/program.ll" \
    -o "$C_IMPORT_TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$C_IMPORT_TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$C_IMPORT_TMP/fas.O$level" \
    "$ROOT/test/c_import/program.fas" "$ROOT/test/c_import/runtime.c"
  "$CC" -Werror -std=c17 -O"$level" "$ROOT/test/c_import/oracle.c" \
    "$ROOT/test/c_import/runtime.c" -o "$C_IMPORT_TMP/oracle.O$level"
  timeout 30 "$C_IMPORT_TMP/fas.O$level" >"$C_IMPORT_TMP/fas.O$level.out" \
    || fail "Fas -O$level execution failed"
  timeout 30 "$C_IMPORT_TMP/oracle.O$level" >"$C_IMPORT_TMP/oracle.O$level.out" \
    || fail "C oracle -O$level execution failed"
  cmp -s "$C_IMPORT_TMP/fas.O$level.out" "$C_IMPORT_TMP/oracle.O$level.out" \
    || fail "Fas and C oracle outputs differ at -O$level"
done

cat >"$C_IMPORT_TMP/lazy.fas" <<'FAS'
use "C" <string.h>
fn main() i32 { return if strlen(c"") > 0 { 1 } else { 0 } }
FAS
REAL_CC=$(command -v "$CC")
cat >"$C_IMPORT_TMP/count-clang" <<'SH'
#!/bin/sh
case " $* " in
  *" -dD "*|*" -emit-llvm "*) printf '%s\n' "$*" >>"$MACRO_LOG" ;;
esac
exec "$REAL_CC" "$@"
SH
chmod +x "$C_IMPORT_TMP/count-clang"
FAS_CC="$C_IMPORT_TMP/count-clang" REAL_CC="$REAL_CC" MACRO_LOG="$C_IMPORT_TMP/macros.log" \
  "$OCAML_FAS" --emit-ir "$C_IMPORT_TMP/lazy.fas" >"$C_IMPORT_TMP/lazy.ir"
[ -f "$C_IMPORT_TMP/macros.log" ] || fail "header declaration conflict macro scan was not run"
[ "$(wc -l <"$C_IMPORT_TMP/macros.log")" -eq 1 ] \
  || fail "header declaration conflict macro scan did not run exactly once"

echo "c_import: O0/O2 verified and linked through L1: ok"
