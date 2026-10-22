#!/bin/sh
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
START_DIR=$(pwd)
CC=$(printenv CC || printf 'clang-22')
LLVM_OPT=$(printenv LLVM_OPT || printf 'opt-22')
LLVM_LLC=$(printenv LLVM_LLC || printf 'llc-22')
OCAML_FAS=$(printenv OCAML_FAS || printf '%s/_build/default/bin/main.exe' "$ROOT")

resolve_tool_path() {
  case "$1" in
    /*) printf '%s\n' "$1" ;;
    */*) printf '%s/%s\n' "$START_DIR" "$1" ;;
    *)
      resolved=$(command -v "$1" 2>/dev/null || true)
      case "$resolved" in
        /*) printf '%s\n' "$resolved" ;;
        */*) printf '%s/%s\n' "$START_DIR" "$resolved" ;;
        *) printf '%s\n' "$1" ;;
      esac
      ;;
  esac
}

CC=$(resolve_tool_path "$CC")
LLVM_OPT=$(resolve_tool_path "$LLVM_OPT")
LLVM_LLC=$(resolve_tool_path "$LLVM_LLC")
OCAML_FAS=$(resolve_tool_path "$OCAML_FAS")
REAL_CC=$(command -v "$CC")
REAL_OPT=$(command -v "$LLVM_OPT")
REAL_LLC=$(command -v "$LLVM_LLC")
unset FAS_OPT_PASSES
DRIVER_TMP=$(mktemp -d)
WORK=$DRIVER_TMP/work
TMPDIR=$DRIVER_TMP/tmp
mkdir -p "$WORK" "$TMPDIR"
export TMPDIR
trap 'rm -rf "$DRIVER_TMP"' EXIT HUP INT TERM

cat >"$WORK/good.fas" <<'FAS'
fn helper() i32 { return 7 }
fn main() i32 { return helper() - 7 }
FAS
cat >"$WORK/bad.fas" <<'FAS'
fn main() i32 { return missing }
FAS
cat >"$WORK/part-a.fas" <<'FAS'
fn helper() i32 { return 7 }
FAS
cat >"$WORK/part-b.fas" <<'FAS'
fn another() i32 { return 3 }
FAS
cat >"$WORK/link.fas" <<'FAS'
extern "C" {
  fn c_link_probe(value i32) i32
}
fn main() i32 { return c_link_probe(1) - 2 }
FAS
cat >"$WORK/helper.c" <<'C'
#include <math.h>
int c_link_probe(int value) {
  return value + (sin((double)value) > 0.0);
}
C
cat >"$WORK/c-sanitize.fas" <<'FAS'
use "C" <<SANITIZE_C
int fas_sanitize_c(void) { return 5; }
SANITIZE_C
extern "C" { fn fas_sanitize_c() i32 }
fn main() i32 { return fas_sanitize_c() - 5 }
FAS
cat >"$WORK/lifetime-defer.fas" <<'FAS'
extern "C" { fn observe(p addr) void }
fn main() i32 {
  {
    value i32 = 5
    defer { observe(&value) }
    observe(&value)
  }
  return 0
}
FAS
cat >"$WORK/use-main.fas" <<'FAS'
use "part-a.fas"
use "part-b.fas"
fn main() i32 { return helper() + another() - 10 }
FAS
cat >"$WORK/use-main-reversed.fas" <<'FAS'
use "part-b.fas"
use "part-a.fas"
fn main() i32 { return helper() + another() - 10 }
FAS
cat >"$WORK/opt-wrap" <<'SH'
#!/bin/sh
printf 'opt %s\n' "$*" >> "$TOOL_LOG"
exec "$REAL_OPT" "$@"
SH
cat >"$WORK/llc-wrap" <<'SH'
#!/bin/sh
printf 'llc %s\n' "$*" >> "$TOOL_LOG"
exec "$REAL_LLC" "$@"
SH
cat >"$WORK/cc-wrap" <<'SH'
#!/bin/sh
printf 'cc %s\n' "$*" >> "$TOOL_LOG"
exec "$REAL_CC" "$@"
SH
chmod +x "$WORK/opt-wrap" "$WORK/llc-wrap" "$WORK/cc-wrap"
FAS_OPT=$WORK/opt-wrap
FAS_LLC=$WORK/llc-wrap
FAS_CC=$WORK/cc-wrap
TOOL_LOG=$WORK/tools.log
export FAS_OPT FAS_LLC FAS_CC REAL_OPT REAL_LLC REAL_CC TOOL_LOG

fail() {
  echo "driver: $*" >&2
  exit 1
}
expect_failure() {
  status=0
  "$@" >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
  [ "$status" -ne 0 ] || fail "command unexpectedly succeeded: $*"
  [ ! -s "$WORK/stdout" ] || fail "failure wrote to stdout: $*"
  [ -s "$WORK/stderr" ] || fail "failure omitted stderr: $*"
}
temps_empty() {
  [ -z "$(find "$TMPDIR" -mindepth 1 -print -quit)" ] || fail "temporary files remain"
}

"$OCAML_FAS" --help >"$WORK/help" 2>"$WORK/stderr"
[ ! -s "$WORK/stderr" ] || fail "help wrote to stderr"
"$OCAML_FAS" -h >"$WORK/short-help" 2>"$WORK/stderr"
cmp "$WORK/help" "$WORK/short-help" >/dev/null || fail "-h differs from --help"
grep -F -- "--emit-llvm[=optimized]" "$WORK/help" >/dev/null || fail "help omitted the optimized LLVM form"
for flag in -o --emit-header --emit-ir --emit-ir-json --emit-llvm --emit-asm -S --emit-obj -c --keep \
  -O0..-O3 -g --no-inline --sanitize=LIST --profile -I -isystem -D -h --help; do
  grep -F -- "$flag" "$WORK/help" >/dev/null || fail "help omitted $flag"
done
if grep -F -- "--emit-ast" "$WORK/help" >/dev/null; then fail "help advertises removed --emit-ast"; fi
for heading in build output "C inputs" debugging environment; do
  grep -Fx -- "$heading:" "$WORK/help" >/dev/null || fail "help omitted $heading heading"
done
sed -n '/^debugging:$/,/^environment:$/p' "$WORK/help" >"$WORK/help-debugging"
for flag in --emit-ir --emit-ir-json --keep -g --no-inline --sanitize=LIST; do
  grep -F -- "  $flag" "$WORK/help-debugging" >/dev/null || fail "help placed $flag outside debugging"
done
sed -n '/^environment:$/,$p' "$WORK/help" >"$WORK/help-environment"
for name in FAS_OPT FAS_LLC FAS_CC FAS_PROFDATA FAS_OPT_PASSES; do
  grep -F -- "$name" "$WORK/help-environment" >/dev/null || fail "help omitted $name"
done
if grep -E -- 'LLVM_OPT|LLVM_LLC|(^|[^A-Za-z_])CC([^A-Za-z_]|$)' "$WORK/help-environment" >/dev/null; then
  fail "help environment lists a generic tool name"
fi
"$OCAML_FAS" --version >"$WORK/version" 2>"$WORK/stderr" || fail "--version failed"
[ ! -s "$WORK/stderr" ] || fail "--version wrote stderr"
[ "$(wc -l <"$WORK/version")" -eq 9 ] || fail "--version line count changed"
head -n 1 "$WORK/version" | grep -E '^fas 0\.6\.1 \(([0-9a-f]+|unknown)\)$' >/dev/null \
  || fail "--version first line changed"
grep -F -- "opt: $WORK/opt-wrap (" "$WORK/version" >/dev/null || fail "--version omitted the opt in use"
grep -F -- "llc: $WORK/llc-wrap (" "$WORK/version" >/dev/null || fail "--version omitted the llc in use"
grep -F -- "cc: $WORK/cc-wrap (" "$WORK/version" >/dev/null || fail "--version omitted the C compiler in use"
if grep -F -- "not found" "$WORK/version" >/dev/null; then fail "--version could not run the selected tools"; fi
(
  unset FAS_OPT FAS_LLC FAS_CC
  CC=/nonexistent LLVM_OPT=/nonexistent LLVM_LLC=/nonexistent "$OCAML_FAS" --version
) >"$WORK/default-version" 2>"$WORK/stderr" || fail "generic tool names broke --version"
grep -E '^opt: [^ ]*/opt-22 \(' "$WORK/default-version" >/dev/null || fail "--version omitted the default opt"
grep -E '^llc: [^ ]*/llc-22 \(' "$WORK/default-version" >/dev/null || fail "--version omitted the default llc"
grep -E '^cc: [^ ]*/clang-22 \(' "$WORK/default-version" >/dev/null || fail "--version omitted the default C compiler"
grep -E '^perf: [^ ]+ \(' "$WORK/default-version" >/dev/null || fail "--version omitted the perf tool"
grep -E '^profdata: [^ ]*/llvm-profdata-22 \(' "$WORK/default-version" >/dev/null \
  || fail "--version omitted the default profdata"
grep -E '^objdump: [^ ]*/llvm-objdump-22 \(' "$WORK/default-version" >/dev/null || fail "--version omitted the default objdump"
grep -E '^symbolizer: [^ ]*/llvm-symbolizer-22 \(' "$WORK/default-version" >/dev/null || fail "--version omitted the default symbolizer"
grep -E '^mca: [^ ]*/llvm-mca-22 \(' "$WORK/default-version" >/dev/null || fail "--version omitted the default mca"
expect_failure "$OCAML_FAS" --version "$WORK/good.fas"
grep -Fx -- "--version cannot be combined with other arguments" "$WORK/stderr" >/dev/null \
  || fail "--version with a file was not rejected"
expect_failure "$OCAML_FAS" --help --version
grep -Fx -- "--version cannot be combined with other arguments" "$WORK/stderr" >/dev/null \
  || fail "--version with --help was not rejected"
expect_failure "$OCAML_FAS" --sanitize= "$WORK/good.fas"
grep -Fx -- "--sanitize requires a non-empty list" "$WORK/stderr" >/dev/null || fail "empty sanitizer list diagnostic changed"
for name in memory thread foo; do
  expect_failure "$OCAML_FAS" "--sanitize=$name" "$WORK/good.fas"
  grep -Fx -- "unknown sanitizer \`$name\`" "$WORK/stderr" >/dev/null || fail "unknown sanitizer diagnostic changed"
done
expect_failure "$OCAML_FAS" --sanitize=address --sanitize=undefined "$WORK/good.fas"
grep -Fx -- "conflicting --sanitize options" "$WORK/stderr" >/dev/null || fail "conflicting sanitizer diagnostic changed"
"$OCAML_FAS" --sanitize=address,address --sanitize=address --emit-ir "$WORK/good.fas" \
  >"$WORK/sanitized-custom.ir" 2>"$WORK/stderr"
[ ! -s "$WORK/stderr" ] || fail "repeated sanitizer names were rejected"

for level in 0 1 2 3; do
  "$OCAML_FAS" "-O$level" "$WORK/good.fas" -o "$WORK/opt-$level" >"$WORK/stdout" 2>"$WORK/stderr"
  [ ! -s "$WORK/stdout" ] || fail "executable wrote stdout at -O$level"
  [ ! -s "$WORK/stderr" ] || fail "executable wrote stderr at -O$level"
  "$WORK/opt-$level" || fail "executable failed at -O$level"
done
"$OCAML_FAS" "$WORK/use-main.fas" -o "$WORK/use-input" >"$WORK/stdout" 2>"$WORK/stderr"
"$WORK/use-input" || fail "use dependencies did not share a declaration scope"
"$OCAML_FAS" "$WORK/use-main-reversed.fas" -o "$WORK/use-input-reversed" >"$WORK/stdout" 2>"$WORK/stderr"
"$WORK/use-input-reversed" || fail "use declaration order changed resolution"
expect_failure "$OCAML_FAS" "$WORK/part-a.fas" "$WORK/part-b.fas"
grep -Fx 'multiple input files are not supported; use "path.fas" for dependencies' \
  "$WORK/stderr" >/dev/null || fail "multiple input diagnostic changed"
for mode in -c -S --emit-ir --emit-ir-json --emit-llvm; do
  expect_failure "$OCAML_FAS" "$WORK/link.fas" "$WORK/helper.c" "$mode"
  grep -Fx 'C inputs and link flags require an executable output' \
    "$WORK/stderr" >/dev/null || fail "$mode accepted C link inputs"
done
for mode in -c -S --emit-ir --emit-ir-json --emit-llvm --emit-header; do
  "$OCAML_FAS" "$mode" "$WORK/good.fas" -o /dev/null >"$WORK/stdout" 2>"$WORK/stderr" \
    || fail "$mode could not write to a non-regular output"
  [ ! -s "$WORK/stdout" ] || fail "$mode wrote stdout with an explicit output"
  [ ! -s "$WORK/stderr" ] || fail "$mode wrote stderr with a valid input"
done
"$OCAML_FAS" "$WORK/good.fas" -o /dev/null >"$WORK/stdout" 2>"$WORK/stderr" \
  || fail "executable could not write to a non-regular output"
[ ! -s "$WORK/stdout" ] || fail "executable wrote stdout with an explicit output"
[ ! -s "$WORK/stderr" ] || fail "executable wrote stderr with a valid input"
for level in 0 1 2 3; do
  grep -F -- "default<O$level>" "$TOOL_LOG" >/dev/null || fail "-O$level opt pipeline missing"
  grep -F -- "llc -O$level -relocation-model=pic" "$TOOL_LOG" >/dev/null || fail "-O$level llc PIC model missing"
done
if grep -F -- "-no-pie" "$TOOL_LOG" >/dev/null; then fail "executable link disabled PIE"; fi
grep -F -- "-passes=verify" "$TOOL_LOG" >/dev/null || fail "LLVM verification missing"
[ "$(grep -c -- "-passes=verify" "$TOOL_LOG")" -ge 8 ] || fail "LLVM was not verified before and after optimization"
if grep -F -- "-verify-each" "$TOOL_LOG" >/dev/null; then
  fail "pass-by-pass verification is enabled"
fi
grep -F -- "cc " "$TOOL_LOG" >/dev/null || fail "CC override was ignored"
temps_empty

cat >"$WORK/c-import.fas" <<FAS
use "C" "$ROOT/test/c_import/macros.h"
fn main() i32 { return FAS_MACRO_ALIAS - 41 }
FAS
cat >"$WORK/import-cc" <<'SH'
#!/bin/sh
case " $* " in
  *" -ast-dump=json "*|*" -dD "*|*" -emit-llvm "*) printf '%s\n' "$*" >>"$IMPORT_LOG" ;;
esac
exec "$REAL_CC" "$@"
SH
chmod +x "$WORK/import-cc"
FAS_CC="$WORK/import-cc" IMPORT_LOG="$WORK/import-clang.log" \
  "$OCAML_FAS" --keep --emit-ir "$WORK/c-import.fas" \
  >"$WORK/c-import.ir" 2>"$WORK/c-import.stderr"
[ "$(wc -l <"$WORK/import-clang.log")" -eq 4 ] \
  || fail "C import used more than four Clang commands"
[ "$(grep -c -- '-ast-dump=json' "$WORK/import-clang.log")" -eq 2 ] \
  || fail "C import did not share its AST dumps"
[ "$(grep -c -- '-dD' "$WORK/import-clang.log")" -eq 1 ] \
  || fail "C import macro scan count changed"
[ "$(grep -c -- '-emit-llvm' "$WORK/import-clang.log")" -eq 1 ] \
  || fail "C import probe translation unit was not shared"
[ "$(grep -c '^fas: kept ' "$WORK/c-import.stderr")" -eq 2 ] \
  || fail "C import --keep listing changed"
grep -E "^fas: kept C import unit: $TMPDIR/fas-c-unit-[^ /]+/fas-c-import-[^ /]+\\.c$" \
  "$WORK/c-import.stderr" >/dev/null || fail "--keep omitted the C import unit"
grep -E "^fas: kept C bindings: $TMPDIR/c-import.bindings-[^ ]+\\.txt$" \
  "$WORK/c-import.stderr" >/dev/null || fail "--keep C import listing changed"
find "$TMPDIR" -mindepth 1 -delete
temps_empty

: >"$TOOL_LOG"
"$OCAML_FAS" -g --keep "$WORK/link.fas" "$WORK/helper.c" -lm -o "$WORK/link" \
  >"$WORK/stdout" 2>"$WORK/stderr"
[ ! -s "$WORK/stdout" ] || fail "C link wrote to stdout"
"$WORK/link" || fail "Fas C and libm link failed"
grep -E "^cc [^ ]*fas-module-[^ ]*\\.s $WORK/helper.c -lm -o $WORK/\\.fas-output-[^ ]+\\.tmp$" \
  "$TOOL_LOG" >/dev/null || fail "tool log omitted the exact CC argument order"
grep -E "^fas: CC command: $FAS_CC [^ ]*fas-module-[^ ]*\\.s $WORK/helper.c -lm -o $WORK/\\.fas-output-[^ ]+\\.tmp$" \
  "$WORK/stderr" >/dev/null || fail "debug log omitted the exact CC command"
asm_path=$(sed -n 's/^fas: CC command: [^ ]* \([^ ]*fas-module-[^ ]*\.s\) .*/\1/p' "$WORK/stderr")
[ -s "$asm_path" ] || fail "--keep did not retain the exact assembly input"

: >"$TOOL_LOG"
"$OCAML_FAS" --sanitize=address --keep "$WORK/c-sanitize.fas" -o "$WORK/c-sanitize" \
  >"$WORK/stdout" 2>"$WORK/stderr"
grep -E '^cc --target=x86_64-unknown-linux-gnu -fPIC -O2 -Xclang -main-file-name -Xclang fas-c-import-[0-9a-f]{32}\.c -ffile-prefix-map=[^ ]+=fas-c-import-[0-9a-f]{32}\.c -fsanitize=address -c .*\.c -o ' \
  "$TOOL_LOG" >/dev/null || fail "generated C input was not compiled with ASan"
grep -E '^cc [^ ]*fas-module-[^ ]*\.s .* -fsanitize=address (-lm )?-o ' "$TOOL_LOG" >/dev/null \
  || fail "generated C program link omitted ASan runtime"
: >"$TOOL_LOG"
"$OCAML_FAS" --sanitize=address --keep "$WORK/link.fas" "$WORK/helper.c" -lm \
  -o "$WORK/address-c-link" >"$WORK/stdout" 2>"$WORK/stderr"
grep -F -- "-fsanitize=address $WORK/helper.c -lm -o " "$TOOL_LOG" >/dev/null \
  || fail "C input link omitted ASan runtime"
"$OCAML_FAS" --emit-llvm "$WORK/good.fas" >"$WORK/no-sanitizer.ll" 2>"$WORK/stderr"
"$OCAML_FAS" --sanitize=undefined --emit-llvm "$WORK/good.fas" \
  >"$WORK/undefined.ll" 2>"$WORK/stderr"
cmp "$WORK/no-sanitizer.ll" "$WORK/undefined.ll" >/dev/null \
  || fail "undefined sanitizer changed Fas LLVM"
: >"$TOOL_LOG"
"$OCAML_FAS" --sanitize=undefined "$WORK/c-sanitize.fas" -o "$WORK/undefined-c-link" \
  >"$WORK/stdout" 2>"$WORK/stderr"
grep -E '^cc --target=x86_64-unknown-linux-gnu -fPIC -O2 -Xclang -main-file-name -Xclang fas-c-import-[0-9a-f]{32}\.c -ffile-prefix-map=[^ ]+=fas-c-import-[0-9a-f]{32}\.c -fsanitize=undefined -c .*\.c -o ' \
  "$TOOL_LOG" >/dev/null || fail "generated C input was not compiled with UBSan"
grep -E '^cc [^ ]*fas-module-[^ ]*\.s .* -fsanitize=undefined (-lm )?-o ' "$TOOL_LOG" >/dev/null \
  || fail "C program link omitted UBSan runtime"
: >"$TOOL_LOG"
"$OCAML_FAS" --sanitize=address,undefined "$WORK/c-sanitize.fas" \
  -o "$WORK/combined-c-link" >"$WORK/stdout" 2>"$WORK/stderr"
grep -E '^cc --target=x86_64-unknown-linux-gnu -fPIC -O2 -Xclang -main-file-name -Xclang fas-c-import-[0-9a-f]{32}\.c -ffile-prefix-map=[^ ]+=fas-c-import-[0-9a-f]{32}\.c -fsanitize=address,undefined -c .*\.c -o ' \
  "$TOOL_LOG" >/dev/null || fail "generated C input omitted a combined sanitizer"
grep -E '^cc [^ ]*fas-module-[^ ]*\.s .* -fsanitize=address,undefined (-lm )?-o ' \
  "$TOOL_LOG" >/dev/null || fail "C program link omitted a combined sanitizer"

"$OCAML_FAS" --emit-llvm -O2 "$WORK/good.fas" >"$WORK/raw.ll" 2>"$WORK/stderr"
[ ! -s "$WORK/stderr" ] || fail "LLVM emission wrote diagnostics"
grep -F "define i32 @main" "$WORK/raw.ll" >/dev/null || fail "LLVM emission omitted main"
grep -F "call i32 @helper" "$WORK/raw.ll" >/dev/null || fail "-O2 changed unoptimized LLVM emission"
"$OCAML_FAS" --sanitize=address --emit-llvm "$WORK/link.fas" >"$WORK/address.ll" 2>"$WORK/stderr"
grep -F "define i32 @main() sanitize_address #1 {" "$WORK/address.ll" >/dev/null || fail "address sanitizer missed Fas definition"
grep -F "declare i32 @c_link_probe(i32)" "$WORK/address.ll" >/dev/null || fail "address sanitizer changed external declaration"
if grep -F "declare i32 @c_link_probe(i32) sanitize_address" "$WORK/address.ll" >/dev/null; then
  fail "address sanitizer marked external declaration"
fi
"$OCAML_FAS" --emit-llvm "$WORK/lifetime-defer.fas" >"$WORK/lifetime-plain.ll" 2>"$WORK/stderr"
if grep -F "llvm.lifetime." "$WORK/lifetime-plain.ll" >/dev/null; then
  fail "lifetime markers appeared without address sanitizer"
fi
"$OCAML_FAS" --sanitize=address --emit-llvm "$WORK/lifetime-defer.fas" \
  >"$WORK/lifetime-address.ll" 2>"$WORK/stderr"
start_line=$(grep -n -F "call void @llvm.lifetime.start.p0" "$WORK/lifetime-address.ll" | cut -d: -f1)
end_line=$(grep -n -F "call void @llvm.lifetime.end.p0" "$WORK/lifetime-address.ll" | cut -d: -f1)
defer_line=$(grep -n -F "call void @observe" "$WORK/lifetime-address.ll" | tail -n 1 | cut -d: -f1)
[ -n "$start_line" ] && [ -n "$end_line" ] && [ -n "$defer_line" ] \
  || fail "address sanitizer omitted a local lifetime marker or deferred call"
[ "$start_line" -lt "$defer_line" ] && [ "$defer_line" -lt "$end_line" ] \
  || fail "local lifetime did not start before use and end after defer"
"$OCAML_FAS" --emit-ir "$WORK/good.fas" >"$WORK/custom.ir" 2>"$WORK/stderr"
[ ! -s "$WORK/stderr" ] || fail "custom IR emission wrote diagnostics"
grep -F "Module {" "$WORK/custom.ir" >/dev/null || fail "custom IR emission omitted module"
"$OCAML_FAS" --emit-llvm -g "$WORK/good.fas" >"$WORK/debug.ll" 2>"$WORK/stderr"
grep -F '!DICompileUnit' "$WORK/debug.ll" >/dev/null || fail "-g emitted no compile unit"
grep -F '!DILocation' "$WORK/debug.ll" >/dev/null || fail "-g emitted no line locations"
"$OCAML_FAS" --emit-llvm "$WORK/good.fas" >"$WORK/plain.ll" 2>"$WORK/stderr"
if grep -E 'llvm\.dbg|!DI[A-Za-z]+' "$WORK/plain.ll" >/dev/null; then
  fail "plain --emit-llvm emitted DWARF metadata"
fi

"$OCAML_FAS" -S "$WORK/good.fas" -o "$WORK/good.s" >"$WORK/stdout" 2>"$WORK/stderr"
[ -s "$WORK/good.s" ] || fail "-S did not produce assembly"
[ ! -s "$WORK/stdout" ] || fail "-S wrote to stdout"
"$OCAML_FAS" --emit-asm "$WORK/good.fas" -o "$WORK/alias.s" >"$WORK/stdout" 2>"$WORK/stderr"
[ -s "$WORK/alias.s" ] || fail "--emit-asm did not produce assembly"
"$OCAML_FAS" -c "$WORK/good.fas" -o "$WORK/good.o" >"$WORK/stdout" 2>"$WORK/stderr"
[ -s "$WORK/good.o" ] || fail "-c did not produce an object"
[ "$(od -An -tx1 -N4 "$WORK/good.o" | tr -d ' \n')" = "7f454c46" ] || fail "-c output is not ELF"
: >"$TOOL_LOG"
"$OCAML_FAS" --sanitize=address -c "$WORK/good.fas" -o "$WORK/address.o" >"$WORK/stdout" 2>"$WORK/stderr"
[ -s "$WORK/address.o" ] || fail "sanitized -c did not produce an object"
nm -u "$WORK/address.o" >"$WORK/address-symbols"
grep -F "__asan_" "$WORK/address-symbols" >/dev/null || fail "sanitized object omitted ASan runtime references"
if grep -F "cc " "$TOOL_LOG" >/dev/null; then fail "sanitized -c linked an executable"; fi
"$OCAML_FAS" --emit-obj "$WORK/good.fas" -o "$WORK/alias.o" >"$WORK/stdout" 2>"$WORK/stderr"
[ -s "$WORK/alias.o" ] || fail "--emit-obj did not produce an object"
cp "$WORK/good.fas" "$WORK/auto.fas"
(cd "$WORK" && "$OCAML_FAS" auto.fas >stdout 2>stderr)
[ -x "$WORK/a.out" ] || fail "default executable output path was not used"
[ ! -s "$WORK/stdout" ] && [ ! -s "$WORK/stderr" ] || fail "default executable wrote diagnostics"
"$WORK/a.out" || fail "default executable output failed"
(cd "$WORK" && "$OCAML_FAS" -S auto.fas >stdout 2>stderr)
[ -s "$WORK/auto.s" ] || fail "default assembly output path was not used"
[ ! -s "$WORK/stdout" ] && [ ! -s "$WORK/stderr" ] || fail "default assembly wrote diagnostics"
(cd "$WORK" && "$OCAML_FAS" -c auto.fas >stdout 2>stderr)
[ -s "$WORK/auto.o" ] || fail "default object output path was not used"
[ ! -s "$WORK/stdout" ] && [ ! -s "$WORK/stderr" ] || fail "default object wrote diagnostics"

: >"$TOOL_LOG"
"$OCAML_FAS" -g --keep "$WORK/good.fas" -o "$WORK/debug" >"$WORK/stdout" 2>"$WORK/stderr"
[ ! -s "$WORK/stdout" ] || fail "--keep wrote to stdout"
grep -F "fas: kept intermediates:" "$WORK/stderr" >/dev/null || fail "--keep omitted paths"
grep -F "FAS_OPT=$FAS_OPT" "$WORK/stderr" >/dev/null || fail "--keep omitted tools"
grep -F "passes=default<O2>" "$WORK/stderr" >/dev/null || fail "-g changed the default optimization level"
grep -F "llc -O2" "$WORK/stderr" >/dev/null || fail "-g llc level missing"
grep -F -- "llc -O2 -relocation-model=pic" "$TOOL_LOG" >/dev/null || fail "-g llc PIC model missing"
[ -n "$(find "$TMPDIR" -mindepth 1 -print -quit)" ] || fail "--keep did not retain intermediates"
"$OCAML_FAS" -g -O3 --keep "$WORK/good.fas" -o "$WORK/debug-o3" >"$WORK/stdout" 2>"$WORK/stderr"
grep -F "passes=default<O3>" "$WORK/stderr" >/dev/null || fail "-g overrode explicit -O3"
find "$TMPDIR" -mindepth 1 -delete
temps_empty
FAS_OPT_PASSES='default<O1>' "$OCAML_FAS" --keep "$WORK/good.fas" -o "$WORK/custom-passes" >"$WORK/stdout" 2>"$WORK/stderr"
grep -F "passes=default<O1>" "$WORK/stderr" >/dev/null || fail "FAS_OPT_PASSES override was not reported"
grep -F -- "-passes=default<O1>" "$TOOL_LOG" >/dev/null || fail "FAS_OPT_PASSES override was ignored"
find "$TMPDIR" -mindepth 1 -delete
temps_empty

: >"$TOOL_LOG"
for level in 0 1 2 3; do
  "$OCAML_FAS" --sanitize=address "-O$level" --keep -S "$WORK/good.fas" \
    -o "$WORK/address-$level.s" >"$WORK/stdout" 2>"$WORK/stderr"
  grep -F -- "-passes=asan,default<O$level>" "$TOOL_LOG" >/dev/null || fail "address pass order missing at -O$level"
  grep -F -- "-asan-use-after-scope" "$TOOL_LOG" >/dev/null || fail "address pass omitted scope instrumentation"
done
FAS_OPT_PASSES='default<O1>' "$OCAML_FAS" --sanitize=address --keep -S "$WORK/good.fas" \
  -o "$WORK/address-custom.s" >"$WORK/stdout" 2>"$WORK/stderr"
grep -F -- "-passes=asan,default<O1>" "$TOOL_LOG" >/dev/null || fail "address pass omitted with FAS_OPT_PASSES"
find "$TMPDIR" -mindepth 1 -delete
temps_empty

(
  unset FAS_OPT FAS_LLC FAS_CC
  CC=/nonexistent LLVM_OPT=/nonexistent LLVM_LLC=/nonexistent \
    "$OCAML_FAS" --keep "$WORK/good.fas" -o "$WORK/default-tools"
) >"$WORK/stdout" 2>"$WORK/stderr" || fail "generic tool names affected a default build"
[ ! -s "$WORK/stdout" ] || fail "default build wrote stdout"
grep -F "tools FAS_OPT=opt-22 FAS_LLC=llc-22 FAS_CC=clang-22 FAS_PROFDATA=llvm-profdata-22;" "$WORK/stderr" >/dev/null \
  || fail "default tools were not selected without FAS_* names"
"$WORK/default-tools" || fail "default build with generic tool names did not run"
find "$TMPDIR" -mindepth 1 -delete
temps_empty
(
  CC=/nonexistent LLVM_OPT=/nonexistent LLVM_LLC=/nonexistent \
    "$OCAML_FAS" --keep "$WORK/good.fas" -o "$WORK/selected-tools"
) >"$WORK/stdout" 2>"$WORK/stderr" || fail "generic tool names affected FAS_* selection"
grep -F "tools FAS_OPT=$WORK/opt-wrap FAS_LLC=$WORK/llc-wrap FAS_CC=$WORK/cc-wrap FAS_PROFDATA=llvm-profdata-22;" "$WORK/stderr" >/dev/null \
  || fail "FAS_* tools were not selected over generic names"
find "$TMPDIR" -mindepth 1 -delete
temps_empty

"$OCAML_FAS" --emit-llvm -g --no-inline helper "$WORK/good.fas" >"$WORK/noinline.ll" 2>"$WORK/stderr"
grep -F "@helper() noinline #0 " "$WORK/noinline.ll" >/dev/null || fail "--no-inline missed selected function"
if grep -F "@main() noinline #0 " "$WORK/noinline.ll" >/dev/null; then fail "--no-inline marked main"; fi
"$OCAML_FAS" --emit-llvm -O2 --no-inline helper "$WORK/good.fas" >"$WORK/noinline-o2.ll" 2>"$WORK/stderr"
grep -F "@helper() noinline #0 #1 {" "$WORK/noinline-o2.ll" >/dev/null || fail "--no-inline without -g missed selected function"
if grep -F "@main() noinline" "$WORK/noinline-o2.ll" >/dev/null; then fail "--no-inline without -g marked main"; fi
cat >"$WORK/noinline.fas" <<'FAS'
fn helper() i32 { return 7 }
fn main() i32 { return helper() + 3 }
FAS
"$OCAML_FAS" -O2 "$WORK/noinline.fas" -o "$WORK/plain-exit" >"$WORK/stdout" 2>"$WORK/stderr" || fail "plain build failed"
"$OCAML_FAS" -O2 --no-inline helper "$WORK/noinline.fas" -o "$WORK/noinline-exit" >"$WORK/stdout" 2>"$WORK/stderr" \
  || fail "--no-inline without -g failed to build"
[ ! -s "$WORK/stdout" ] || fail "--no-inline build wrote stdout"
status=0
"$WORK/plain-exit" || status=$?
[ "$status" -eq 10 ] || fail "plain executable exit code changed"
status=0
"$WORK/noinline-exit" || status=$?
[ "$status" -eq 10 ] || fail "--no-inline executable exit code changed"
cat >"$WORK/noinline-generic.fas" <<'FAS'
fn leaf[S const usize](x usize) usize { return x * S + 1 }
fn pick[S const usize, C const usize](x usize, s usize) usize {
    if C > 1 {
        if s < S + C { return pick[S, C / 2](x, s) }
        return pick[S + C, C / 2](x, s)
    }
    return leaf[S](x)
}
fn main() i32 { return trunc[i32](pick[3, 8](2, 7)) }
FAS
for flags in "-O0" "-O2" "-O2 -g"; do
  "$OCAML_FAS" --emit-llvm $flags --no-inline leaf "$WORK/noinline-generic.fas" >"$WORK/noinline-generic.ll" 2>"$WORK/stderr" \
    || fail "--no-inline on a generic failed ($flags)"
  [ "$(grep -c '^define internal i64 @"leaf\$spec\$.*) noinline' "$WORK/noinline-generic.ll")" -eq 8 ] \
    || fail "--no-inline leaf missed instances ($flags)"
  [ "$(grep -c '^define.* noinline' "$WORK/noinline-generic.ll")" -eq 8 ] || fail "--no-inline leaf marked others ($flags)"
done
"$OCAML_FAS" --emit-llvm --no-inline 'leaf[7]' --no-inline 'pick[3, 8]' "$WORK/noinline-generic.fas" >"$WORK/noinline-generic.ll" 2>"$WORK/stderr" \
  || fail "--no-inline on instances failed"
[ "$(grep -c '^define.* noinline' "$WORK/noinline-generic.ll")" -eq 2 ] || fail "--no-inline instances marked a wrong count"
grep -F '@"leaf$spec$4:1:S=usize:7"(' "$WORK/noinline-generic.ll" | grep -F ' noinline' >/dev/null || fail "--no-inline leaf[7] missed"
grep -F '@"pick$spec$4:1:S=usize:3;1:C=usize:8"(' "$WORK/noinline-generic.ll" | grep -F ' noinline' >/dev/null \
  || fail "--no-inline pick[3, 8] missed"
"$OCAML_FAS" -O2 --no-inline leaf "$WORK/noinline-generic.fas" -o "$WORK/noinline-generic" >"$WORK/stdout" 2>"$WORK/stderr" \
  || fail "--no-inline leaf failed to build"
status=0
"$WORK/noinline-generic" || status=$?
[ "$status" -eq 15 ] || fail "--no-inline leaf changed the exit code"
expect_failure "$OCAML_FAS" --emit-llvm --no-inline 'leaf[4]' "$WORK/noinline-generic.fas"
grep -Fx 'error: --no-inline function `leaf[4]` was not emitted' "$WORK/stderr" >/dev/null || fail "--no-inline missing instance error"
grep -Fx 'help: emitted instances: `leaf[3]`, `leaf[5]`, `leaf[7]`, `leaf[9]`, ...' "$WORK/stderr" >/dev/null \
  || fail "--no-inline missing instance help"
expect_failure "$OCAML_FAS" --emit-llvm=bogus "$WORK/good.fas"
grep -F 'unknown --emit-llvm value `bogus`; accepted value is `optimized`' "$WORK/stderr" >/dev/null \
  || fail "--emit-llvm value error unclear"
for level in 0 2; do
  "$OCAML_FAS" --emit-llvm=optimized --keep "-O$level" "$WORK/good.fas" -o "$WORK/optimized-$level.ll" \
    >"$WORK/stdout" 2>"$WORK/stderr" || fail "optimized LLVM failed at -O$level"
  [ ! -s "$WORK/stdout" ] || fail "optimized LLVM -o wrote stdout at -O$level"
  kept=$(grep -o '[^ ,]*fas-opt-[^ ,]*\.ll' "$WORK/stderr" || true)
  [ -n "$kept" ] || fail "--keep omitted the optimized LLVM file at -O$level"
  [ "$(head -n 1 "$WORK/optimized-$level.ll")" = "; ModuleID = 'fas'" ] \
    || fail "optimized LLVM first line is not the fas module id at -O$level"
  tail -n +2 "$kept" >"$WORK/kept-body-$level.ll"
  tail -n +2 "$WORK/optimized-$level.ll" >"$WORK/optimized-body-$level.ll"
  cmp "$WORK/optimized-body-$level.ll" "$WORK/kept-body-$level.ll" >/dev/null \
    || fail "optimized LLVM differs from kept opt output at -O$level"
  "$REAL_OPT" -passes=verify "$WORK/optimized-$level.ll" -disable-output || fail "optimized LLVM does not verify at -O$level"
  find "$TMPDIR" -mindepth 1 -delete
  temps_empty
  "$OCAML_FAS" --emit-llvm=optimized "-O$level" "$WORK/good.fas" -o "$WORK/optimized-again-$level.ll" \
    >"$WORK/stdout" 2>"$WORK/stderr" || fail "repeated optimized LLVM failed at -O$level"
  cmp "$WORK/optimized-$level.ll" "$WORK/optimized-again-$level.ll" >/dev/null \
    || fail "optimized LLVM differs between runs at -O$level"
  temps_empty
  "$OCAML_FAS" --keep "-O$level" "$WORK/good.fas" -o "$WORK/kept-exe-$level" >"$WORK/stdout" 2>"$WORK/stderr" \
    || fail "--keep executable build failed at -O$level"
  kept=$(grep -o '[^ ,]*fas-opt-[^ ,]*\.ll' "$WORK/stderr" || true)
  [ -n "$kept" ] || fail "--keep omitted the opt file at -O$level"
  tail -n +2 "$kept" >"$WORK/kept-body-$level.ll"
  tail -n +2 "$WORK/optimized-$level.ll" >"$WORK/optimized-body-$level.ll"
  cmp "$WORK/optimized-body-$level.ll" "$WORK/kept-body-$level.ll" >/dev/null \
    || fail "optimized LLVM body differs from a separate --keep build at -O$level"
  find "$TMPDIR" -mindepth 1 -delete
  temps_empty
done
FAS_OPT_PASSES='default<O1>' "$OCAML_FAS" --emit-llvm=optimized --keep -O2 "$WORK/good.fas" \
  -o "$WORK/optimized-custom.ll" >"$WORK/stdout" 2>"$WORK/stderr" || fail "optimized LLVM ignored FAS_OPT_PASSES"
grep -F "opt passes=default<O1>" "$WORK/stderr" >/dev/null || fail "optimized LLVM did not report FAS_OPT_PASSES"
kept=$(grep -o '[^ ,]*fas-opt-[^ ,]*\.ll' "$WORK/stderr" || true)
[ "$(head -n 1 "$WORK/optimized-custom.ll")" = "; ModuleID = 'fas'" ] \
  || fail "FAS_OPT_PASSES optimized LLVM first line is not the fas module id"
tail -n +2 "$kept" >"$WORK/kept-custom-body.ll"
tail -n +2 "$WORK/optimized-custom.ll" >"$WORK/optimized-custom-body.ll"
cmp "$WORK/optimized-custom-body.ll" "$WORK/kept-custom-body.ll" >/dev/null \
  || fail "FAS_OPT_PASSES optimized LLVM differs from kept opt output"
find "$TMPDIR" -mindepth 1 -delete
temps_empty
"$OCAML_FAS" --emit-llvm --keep "$WORK/good.fas" -o "$WORK/keep-llvm.ll" >"$WORK/stdout" 2>"$WORK/stderr"
grep -F "fas: tool FAS_OPT=$FAS_OPT; opt passes=verify" "$WORK/stderr" >/dev/null \
  || fail "--emit-llvm --keep omitted the verifier tool label"
find "$TMPDIR" -mindepth 1 -delete
temps_empty
cp "$WORK/good.fas" "$WORK/same-path.fas"
expect_failure "$OCAML_FAS" "$WORK/same-path.fas" -o "$WORK/same-path.fas"
cmp "$WORK/good.fas" "$WORK/same-path.fas" >/dev/null || fail "input/output collision changed input"

"$OCAML_FAS" -o - "$WORK/good.fas" >"$WORK/stdout" 2>"$WORK/stderr" && fail "-o - was accepted"
[ ! -s "$WORK/stdout" ] || fail "-o - wrote stdout"
grep -F -- "-o - is not supported" "$WORK/stderr" >/dev/null || fail "-o - error unclear"
expect_failure "$OCAML_FAS" -o
grep -F -- "-o requires an output path" "$WORK/stderr" >/dev/null || fail "missing -o argument error unclear"
expect_failure "$OCAML_FAS" --emit-ast "$WORK/good.fas"
grep -F -- "unknown option: --emit-ast" "$WORK/stderr" >/dev/null || fail "removed AST error unclear"

printf 'stale artifact\n' >"$WORK/compile-fail"
expect_failure "$OCAML_FAS" "$WORK/bad.fas" -o "$WORK/compile-fail"
[ ! -e "$WORK/compile-fail" ] || fail "compile failure left stale output"
expect_failure "$OCAML_FAS" --emit-llvm "$WORK/bad.fas" \
  -o "$WORK/missing-output-directory/out.ll"
grep -F 'unknown name `missing`' "$WORK/stderr" >/dev/null \
  || fail "output path hid the compile diagnostic"
if grep -F '.fas-output-' "$WORK/stderr" >/dev/null; then
  fail "compile diagnostic exposed a temporary output path"
fi
[ ! -e "$WORK/missing-output-directory" ] || fail "compile failure created an output directory"
temps_empty

mkdir "$WORK/output-directory"
output_directory_check() {
  rm -f "$TOOL_LOG"
  expect_failure "$@" -o "$WORK/output-directory"
  grep -Fxq -- "error: output path \`$WORK/output-directory\` is a directory" "$WORK/stderr" \
    || fail "directory output error unclear: $*"
  [ ! -e "$TOOL_LOG" ] || fail "a tool ran before the directory output check: $*"
  [ -d "$WORK/output-directory" ] || fail "directory output was removed: $*"
  [ -z "$(find "$WORK/output-directory" -mindepth 1 -print -quit)" ] \
    || fail "directory output received files: $*"
}
output_directory_check "$OCAML_FAS" "$WORK/good.fas"
output_directory_check "$OCAML_FAS" --emit-llvm "$WORK/good.fas"
output_directory_check "$OCAML_FAS" --emit-ir "$WORK/good.fas"
output_directory_check "$OCAML_FAS" --emit-asm "$WORK/good.fas"
output_directory_check "$OCAML_FAS" -c "$WORK/good.fas"
temps_empty

ir_status=0
"$OCAML_FAS" --emit-ir "$WORK/bad.fas" >"$WORK/ir-failure-stdout" \
  2>"$WORK/ir-failure-stderr" || ir_status=$?
json_status=0
"$OCAML_FAS" --emit-ir-json "$WORK/bad.fas" -o /dev/null \
  >"$WORK/json-failure-stdout" 2>"$WORK/json-failure-stderr" || json_status=$?
[ "$ir_status" -eq "$json_status" ] || fail "JSON emission changed rejected-program status"
cmp "$WORK/ir-failure-stdout" "$WORK/json-failure-stdout" >/dev/null \
  || fail "rejected JSON emission wrote output"
cmp "$WORK/ir-failure-stderr" "$WORK/json-failure-stderr" >/dev/null \
  || fail "JSON emission changed rejected-program diagnostics"
temps_empty

regular_status=0
"$OCAML_FAS" "$WORK/bad.fas" -o "$WORK/compile-fail-regular" \
  >"$WORK/regular-stdout" 2>"$WORK/regular-stderr" || regular_status=$?
device_status=0
"$OCAML_FAS" "$WORK/bad.fas" -o /dev/null \
  >"$WORK/device-stdout" 2>"$WORK/device-stderr" || device_status=$?
[ "$regular_status" -eq "$device_status" ] || fail "non-regular output changed compile-failure status"
cmp "$WORK/regular-stdout" "$WORK/device-stdout" >/dev/null \
  || fail "non-regular output changed compile-failure stdout"
cmp "$WORK/regular-stderr" "$WORK/device-stderr" >/dev/null \
  || fail "non-regular output changed compile diagnostics"
[ ! -e "$WORK/compile-fail-regular" ] || fail "regular output remained after compile failure"
temps_empty

cat >"$WORK/fail-cc" <<'SH'
#!/bin/sh
while [ "$#" -gt 0 ]; do
  if [ "$1" = "-o" ]; then
    shift
    printf 'partial artifact\n' > "$1"
    echo 'link failed' >&2
    exit 19
  fi
  shift
done
exit 20
SH
chmod +x "$WORK/fail-cc"
printf 'stale artifact\n' >"$WORK/link-fail"
status=0
FAS_CC=$WORK/fail-cc "$OCAML_FAS" "$WORK/good.fas" -o "$WORK/link-fail" >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
[ "$status" -ne 0 ] || fail "failed link succeeded"
[ ! -s "$WORK/stdout" ] || fail "failed link wrote stdout"
grep -F "link failed" "$WORK/stderr" >/dev/null || fail "link diagnostic missing"
grep -Fxq -- "error: $WORK/fail-cc failed (exit 19): link failed" "$WORK/stderr" \
  || fail "link failure rendered with a fake location"
[ ! -e "$WORK/link-fail" ] || fail "link failure left output"
temps_empty

expect_failure "$OCAML_FAS" "$WORK/missing.fas"
grep -F "cannot read" "$WORK/stderr" >/dev/null || fail "missing input error unclear"
grep -Fxq -- "error: cannot read Fas dependency path \`$WORK/missing.fas\`: No such file or directory" "$WORK/stderr" \
  || fail "missing input rendered with a fake location"
if grep -F '<generated>' "$WORK/stderr" >/dev/null; then
  fail "missing input exposed the synthetic location"
fi
status=0
"$OCAML_FAS" check --diagnostics=json "$WORK/missing.fas" >"$WORK/json-stdout" 2>"$WORK/json-stderr" || status=$?
[ "$status" -ne 0 ] || fail "missing JSON input check succeeded"
grep -Fq -- '{"diagnostics":[{"severity":"error","file":null,"line":null,"column":null,"start_offset":null,"end_offset":null,"message":"cannot read Fas dependency path' "$WORK/json-stderr" \
  || fail "missing input JSON kept a fake location"
expect_failure "$OCAML_FAS" "$WORK"
grep -F "is a directory" "$WORK/stderr" >/dev/null || fail "directory input error unclear"

status=0
FAS_OPT=$WORK/missing-opt "$OCAML_FAS" --emit-llvm -o "$WORK/tool-fail.ll" "$WORK/good.fas" >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
[ "$status" -ne 0 ] || fail "missing FAS_OPT succeeded"
[ ! -s "$WORK/stdout" ] || fail "missing FAS_OPT wrote stdout"
grep -F "LLVM verification" "$WORK/stderr" >/dev/null || fail "missing tool error unclear"
[ ! -e "$WORK/tool-fail.ll" ] || fail "tool failure left output"
find "$TMPDIR" -mindepth 1 -delete
temps_empty

RUN_TMP=$WORK/run-tmp
mkdir -p "$RUN_TMP"
run_temps_empty() {
  [ -z "$(find "$RUN_TMP" -mindepth 1 -print -quit)" ] || fail "fas run left temporary files"
}
cat >"$WORK/run-ret.fas" <<'FAS'
fn main() i32 { return 42 }
FAS
cat >"$WORK/run.fas" <<'FAS'
fn main() i32 { return 0 }
FAS
cat >"$WORK/run-args.fas" <<'FAS'
extern "C" { fn c_dump_args() i32 }
fn main() i32 { return c_dump_args() }
FAS
cat >"$WORK/run-stdin.fas" <<'FAS'
extern "C" { fn c_read_byte() i32 }
fn main() i32 { return c_read_byte() }
FAS
cat >"$WORK/run-say.fas" <<'FAS'
extern "C" { fn c_say() i32 }
fn main() i32 { return c_say() }
FAS
cat >"$WORK/run-abort.fas" <<'FAS'
extern "C" { fn c_abort_now() i32 }
fn main() i32 { return c_abort_now() }
FAS
cat >"$WORK/run-io.c" <<'C'
#include <stdio.h>
#include <stdlib.h>
int c_dump_args(void) {
  const char *path = getenv("FAS_RUN_DUMP");
  FILE *in = fopen("/proc/self/cmdline", "rb");
  FILE *out = path == NULL ? NULL : fopen(path, "wb");
  int c;
  if (in == NULL || out == NULL) return 1;
  while ((c = fgetc(in)) != EOF) fputc(c, out);
  fclose(in);
  fclose(out);
  return 0;
}
int c_read_byte(void) { return getchar(); }
int c_say(void) {
  puts("run-out");
  fputs("run-err\n", stderr);
  return 0;
}
int c_abort_now(void) { abort(); }
C
cat >"$WORK/noexec-cc" <<'SH'
#!/bin/sh
"$REAL_CC" "$@" || exit $?
prev=
for arg in "$@"; do
  if [ "$prev" = "-o" ]; then chmod 0644 "$arg"; fi
  prev=$arg
done
SH
chmod +x "$WORK/noexec-cc"

status=0
env TMPDIR="$RUN_TMP" "$OCAML_FAS" run "$WORK/run-ret.fas" >"$WORK/run-stdout" 2>"$WORK/run-stderr" || status=$?
[ "$status" -eq 42 ] || fail "fas run did not pass exit code 42 through"
[ ! -s "$WORK/run-stdout" ] && [ ! -s "$WORK/run-stderr" ] || fail "fas run wrote output for a silent program"
run_temps_empty

env FAS_RUN_DUMP="$WORK/run-args.dump" TMPDIR="$RUN_TMP" \
  "$OCAML_FAS" run "$WORK/run-args.fas" "$WORK/run-io.c" -- a --version -h -- b \
  >"$WORK/run-stdout" 2>"$WORK/run-stderr" || fail "fas run rejected program arguments"
printf '%s\n' "$WORK/run-args" a --version -h -- b >"$WORK/run-args.expected"
tr '\0' '\n' <"$WORK/run-args.dump" >"$WORK/run-args.actual"
cmp "$WORK/run-args.expected" "$WORK/run-args.actual" >/dev/null \
  || fail "program arguments or argv[0] changed under fas run"
run_temps_empty

status=0
printf 'Z' | env TMPDIR="$RUN_TMP" "$OCAML_FAS" run "$WORK/run-stdin.fas" "$WORK/run-io.c" \
  >"$WORK/run-stdout" 2>"$WORK/run-stderr" || status=$?
[ "$status" -eq 90 ] || fail "fas run did not pass stdin through"
run_temps_empty

env TMPDIR="$RUN_TMP" "$OCAML_FAS" run "$WORK/run-say.fas" "$WORK/run-io.c" \
  >"$WORK/run-stdout" 2>"$WORK/run-stderr" || fail "fas run failed for the output check"
printf 'run-out\n' >"$WORK/run-expected-stdout"
printf 'run-err\n' >"$WORK/run-expected-stderr"
cmp "$WORK/run-expected-stdout" "$WORK/run-stdout" >/dev/null || fail "fas run changed program stdout"
cmp "$WORK/run-expected-stderr" "$WORK/run-stderr" >/dev/null || fail "fas run changed program stderr"
run_temps_empty

status=0
env TMPDIR="$RUN_TMP" "$OCAML_FAS" run "$WORK/run-abort.fas" "$WORK/run-io.c" \
  >"$WORK/run-stdout" 2>"$WORK/run-stderr" || status=$?
[ "$status" -eq 134 ] || fail "signal exit did not report 128 plus SIGABRT"
[ ! -s "$WORK/run-stdout" ] && [ ! -s "$WORK/run-stderr" ] || fail "signal exit printed fas output"
run_temps_empty

status=0
env TMPDIR="$RUN_TMP" "$OCAML_FAS" run --keep "$WORK/run-ret.fas" \
  >"$WORK/run-stdout" 2>"$WORK/run-stderr" || status=$?
[ "$status" -eq 42 ] || fail "fas run --keep changed the exit code"
[ -z "$(find "$RUN_TMP" -name 'fas-run-*' -print -quit)" ] || fail "fas run --keep kept the executable"
grep -F "fas: kept intermediates:" "$WORK/run-stderr" >/dev/null || fail "fas run --keep omitted intermediates"
find "$RUN_TMP" -mindepth 1 -delete
run_temps_empty

for flag in -c -S --emit-llvm --emit-llvm=optimized --emit-ir --emit-header; do
  expect_failure env TMPDIR="$RUN_TMP" "$OCAML_FAS" run "$WORK/run-ret.fas" "$flag"
  grep -Fx "fas run builds and runs an executable; \`$flag\` is not accepted" "$WORK/stderr" >/dev/null \
    || fail "fas run accepted $flag"
done
expect_failure env TMPDIR="$RUN_TMP" "$OCAML_FAS" run "$WORK/run-ret.fas" -o "$WORK/run-out-file"
grep -Fx 'fas run builds and runs an executable; `-o` is not accepted' "$WORK/stderr" >/dev/null \
  || fail "fas run accepted -o"
expect_failure env TMPDIR="$RUN_TMP" "$OCAML_FAS" run --version
grep -Fx -- "--version cannot be combined with other arguments" "$WORK/stderr" >/dev/null \
  || fail "fas run --version was not rejected"
expect_failure env TMPDIR="$RUN_TMP" "$OCAML_FAS" run
grep -Fx -- "no input files" "$WORK/stderr" >/dev/null || fail "fas run without a file was not rejected"

status=0
env TMPDIR="$RUN_TMP" "$OCAML_FAS" run "$WORK/bad.fas" >"$WORK/run-stdout" 2>"$WORK/run-stderr" || status=$?
[ "$status" -eq 1 ] || fail "fas run build failure did not exit 1"
grep -F 'unknown name `missing`' "$WORK/run-stderr" >/dev/null || fail "fas run build diagnostic changed"
[ ! -s "$WORK/run-stdout" ] || fail "fas run build failure wrote stdout"
run_temps_empty

env TMPDIR="$RUN_TMP" "$OCAML_FAS" "$WORK/run.fas" -o "$WORK/run-file" >"$WORK/stdout" 2>"$WORK/stderr" \
  || fail "run.fas did not build as a normal input"
"$WORK/run-file" || fail "normal run.fas build did not run"
status=0
env TMPDIR="$RUN_TMP" "$OCAML_FAS" run "$WORK/run.fas" >"$WORK/run-stdout" 2>"$WORK/run-stderr" || status=$?
[ "$status" -eq 0 ] || fail "fas run of run.fas failed"
run_temps_empty
for level in 0 2; do
  status=0
  env TMPDIR="$RUN_TMP" "$OCAML_FAS" run "-O$level" "$WORK/run-ret.fas" \
    >"$WORK/run-stdout" 2>"$WORK/run-stderr" || status=$?
  [ "$status" -eq 42 ] || fail "fas run -O$level did not pass exit code 42 through"
  run_temps_empty
done

status=0
env TMPDIR="$RUN_TMP" FAS_CC="$WORK/noexec-cc" "$OCAML_FAS" run "$WORK/run-ret.fas" \
  >"$WORK/run-stdout" 2>"$WORK/run-stderr" || status=$?
[ "$status" -eq 1 ] || fail "unstartable program did not exit 1"
[ "$(wc -l <"$WORK/run-stderr")" -eq 1 ] || fail "unstartable program printed more than one line"
grep -E '^fas: cannot run [^ ]+: Permission denied$' "$WORK/run-stderr" >/dev/null \
  || fail "unstartable program diagnostic changed"
[ ! -s "$WORK/run-stdout" ] || fail "unstartable program wrote stdout"
run_temps_empty

cat >"$WORK/run-sleep.fas" <<'FAS'
extern "C" { fn c_pid_then_sleep() i32 }
fn main() i32 { return c_pid_then_sleep() }
FAS
cat >"$WORK/run-sleep.c" <<'C'
#include <stdio.h>
#include <unistd.h>
int c_pid_then_sleep(void) {
  char buffer[4096];
  size_t length, index = 0;
  FILE *in = fopen("/proc/self/cmdline", "rb");
  FILE *out;
  if (in == NULL) return 1;
  length = fread(buffer, 1, sizeof buffer - 1, in);
  fclose(in);
  buffer[length] = '\0';
  while (index < length && buffer[index] != '\0') index++;
  if (index + 1 >= length) return 1;
  out = fopen(buffer + index + 1, "w");
  if (out == NULL) return 1;
  fprintf(out, "%ld\n", (long)getpid());
  fclose(out);
  sleep(30);
  return 0;
}
C
fas_exited() {
  [ ! -e "/proc/$1" ] || grep -q '^State:[[:space:]]*Z' "/proc/$1/status" 2>/dev/null
}
stop_run_pin() {
  [ -z "$helper_pid" ] || kill "$helper_pid" 2>/dev/null || true
  if [ -n "$fas_pid" ]; then
    kill "$fas_pid" 2>/dev/null || true
    wait "$fas_pid" 2>/dev/null || true
  fi
}
run_signal_pin() {
  signal=$1
  expected=$2
  pid_file=$WORK/run-sleep-$signal.pid
  fas_pid=
  helper_pid=
  rm -f "$pid_file"
  env TMPDIR="$RUN_TMP" "$OCAML_FAS" run "$WORK/run-sleep.fas" "$WORK/run-sleep.c" -- "$pid_file" \
    >"$WORK/run-stdout" 2>"$WORK/run-stderr" &
  fas_pid=$!
  tries=0
  while [ ! -s "$pid_file" ]; do
    tries=$((tries + 1))
    if [ "$tries" -gt 100 ] || fas_exited "$fas_pid"; then
      stop_run_pin
      fail "fas run did not start the helper before SIG$signal"
    fi
    sleep 0.1
  done
  helper_pid=$(cat "$pid_file")
  case "$helper_pid" in
    '' | *[!0-9]*) stop_run_pin; fail "helper wrote a bad pid before SIG$signal" ;;
  esac
  kill "-$signal" "$fas_pid"
  tries=0
  while ! fas_exited "$fas_pid"; do
    tries=$((tries + 1))
    if [ "$tries" -gt 50 ]; then
      stop_run_pin
      fail "fas run did not exit within 5 seconds of SIG$signal"
    fi
    sleep 0.1
  done
  status=0
  wait "$fas_pid" || status=$?
  fas_pid=
  if kill -0 "$helper_pid" 2>/dev/null; then
    stop_run_pin
    fail "helper outlived fas run after SIG$signal"
  fi
  helper_pid=
  [ "$status" -eq "$expected" ] || fail "fas run exited $status after SIG$signal, expected $expected"
  run_temps_empty
}
run_signal_pin TERM 143
run_signal_pin HUP 129

echo "driver: passed"
