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
export FAS_CC="$CC" FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC"

CHECK_TMP=$(mktemp -d)
WORK=$CHECK_TMP/work
TMPDIR=$CHECK_TMP/tmp
mkdir -p "$WORK" "$TMPDIR"
export TMPDIR
trap 'rm -rf "$CHECK_TMP"' EXIT HUP INT TERM

fail() {
  echo "check_command: $*" >&2
  exit 1
}

ACCENT=$(printf '\303\251')

cat >"$WORK/good.fas" <<'FAS'
fn main() i32 { return 0 }
FAS
cat >"$WORK/bad.fas" <<'FAS'
fn main() i32 { return missing }
FAS
cat >"$WORK/help.fas" <<'FAS'
fn a() f64 { return .5 }
FAS
cat >"$WORK/dup.fas" <<'FAS'
fn a() i32 { return 1 }
fn a() i32 { return 2 }
FAS
cat >"$WORK/inner.fas" <<'FAS'
fn helper() i32 { return nope }
FAS
cat >"$WORK/outer.fas" <<'FAS'
use "inner.fas"
fn main() i32 { return 0 }
FAS
cat >"$WORK/msg.fas" <<'FAS'
fn main() i32 { return "café \"q\" \\ x" }
FAS
printf 'use "C" <<C\nint caf\303\251(void) { return ; }\nC\nfn main() i32 { return 0 }\n' >"$WORK/cuni.fas"
printf 'use "C" <<C\nint answer(void) { return 42; }\nC\nfn main() i32 { return 0 }\n' >"$WORK/cok.fas"
cp "$WORK/bad.fas" "$WORK/caf$ACCENT.fas"

echo "check_command: check and --emit-ir agree on every test/*.fas file"
: >"$WORK/valid.txt"
checked=0
valid=0
for source in "$ROOT"/test/*.fas; do
  [ -f "$source" ] || continue
  name=${source#"$ROOT"/test/}
  checked=$((checked + 1))
  check_status=0
  env FAS_OPT=/nonexistent FAS_LLC=/nonexistent "$OCAML_FAS" check "$source" \
    >"$WORK/check-stdout" 2>"$WORK/check-stderr" || check_status=$?
  [ ! -s "$WORK/check-stdout" ] || fail "check wrote stdout for $name"
  emit_status=0
  env FAS_OPT=/nonexistent FAS_LLC=/nonexistent "$OCAML_FAS" --emit-ir "$source" -o /dev/null \
    >"$WORK/emit-stdout" 2>"$WORK/emit-stderr" || emit_status=$?
  [ ! -s "$WORK/emit-stdout" ] || fail "--emit-ir -o /dev/null wrote stdout for $name"
  [ "$check_status" -eq "$emit_status" ] \
    || fail "check exit $check_status differs from --emit-ir exit $emit_status for $name"
  cmp -s "$WORK/check-stderr" "$WORK/emit-stderr" \
    || fail "check diagnostics differ from --emit-ir for $name"
  if [ "$check_status" -eq 0 ]; then
    valid=$((valid + 1))
    printf '%s\n' "$source" >>"$WORK/valid.txt"
  fi
done
[ "$checked" -gt 0 ] || fail "no test/*.fas files were found"
echo "check_command: $checked files compared, $valid accepted"

needs_cc=0
while IFS= read -r source; do
  status=0
  env FAS_CC=/nonexistent "$OCAML_FAS" check "$source" \
    >"$WORK/cc-stdout" 2>"$WORK/cc-stderr" || status=$?
  [ ! -s "$WORK/cc-stdout" ] || fail "check wrote stdout without a C compiler"
  if [ "$status" -ne 0 ]; then
    grep -F "/nonexistent" "$WORK/cc-stderr" >/dev/null \
      || fail "valid file failed without a C compiler: ${source#"$ROOT"/}"
    needs_cc=$((needs_cc + 1))
  fi
done <"$WORK/valid.txt"
echo "check_command: $needs_cc accepted files need the C compiler"

status=0
env FAS_CC=/nonexistent FAS_OPT=/nonexistent FAS_LLC=/nonexistent \
  "$OCAML_FAS" check "$WORK/good.fas" >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
[ "$status" -eq 0 ] || fail "good file failed without tools"
[ ! -s "$WORK/stdout" ] && [ ! -s "$WORK/stderr" ] || fail "good file check printed output"

CTMP=$CHECK_TMP/ctmp
mkdir -p "$CTMP"
status=0
env TMPDIR="$CTMP" "$OCAML_FAS" check "$WORK/cok.fas" >"$WORK/stdout" 2>"$WORK/stderr" \
  || status=$?
[ "$status" -eq 0 ] || fail "C import check failed"
[ ! -s "$WORK/stdout" ] && [ ! -s "$WORK/stderr" ] || fail "C import check printed output"
[ -z "$(find "$CTMP" -mindepth 1 -print -quit)" ] || fail "check left temporary files"

"$OCAML_FAS" check -O0 -g -I "$WORK" -isystem "$WORK" -D FLAG=1 --no-inline main \
  --sanitize=address "$WORK/good.fas" >"$WORK/stdout" 2>"$WORK/stderr" \
  || fail "check rejected an accepted option"
[ ! -s "$WORK/stdout" ] && [ ! -s "$WORK/stderr" ] || fail "accepted options printed output"
"$OCAML_FAS" check -O3 --sanitize=undefined "$WORK/good.fas" >/dev/null 2>&1 \
  || fail "check rejected -O3 or undefined sanitizer"

expect_usage() {
  expected=$1
  shift
  status=0
  "$OCAML_FAS" "$@" >"$WORK/usage-stdout" 2>"$WORK/usage-stderr" || status=$?
  [ "$status" -eq 2 ] || fail "fas $* exited $status, expected 2"
  [ ! -s "$WORK/usage-stdout" ] || fail "fas $* wrote stdout"
  printf '%s\n' "$expected" >"$WORK/usage-expected"
  cmp -s "$WORK/usage-expected" "$WORK/usage-stderr" \
    || fail "fas $* diagnostic changed: $(cat "$WORK/usage-stderr")"
}
for flag in -o -c -S --emit-llvm --emit-llvm=optimized --emit-ir --emit-ir-json \
  --emit-header --emit-asm --emit-obj -lm -l -L "-L$WORK"; do
  expect_usage "fas check checks one Fas file; \`$flag\` is not accepted" \
    check "$flag" "$WORK/good.fas"
done
expect_usage "fas check checks one Fas file; \`helper.c\` is not accepted" \
  check "$WORK/good.fas" helper.c
expect_usage "fas check checks one Fas file; \`lib.a\` is not accepted" \
  check lib.a "$WORK/good.fas"
expect_usage "fas check checks one Fas file; \`-o\` is not accepted" \
  check --diagnostics=json -o "$WORK/out" "$WORK/good.fas"
expect_usage "no input files" check
expect_usage "unknown --diagnostics value \`xml\`; accepted values are \`text\` and \`json\`" \
  check --diagnostics=xml "$WORK/good.fas"
expect_usage "unknown --diagnostics value \`yaml\`; accepted values are \`text\` and \`json\`" \
  --diagnostics=yaml "$WORK/good.fas"
expect_usage "unknown option: --diagnostics" check --diagnostics "$WORK/good.fas"
expect_usage "--version cannot be combined with other arguments" check --version "$WORK/good.fas"
expect_usage "fas run builds and runs an executable; \`-o\` is not accepted" \
  run -o "$WORK/out" "$WORK/good.fas"

"$OCAML_FAS" --help >"$WORK/help" 2>"$WORK/stderr" || fail "--help failed"
grep -Fx '  check FILE    check without building (no LLVM, no linking)' "$WORK/help" \
  >/dev/null || fail "help omitted the check command"
grep -F -- 'fas check file.fas [options]' "$WORK/help" >/dev/null \
  || fail "help omitted the check synopsis"
grep -F -- '--diagnostics=text|json' "$WORK/help" >/dev/null \
  || fail "help omitted --diagnostics"

"$OCAML_FAS" check --diagnostics=text "$WORK/good.fas" >"$WORK/stdout" 2>"$WORK/stderr" \
  || fail "explicit text diagnostics rejected"
[ ! -s "$WORK/stdout" ] && [ ! -s "$WORK/stderr" ] || fail "text success printed output"

"$OCAML_FAS" check --diagnostics=json "$WORK/good.fas" >"$WORK/stdout" 2>"$WORK/stderr" \
  || fail "json check of a valid file failed"
[ ! -s "$WORK/stdout" ] || fail "json check printed stdout"
printf '%s\n' '{"diagnostics":[]}' >"$WORK/expected-empty"
cmp -s "$WORK/expected-empty" "$WORK/stderr" || fail "json check success line changed"

"$OCAML_FAS" --diagnostics=json --emit-ir "$WORK/good.fas" >"$WORK/stdout" 2>"$WORK/stderr" \
  || fail "json build of a valid file failed"
[ -s "$WORK/stdout" ] && [ ! -s "$WORK/stderr" ] || fail "json build printed extra stderr"
"$OCAML_FAS" run --diagnostics=json "$WORK/good.fas" >"$WORK/stdout" 2>"$WORK/stderr" \
  || fail "json run of a valid file failed"
[ ! -s "$WORK/stderr" ] || fail "json run printed stderr for a valid program"

cat >"$WORK/pair.py" <<'PY'
import json
import sys

def fail(message):
    raise SystemExit("check_command: " + message)

raw = open(sys.argv[1], "rb").read()
text = open(sys.argv[2], "rb").read().decode("utf-8", "surrogateescape")
lines = raw.split(b"\n")
if len(lines) != 2 or lines[1] != b"" or not lines[0]:
    fail("json diagnostics are not exactly one line")
document = json.loads(lines[0].decode("utf-8"))
if list(document) != ["diagnostics"]:
    fail("json document has unexpected keys")
if not document["diagnostics"]:
    fail("error case produced no diagnostics")
keys = ["severity", "file", "line", "column", "start_offset", "end_offset", "message", "notes", "help"]
position = 0
for diagnostic in document["diagnostics"]:
    if list(diagnostic) != keys:
        fail("json diagnostic has unexpected keys")
    severity = diagnostic["severity"]
    message = diagnostic["message"]
    if diagnostic["file"] is None:
        if [diagnostic[key] for key in keys[2:6]] != [None, None, None, None]:
            fail("synthetic diagnostic has numeric fields")
        header = "%s: %s" % (severity, message)
    else:
        header = "%s:%d:%d: %s: %s" % (
            diagnostic["file"],
            diagnostic["line"],
            diagnostic["column"],
            severity,
            message,
        )
        start, end = diagnostic["start_offset"], diagnostic["end_offset"]
        if start or end:
            data = open(diagnostic["file"], "rb").read()
            if not 0 <= start <= end <= len(data):
                fail("offsets outside the file")
            line = data.count(b"\n", 0, start) + 1
            column = start - (data.rfind(b"\n", 0, start) + 1) + 1
            if (line, column) != (diagnostic["line"], diagnostic["column"]):
                fail("offsets disagree with line and column")
    index = text.find(header, position)
    if index < 0:
        fail("text rendering lacks " + repr(header))
    position = index + len(header)
    for note in diagnostic["notes"]:
        if text.find("note: " + note, position) < 0:
            fail("text rendering lacks note " + repr(note))
    if diagnostic["help"] is not None:
        if text.find("help: " + diagnostic["help"], position) < 0:
            fail("text rendering lacks help " + repr(diagnostic["help"]))
PY

pair() {
  label=$1
  file=$2
  shift 2
  text_status=0
  "$OCAML_FAS" "$@" "$file" >"$WORK/pair-text-stdout" 2>"$WORK/pair-text" || text_status=$?
  json_status=0
  "$OCAML_FAS" "$@" --diagnostics=json "$file" >"$WORK/pair-json-stdout" \
    2>"$WORK/pair-json" || json_status=$?
  [ "$text_status" -ne 0 ] || fail "$label: expected a failing case"
  [ "$text_status" -eq "$json_status" ] || fail "$label: exit status changed with json"
  cmp -s "$WORK/pair-text-stdout" "$WORK/pair-json-stdout" \
    || fail "$label: stdout changed with json"
  python3 -I "$WORK/pair.py" "$WORK/pair-json" "$WORK/pair-text" \
    || fail "$label: json diagnostics do not match the text rendering"
}
pair unknown-name "$WORK/bad.fas" check
pair unknown-name-run "$WORK/bad.fas" run
pair unknown-name-build "$WORK/bad.fas" --emit-ir
pair float-help "$WORK/help.fas" check
pair duplicate-note "$WORK/dup.fas" check
pair include-chain-note "$WORK/outer.fas" check
pair quotes-backslashes "$WORK/msg.fas" check
pair c-message-non-ascii "$WORK/cuni.fas" check
pair missing-file "$WORK/missing.fas" check
pair non-ascii-file-name "$WORK/caf$ACCENT.fas" check

status=0
"$OCAML_FAS" check --diagnostics=json "$WORK/missing.fas" >"$WORK/stdout" 2>"$WORK/stderr" \
  || status=$?
[ "$status" -eq 1 ] || fail "missing file check exited $status"
grep -F '"file":null,"line":null,"column":null,"start_offset":null,"end_offset":null' \
  "$WORK/stderr" >/dev/null || fail "synthetic diagnostic fields are not null"

mkdir -p "$WORK/all"
cat >"$WORK/all/lib.fas" <<'FAS'
fn lib_bad(p u64) u8 {
    return p
}
FAS
cat >"$WORK/all/main.fas" <<'FAS'
use "lib.fas"
fn f(a u64) u32 {
    x u32 = a
    return x
}
fn main() i32 {
    return 0
}
FAS
status=0
"$OCAML_FAS" check --all-errors "$WORK/all/main.fas" >"$WORK/stdout" 2>"$WORK/stderr" \
  || status=$?
[ "$status" -eq 1 ] || fail "--all-errors check exited $status"
[ "$(grep -c ': error: ' "$WORK/stderr")" -eq 2 ] || fail "--all-errors did not report both errors"
grep -F '      return p' "$WORK/stderr" >/dev/null || fail "--all-errors lost the use file excerpt"
grep -F '      x u32 = a' "$WORK/stderr" >/dev/null || fail "--all-errors lost the root file excerpt"
status=0
"$OCAML_FAS" check "$WORK/all/main.fas" >"$WORK/stdout" 2>"$WORK/stderr" || status=$?
[ "$status" -eq 1 ] || fail "check without --all-errors exited $status"
[ "$(grep -c ': error: ' "$WORK/stderr")" -eq 1 ] || fail "check without --all-errors reported more than one error"
status=0
"$OCAML_FAS" -O0 --all-errors "$WORK/all/main.fas" -o "$WORK/all/a.out" >"$WORK/stdout" 2>"$WORK/stderr" \
  || status=$?
[ "$status" -eq 1 ] && [ ! -e "$WORK/all/a.out" ] || fail "--all-errors build produced output"
[ "$(grep -c ': error: ' "$WORK/stderr")" -eq 2 ] || fail "--all-errors build did not report both errors"

best_ms() {
  best=
  for _ in 1 2 3; do
    start=$(date +%s%N)
    "$@" >"$WORK/time-stdout" 2>"$WORK/time-stderr" || fail "timed command failed: $*"
    stop=$(date +%s%N)
    elapsed=$(((stop - start) / 1000000))
    if [ -z "$best" ] || [ "$elapsed" -lt "$best" ]; then
      best=$elapsed
    fi
  done
  printf '%s\n' "$best"
}
CHACHA="$ROOT/test/chacha20/mc_chacha20.fas"
check_ms=$(best_ms "$OCAML_FAS" check "$CHACHA")
build_ms=$(best_ms "$OCAML_FAS" -O0 -c "$CHACHA" -o "$WORK/chacha.o")
echo "check_command: check $check_ms ms, -O0 -c $build_ms ms (best of 3, mc_chacha20.fas)"
[ "$check_ms" -lt "$build_ms" ] || fail "check was not faster than -O0 -c"

echo "check_command: passed"
