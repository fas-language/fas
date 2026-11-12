#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_OBJDUMP=${LLVM_OBJDUMP:-llvm-objdump-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/variadic_call_types.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
for signature in \
  'call i32 (ptr, ...) @printf' \
  'call i32 (ptr, ptr, ...) @fprintf' \
  'call i32 (ptr, i64, ptr, ...) @snprintf' \
  'call i32 (i32, ...) @sum'; do
  grep -F "$signature" "$TMP/program.ll" >/dev/null || {
    echo "variadic calls: missing $signature" >&2
    exit 1
  }
done
grep -F 'call i32 @fas_clear_rax()' "$TMP/program.ll" >/dev/null
grep -F 'call i32 @fas_expected_sum(i32' "$TMP/program.ll" >/dev/null
if grep -F 'call i32 (i32) @fas_expected_sum' "$TMP/program.ll" >/dev/null; then
  echo 'variadic calls: non-variadic call acquired a function type' >&2
  exit 1
fi
"$LLVM_OPT" -S -passes='default<O2>' -verify-each "$TMP/program.ll" \
  -o "$TMP/program-o2.ll"
"$LLVM_OPT" -passes=verify "$TMP/program-o2.ll" -disable-output

"$OCAML_FAS" -O0 -c "$ROOT/test/variadic_call_types.fas" -o "$TMP/program-O0.o"
"$LLVM_OBJDUMP" -dr --no-show-raw-insn "$TMP/program-O0.o" >"$TMP/program-O0.dis"
python3 - "$TMP/program-O0.dis" <<'PY'
import re
import sys

text = open(sys.argv[1], encoding="utf-8").read().splitlines()
pending_al = None
calls = {name: [] for name in ("printf", "fprintf", "snprintf", "sum")}
for line in text:
    immediate = re.search(r"\bmovb\s+\$0x([0-9a-fA-F]+),\s*%al\b", line)
    if immediate:
        pending_al = int(immediate.group(1), 16)
    relocation = re.search(r"R_X86_64_PLT32\s+(\S+)", line)
    if relocation:
        name = re.sub(r"-0x[0-9a-fA-F]+$", "", relocation.group(1))
        if name in calls:
            if pending_al is None:
                raise SystemExit(f"variadic calls: {name} has no preceding %al write")
            calls[name].append(pending_al)
        pending_al = None
expected = {
    "printf": [1],
    "fprintf": [1],
    "snprintf": [1],
    "sum": [1, 2, 3, 4, 5, 6, 7, 8, 8, 8],
}
if calls != expected:
    raise SystemExit(f"variadic calls: unexpected O0 %al writes {calls!r}")
PY
for level in 0 2; do
  "$OCAML_FAS" -O"$level" "$ROOT/test/variadic_call_types.fas" \
    "$ROOT/test/variadic_call_types.c" -o "$TMP/program-O$level"
  timeout 60 "$TMP/program-O$level" >"$TMP/out-O$level" 2>"$TMP/err-O$level"
done

echo 'variadic_call_types: typed imported and declared calls, O0/O2 runtime and O0 object: ok'
