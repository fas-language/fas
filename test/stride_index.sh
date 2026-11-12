#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-cc}
OCAML_FAS=${OCAML_FAS:-"$ROOT/_build/default/bin/main.exe"}

STRIDE_TMP=$(mktemp -d)
trap 'rm -rf "$STRIDE_TMP"' EXIT HUP INT TERM

PYTHONDONTWRITEBYTECODE=1 python3 - "$STRIDE_TMP" << 'PYEOF'
import json
import pathlib
import sys

out = pathlib.Path(sys.argv[1])
cases = [
    ("u32", 0, "<", 10, "+", 2, [("<", 6)]),
    ("u32", 0, "<", 10, "+", 2, [("<=", 5)]),
    ("i32", 0, "<", 10, "+", 2, [("<", 6)]),
    ("i32", 0, "<", 10, "+", 2, [("<=", 5)]),
    ("u32", 8, ">", 0, "-", 2, [("<", 6)]),
    ("i32", 8, ">", 0, "-", 2, [("<", 6)]),
    ("i32", 0, "<", 10, "+", 2, [(">", -1), ("<", 6)]),
    ("u32", 0, "<", 10, "+", 2, [(">=", 1), ("<", 6)]),
    ("u32", 1, "<", 12, "+", 3, [("<", 6)]),
    ("i32", 1, "<", 12, "+", 3, [("<", 5)]),
    ("u32", 9, ">", 2, "-", 2, [("<", 5)]),
]
(out / "cases.json").write_text(json.dumps(cases))

def c_case(index, case):
    ty, start, loop_op, bound, step_op, step, guards = case
    ctype = "uint32_t" if ty == "u32" else "int32_t"
    opened = "".join(f"if (i {op} {constant}) {{\n" for op, constant in guards)
    closed = "}\n" * len(guards)
    return (
        f"static int64_t case{index}(void) {{\n"
        f"  uint32_t sum = 0;\n"
        f"  for ({ctype} i = {start}; i {loop_op} {bound}; i {step_op}= {step}) {{\n"
        f"{opened}"
        f"  if ((int64_t)i < 0 || (int64_t)i >= 5) return -1;\n"
        f"  sum += t[i];\n"
        f"{closed}"
        "  }\n"
        "  return (int64_t)sum;\n"
        "}\n"
    )

oracle = [
    "#include <stdint.h>",
    "#include <stdio.h>",
    "static const uint32_t t[5] = {1, 2, 3, 4, 5};",
]
oracle += [c_case(index, case) for index, case in enumerate(cases)]
oracle.append("int main(void) {")
oracle += [
    f'  printf("%lld\\n", (long long)case{index}());'
    for index in range(len(cases))
]
oracle += ["  return 0;", "}"]
(out / "oracle.c").write_text("\n".join(oracle) + "\n")
PYEOF

"$CC" -std=c11 -O0 -o "$STRIDE_TMP/oracle" "$STRIDE_TMP/oracle.c"
"$STRIDE_TMP/oracle" > "$STRIDE_TMP/expected.txt"

PYTHONDONTWRITEBYTECODE=1 python3 - "$STRIDE_TMP" << 'PYEOF'
import json
import pathlib
import sys

out = pathlib.Path(sys.argv[1])
cases = json.loads((out / "cases.json").read_text())
expected = [int(line) for line in (out / "expected.txt").read_text().split()]
if len(expected) != len(cases) or any(value < 0 for value in expected):
    raise SystemExit("stride index: C oracle produced an out-of-range index")
functions = []
for index, (case, want) in enumerate(zip(cases, expected)):
    ty, start, loop_op, bound, step_op, step, guards = case
    opened = "".join(f"  if i {op} {constant} {{\n" for op, constant in guards)
    closed = "  }\n" * len(guards)
    functions.append(
        f"fn case{index}() u32 {{\n"
        "  t arr[5, u32] = {1, 2, 3, 4, 5}\n"
        "  sum u32 = 0\n"
        f"  for i {ty} = {start}; i {loop_op} {bound}; i {step_op}= {step} {{\n"
        f"{opened}"
        "  sum = sum + t[i]\n"
        f"{closed}"
        "  }\n"
        "  return sum\n"
        "}\n"
    )
main = (
    "fn main() i32 {\n"
    + "".join(
        f"  if case{index}() != {want} {{ return {index + 1} }}\n"
        for index, want in enumerate(expected)
    )
    + "  return 0\n}\n"
)
(out / "values.fas").write_text("\n".join(functions) + main)
PYEOF

"$OCAML_FAS" --emit-llvm "$STRIDE_TMP/values.fas" >"$STRIDE_TMP/values.ll"
"$LLVM_OPT" -passes=verify "$STRIDE_TMP/values.ll" -disable-output
"$LLVM_OPT" "-passes=default<O2>" "$STRIDE_TMP/values.ll" -S -o "$STRIDE_TMP/values.O2.ll"
"$LLVM_OPT" -passes=verify "$STRIDE_TMP/values.O2.ll" -disable-output

for level in 0 2; do
  case "$level" in
    0) input="$STRIDE_TMP/values.ll" ;;
    *) input="$STRIDE_TMP/values.O2.ll" ;;
  esac
  "$LLVM_LLC" -O"$level" -filetype=obj "$input" -o "$STRIDE_TMP/values.O$level.o"
  "$CC" "$STRIDE_TMP/values.O$level.o" -o "$STRIDE_TMP/values.O$level"
  set +e
  "$STRIDE_TMP/values.O$level"
  status=$?
  set -e
  if [ "$status" -ne 0 ]; then
    printf 'stride index: O%d returned %s\n' "$level" "$status" >&2
    exit 1
  fi
done

printf 'stride index: ok\n'
