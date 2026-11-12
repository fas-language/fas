#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-cc}
OCAML_FAS=${OCAML_FAS:-"$ROOT/_build/default/bin/main.exe"}

HELP_TMP=$(mktemp -d)
trap 'rm -rf "$HELP_TMP"' EXIT HUP INT TERM

PYTHONDONTWRITEBYTECODE=1 python3 - "$OCAML_FAS" "$HELP_TMP" << 'PYEOF'
import pathlib
import subprocess
import sys

compiler = sys.argv[1]
out = pathlib.Path(sys.argv[2])
prefix = "help: widen an operand first: `"
cases = [
    (
        [("nres", "u32", "40000000")],
        "usize",
        "nres * 140 + 16",
        "5600000016",
    ),
    (
        [("a", "u16", "65535"), ("b", "u16", "40000")],
        "u32",
        "(a + 1) * (b - 2)",
        "2621308928",
    ),
    (
        [("a", "u16", "65535"), ("b", "u16", "2"), ("c", "u16", "2")],
        "u32",
        "a * b * c",
        "262140",
    ),
    (
        [("a", "u16", "40000"), ("s", "u16", "3"), ("b", "u16", "4")],
        "u32",
        "a << s + b",
        "5120000",
    ),
    (
        [("a", "u16", "65535"), ("b", "u16", "2")],
        "i32",
        "-(a + b)",
        "-65537",
    ),
    (
        [("a", "u16", "65535"), ("b", "u16", "1"), ("c", "u16", "2")],
        "u32",
        "a - (b - c)",
        "65536",
    ),
    (
        [("a", "u16", "1"), ("b", "u16", "1")],
        "u32",
        "b * ~a",
        "4294967294",
    ),
    (
        [("a", "u16", "40000"), ("s", "i32", "-1")],
        "i64",
        "(a / 3) & s",
        "13333",
    ),
    (
        [("a", "u16", "40000"), ("s", "i32", "-1")],
        "i64",
        "(a * s) + 1",
        "-39999",
    ),
]
for value in ["-128", "-1", "0", "127"]:
    signed = int(value)
    cases.append(([("s", "i8", value)], "i32", "~s", str(~signed)))
    cases.append(([("s", "i8", value)], "i32", "~(s & 0x7f)", str(~(signed & 0x7F))))
    cases.append(([("s", "i8", value)], "i32", "~(s + 1)", str(~(signed + 1))))
for value in ["0", "1", "127", "255"]:
    unsigned = int(value)
    cases.append(([("u", "u8", value)], "u32", "~u", str(~unsigned & 0xFFFFFFFF)))
    cases.append(
        ([("u", "u8", value)], "u32", "~(u & 0x7f)", str(~(unsigned & 0x7F) & 0xFFFFFFFF))
    )
    cases.append(
        ([("u", "u8", value)], "u32", "~(u + 1)", str(~(unsigned + 1) & 0xFFFFFFFF))
    )
functions = []
for index, (decls, dest, expression, expected) in enumerate(cases):
    params = ", ".join(f"{name} {ty}" for name, ty, _ in decls)
    diagnostic = f"fn f({params}) {dest} {{ value {dest} = {expression}; return value }}\n"
    path = out / f"diag{index}.fas"
    path.write_text(diagnostic)
    result = subprocess.run(
        [compiler, "--emit-llvm", str(path)],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
        text=True,
    )
    helps = [line for line in result.stderr.splitlines() if line.startswith(prefix)]
    if result.returncode == 0 or len(helps) != 1 or not helps[0].endswith("`"):
        raise SystemExit(f"narrow help values: case {index} has no single widening help")
    suggestion = helps[0][len(prefix) : -1]
    body = "".join(f"  {name} {ty} = {value}\n" for name, ty, value in decls)
    functions.append(
        f"fn case{index}() i32 {{\n{body}"
        f"  got {dest} = {suggestion}\n"
        f"  if got != {expected} {{ return 1 }}\n"
        "  return 0\n}\n"
    )
main = (
    "fn main() i32 {\n"
    + "".join(f"  if case{index}() != 0 {{ return {index + 1} }}\n" for index in range(len(cases)))
    + "  return 0\n}\n"
)
(out / "values.fas").write_text("\n".join(functions) + main)
PYEOF

"$OCAML_FAS" --emit-llvm "$HELP_TMP/values.fas" >"$HELP_TMP/values.ll"
"$LLVM_OPT" -passes=verify "$HELP_TMP/values.ll" -disable-output

for level in 0 2; do
  case "$level" in
    0) ;;
    *) "$LLVM_OPT" "-passes=default<O$level>" "$HELP_TMP/values.ll" -S -o "$HELP_TMP/values.O$level.ll" ;;
  esac
  "$LLVM_LLC" -O"$level" -filetype=obj \
    "$([ "$level" = 0 ] && printf '%s' "$HELP_TMP/values.ll" || printf '%s' "$HELP_TMP/values.O$level.ll")" \
    -o "$HELP_TMP/values.O$level.o"
  "$CC" "$HELP_TMP/values.O$level.o" -o "$HELP_TMP/values.O$level"
  set +e
  "$HELP_TMP/values.O$level"
  status=$?
  set -e
  if [ "$status" -ne 0 ]; then
    printf 'narrow help values: O%d returned %s\n' "$level" "$status" >&2
    exit 1
  fi
done

printf 'narrow help values: ok\n'
