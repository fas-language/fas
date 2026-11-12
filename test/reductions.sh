#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-cc}
OCAML_FAS=${OCAML_FAS:-"$ROOT/_build/default/bin/main.exe"}

RED_TMP=$(mktemp -d)
trap 'rm -rf "$RED_TMP"' EXIT HUP INT TERM
ulimit -c 0 2>/dev/null || true

PYTHONDONTWRITEBYTECODE=1 python3 - "$RED_TMP" << 'EOF'
import sys

out = sys.argv[1]
lines = []
checks = [0]


def check(expr, expected, tag):
    idx = len(checks)
    checks.append((tag, expr, expected))
    lines.append(f"  if {expr} != {expected} {{ return {idx} }}")


def vec(name, vals, ty, w):
    lines.append(f"  {name} vec[{len(vals)},{ty}] = splat(0)")
    for i, v in enumerate(vals):
        lines.append(f"  {name}[{i}] = {v}")


def sval(v, w):
    return v if v < (1 << (w - 1)) else v - (1 << w)


u8 = [0x8A, 0x8C, 0xF8, 0x8B]
assert sum(u8) >= 256
assert (u8[0] & u8[1] & u8[2] & u8[3]) not in (0, u8[0])
vec("a8", u8, "u8", 8)
check("reduce_sum(a8)", sum(u8) % 256, "sum-wrap-u8")
check("reduce_and(a8)", u8[0] & u8[1] & u8[2] & u8[3], "and-u8")
check("reduce_or(a8)", u8[0] | u8[1] | u8[2] | u8[3], "or-u8")
check("reduce_xor(a8)", u8[0] ^ u8[1] ^ u8[2] ^ u8[3], "xor-u8")
check("reduce_min(a8)", min(u8), "min-u8")
check("reduce_max(a8)", max(u8), "max-u8")

i8 = [0x8A, 0x3C, 0xF1, 0x07]
sv = [sval(v, 8) for v in i8]
assert min(sv) != min(i8) and max(sv) != max(i8)
vec("b8", sv, "i8", 8)
check("reduce_min(b8)", min(sv), "min-signed-i8")
check("reduce_max(b8)", max(sv), "max-signed-i8")
check("reduce_sum(b8)", (sum(sv) % 256) - 256 if sum(sv) % 256 >= 128 else sum(sv) % 256, "sum-wrap-i8")

i32 = [2147483647, 2147483647, -2147483648, 5]
assert min(i32) == -2147483648 and max(i32) == 2147483647
assert not (-(1 << 31) <= sum(i32) < (1 << 31))
vec("c32", i32, "i32", 32)
check("reduce_min(c32)", min(i32), "min-i32")
check("reduce_max(c32)", max(i32), "max-i32")
check("reduce_sum(c32)", sum(i32) - (1 << 32), "sum-wrap-i32")

u32 = [0x80000000, 0x7FFFFFFF, 0x80000002, 0x80000001]
assert min(u32) == 0x7FFFFFFF
vec("d32", u32, "u32", 32)
check("reduce_min(d32)", min(u32), "min-unsigned-u32")
check("reduce_max(d32)", max(u32), "max-unsigned-u32")

u64 = [0x8000000000000005, 3, 0xFFFFFFFFFFFFFFFF, 1]
assert min(u64) == 1 and max(u64) == 0xFFFFFFFFFFFFFFFF
vec("e64", u64, "u64", 64)
check("reduce_min(e64)", min(u64), "min-u64-high")
check("reduce_max(e64)", max(u64), "max-u64-high")
check("reduce_sum(e64)", sum(u64) % (1 << 64), "sum-wrap-u64")
check("reduce_xor(e64)", u64[0] ^ u64[1] ^ u64[2] ^ u64[3], "xor-u64")

one = [-5]
lines.append("  one vec[1,i32] = splat(0)")
lines.append("  one[0] = -5")
check("reduce_sum(one)", -5, "one-lane-sum")
check("reduce_min(one)", -5, "one-lane-min")

odd = [7, -2, 9]
lines.append("  three vec[3,i16] = splat(0)")
lines.append("  three[0] = 7")
lines.append("  three[1] = -2")
lines.append("  three[2] = 9")
check("reduce_sum(three)", 7 - 2 + 9, "odd-sum-i16")
check("reduce_min(three)", -2, "odd-min-i16")
check("reduce_max(three)", 9, "odd-max-i16")

lines.append("  return 0")
lines.append("}")
with open(f"{out}/reductions.fas", "w") as handle:
    handle.write("fn main() i32 {\n" + "\n".join(lines) + "\n")
with open(f"{out}/oracle.txt", "w") as handle:
    for entry in checks[1:]:
        handle.write(repr(entry) + "\n")
EOF

"$OCAML_FAS" --emit-llvm "$RED_TMP/reductions.fas" >"$RED_TMP/reductions.ll"
for needle in 'extractelement' 'add i' 'icmp slt i' 'icmp ult i'; do
  if ! grep -q "$needle" "$RED_TMP/reductions.ll"; then
    printf 'reductions: missing %s in runtime IR\n' "$needle" >&2
    exit 1
  fi
done
if grep -E '\b(poison|undef)\b' "$RED_TMP/reductions.ll" >/dev/null; then
  printf 'reductions: undefined value in runtime IR\n' >&2
  exit 1
fi
"$LLVM_OPT" -passes=verify "$RED_TMP/reductions.ll" -disable-output

for level in 0 2 3; do
  case "$level" in
    0) ;;
    *) "$LLVM_OPT" "-passes=default<O$level>" "$RED_TMP/reductions.ll" -S -o "$RED_TMP/reductions.O$level.ll" ;;
  esac
  "$LLVM_LLC" -O"$level" -filetype=obj \
    "$([ "$level" = 0 ] && printf '%s' "$RED_TMP/reductions.ll" || printf '%s' "$RED_TMP/reductions.O$level.ll")" \
    -o "$RED_TMP/reductions.O$level.o"
  "$CC" "$RED_TMP/reductions.O$level.o" -o "$RED_TMP/reductions.O$level"
  set +e
  "$RED_TMP/reductions.O$level"
  status=$?
  set -e
  if [ "$status" -ne 0 ]; then
    printf 'reductions: O%d returned %s\n' "$level" "$status" >&2
    exit 1
  fi
done

printf 'reductions: ok\n'
