#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-cc}
OCAML_FAS=${OCAML_FAS:-"$ROOT/_build/default/bin/main.exe"}

MASK_TMP=$(mktemp -d)
trap 'rm -rf "$MASK_TMP"' EXIT HUP INT TERM
ulimit -c 0 2>/dev/null || true

PYTHONDONTWRITEBYTECODE=1 python3 - "$MASK_TMP" << 'EOF'
import sys

out = sys.argv[1]
lines = []
checks = [0]
x = [1, 2, 3, 4]
y = [1, 0, 3, 9]
z = [1, 2, 0, 8]


def pack(lanes):
    return sum((v & 0xFF) << (8 * i) for i, v in enumerate(lanes))


def vec(name, vals):
    lines.append(f"  {name} vec[4,u8] = splat(0)")
    for i, v in enumerate(vals):
        lines.append(f"  {name}[{i}] = {v}")


def check(expr, expected, tag):
    idx = len(checks)
    checks.append((tag, expr, expected))
    lines.append(f"  if {expr} != {expected} {{ return {idx} }}")


vec("xv", x)
vec("yv", y)
vec("zv", z)
vec("pv", [10, 20, 30, 40])
vec("pw", [11, 21, 31, 41])
lines.append("  m vec[4,bool] = xv == yv")
lines.append("  n vec[4,bool] = xv == zv")
mask_m = [int(a == b) for a, b in zip(x, y)]
mask_n = [int(a == b) for a, b in zip(x, z)]
assert any(mask_m) and not all(mask_m) and any(mask_n) and not all(mask_n)
assert mask_m != mask_n and [a & b for a, b in zip(mask_m, mask_n)] != mask_m
assert [a | b for a, b in zip(mask_m, mask_n)] != [a ^ b for a, b in zip(mask_m, mask_n)]
for name, mask in (
    ("select-pick", mask_m),
    ("bitand-pick", [int(a and b) for a, b in zip(mask_m, mask_n)]),
    ("bitor-pick", [int(a or b) for a, b in zip(mask_m, mask_n)]),
    ("bitxor-pick", [int(a != b) for a, b in zip(mask_m, mask_n)]),
    ("not-pick", [1 - v for v in mask_m]),
):
    pick = [10 + 10 * i if mask[i] else 11 + 10 * i for i in range(4)]
    check("bitcast[u32](select(%s, pv, pw))" % ("m" if name == "select-pick" else {"bitand-pick": "m & n", "bitor-pick": "m | n", "bitxor-pick": "m ^ n", "not-pick": "!m"}[name]), pack(pick), name)
check("any(m)", "true", "any-mixed-true")
check("all(m)", "false", "all-mixed-false")
check("any(!m & !n)", "true" if any(1 - a & (1 - b) for a, b in zip(mask_m, mask_n)) else "false", "any-and-not")
check("all(m | n)", "true" if all(a | b for a, b in zip(mask_m, mask_n)) else "false", "all-or")
check("all(!m | n)", "true" if all((1 - a) | b for a, b in zip(mask_m, mask_n)) else "false", "all-not-or")
check("any(m & !m)", "false", "any-self-and-not-false")
check("all(m | !m)", "true", "all-self-or-not-true")
lines.append("  return 0")
lines.append("}")
with open(f"{out}/mask.fas", "w") as handle:
    handle.write("fn main() i32 {\n" + "\n".join(lines) + "\n")
with open(f"{out}/oracle.txt", "w") as handle:
    for entry in checks[1:]:
        handle.write(repr(entry) + "\n")

with open(f"{out}/trap.fas", "w") as handle:
    handle.write(
        "fn main() i32 {\n"
        "  m vec[2,bool] = splat(false)\n"
        "  a vec[2,i32] = splat(6)\n"
        "  b vec[2,i32] = splat(0)\n"
        "  c vec[2,i32] = splat(9)\n"
        "  r vec[2,i32] = select(m, a / b, c)\n"
        "  return 0\n"
        "}\n"
    )
EOF

"$OCAML_FAS" --emit-llvm "$MASK_TMP/mask.fas" >"$MASK_TMP/mask.ll"
for needle in 'and <4 x i1>' 'or <4 x i1>' 'xor <4 x i1>' 'select <4 x i1>' 'extractelement <4 x i1>' 'or i1' 'and i1'; do
  if ! grep -q "$needle" "$MASK_TMP/mask.ll"; then
    printf 'masks/selection: missing %s in runtime IR\n' "$needle" >&2
    exit 1
  fi
done
if grep -E '\b(poison|undef)\b' "$MASK_TMP/mask.ll" >/dev/null; then
  printf 'masks/selection: undefined value in runtime IR\n' >&2
  exit 1
fi
"$LLVM_OPT" -passes=verify "$MASK_TMP/mask.ll" -disable-output

for level in 0 2 3; do
  case "$level" in
    0) ;;
    *) "$LLVM_OPT" "-passes=default<O$level>" "$MASK_TMP/mask.ll" -S -o "$MASK_TMP/mask.O$level.ll" ;;
  esac
  "$LLVM_LLC" -O"$level" -filetype=obj \
    "$([ "$level" = 0 ] && printf '%s' "$MASK_TMP/mask.ll" || printf '%s' "$MASK_TMP/mask.O$level.ll")" \
    -o "$MASK_TMP/mask.O$level.o"
  "$CC" "$MASK_TMP/mask.O$level.o" -o "$MASK_TMP/mask.O$level"
  set +e
  "$MASK_TMP/mask.O$level"
  status=$?
  set -e
  if [ "$status" -ne 0 ]; then
    printf 'masks/selection: O%d returned %s\n' "$level" "$status" >&2
    exit 1
  fi
done

"$OCAML_FAS" --emit-llvm "$MASK_TMP/trap.fas" >"$MASK_TMP/trap.ll"
if ! grep -q 'sdiv <2 x i32>' "$MASK_TMP/trap.ll"; then
  printf 'masks/selection: eager select lost the guarded division\n' >&2
  exit 1
fi
"$LLVM_OPT" -passes=verify "$MASK_TMP/trap.ll" -disable-output
"$LLVM_LLC" -O2 -filetype=obj "$MASK_TMP/trap.ll" -o "$MASK_TMP/trap.o"
"$CC" "$MASK_TMP/trap.o" -o "$MASK_TMP/trap"
set +e
"$MASK_TMP/trap"
status=$?
set -e
if [ "$status" -eq 0 ]; then
  printf 'masks/selection: select guarded the division (no trap)\n' >&2
  exit 1
fi

printf 'masks/selection: ok\n'
