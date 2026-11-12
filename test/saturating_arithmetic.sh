#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-cc}
OCAML_FAS=${OCAML_FAS:-"$ROOT/_build/default/bin/main.exe"}

SAT_TMP=$(mktemp -d)
trap 'rm -rf "$SAT_TMP"' EXIT HUP INT TERM

PYTHONDONTWRITEBYTECODE=1 python3 - "$SAT_TMP" << 'EOF'
import random
import sys

out = sys.argv[1]
rng = random.Random(20260925)
lines = []
checks = [0]


def clamp(value, lo, hi):
    return min(max(value, lo), hi)


def bounds(bits, signed):
    if signed:
        return -(1 << (bits - 1)), (1 << (bits - 1)) - 1
    return 0, (1 << bits) - 1


def emit(op, bits, signed, a, b, tag, ty=None):
    lo, hi = bounds(bits, signed)
    assert lo <= a <= hi and lo <= b <= hi
    if op == "add":
        result = a + b
        builtin = "add_sat"
    elif op == "sub":
        result = a - b
        builtin = "sub_sat"
    else:
        result = (a * b) >> bits
        builtin = "mul_hi"
    expected = clamp(result, lo, hi)
    assert lo <= expected <= hi
    if ty is None:
        ty = ("i" if signed else "u") + str(bits)
    idx = len(checks)
    checks.append((tag, op, bits, signed, a, b, expected))
    lines.append(f"  a{idx} {ty} = {a}")
    lines.append(f"  b{idx} {ty} = {b}")
    lines.append(f"  if {builtin}(a{idx}, b{idx}) != {expected} {{ return {idx} }}")


def emit_unary(op, bits, signed, a, tag, ty=None):
    lo, hi = bounds(bits, signed)
    assert lo <= a <= hi
    pattern = a & ((1 << bits) - 1)
    if op == "popcount":
        expected = bin(pattern).count("1")
    elif op == "clz":
        expected = bits - pattern.bit_length()
    else:
        expected = (pattern & -pattern).bit_length() - 1 if pattern else bits
    assert 0 <= expected <= bits
    if ty is None:
        ty = ("i" if signed else "u") + str(bits)
    idx = len(checks)
    checks.append((tag, op, bits, signed, a, expected))
    lines.append(f"  a{idx} {ty} = {a}")
    lines.append(f"  if {op}(a{idx}) != {expected} {{ return {idx} }}")


for bits in (8, 16, 32, 64):
    for signed in (False, True):
        lo, hi = bounds(bits, signed)
        if signed:
            pairs = [
                (lo, lo),
                (lo, -1),
                (lo, 1),
                (hi, hi),
                (hi, -1),
                (hi, 1),
                (-1, hi),
                (0, lo),
                (lo, hi),
                (hi, lo),
            ]
        else:
            pairs = [
                (lo, lo),
                (lo, hi),
                (hi, lo),
                (hi, hi),
                (hi, 1),
                (1, hi),
                (hi - 1, 2),
                (hi // 2 + 1, hi // 2 + 1),
            ]
        for _ in range(6):
            pairs.append((rng.randint(lo, hi), rng.randint(lo, hi)))
        for a, b in pairs:
            for op in ("add", "sub", "mulhi"):
                emit(op, bits, signed, a, b, f"{bits}-{'s' if signed else 'u'}")
            for op in ("popcount", "clz", "ctz"):
                emit_unary(op, bits, signed, a, f"{bits}-{'s' if signed else 'u'}")

for signed, ty in ((False, "usize"), (True, "isize")):
    lo, hi = bounds(64, signed)
    if signed:
        pairs = [(lo, lo), (lo, -1), (hi, hi), (hi, 1), (-1, hi), (0, lo)]
    else:
        pairs = [(lo, lo), (lo, hi), (hi, lo), (hi, hi), (hi, 1), (1, hi)]
    for _ in range(4):
        pairs.append((rng.randint(lo, hi), rng.randint(lo, hi)))
    for a, b in pairs:
        for op in ("add", "sub", "mulhi"):
            emit(op, 64, signed, a, b, f"tsize-{'s' if signed else 'u'}", ty=ty)
        for op in ("popcount", "clz", "ctz"):
            emit_unary(op, 64, signed, a, f"tsize-{'s' if signed else 'u'}", ty=ty)


def emit_vec_unary(op, bits, signed, xs, tag):
    lo, hi = bounds(bits, signed)
    expected = []
    for x in xs:
        assert lo <= x <= hi
        pattern = x & ((1 << bits) - 1)
        if op == "popcount":
            expected.append(bin(pattern).count("1"))
        elif op == "clz":
            expected.append(bits - pattern.bit_length())
        else:
            expected.append((pattern & -pattern).bit_length() - 1 if pattern else bits)
    ty = ("i" if signed else "u") + str(bits)
    idx = len(checks)
    checks.append((tag, op, bits, signed, xs, expected))
    lines.append(f"  va{idx} vec[4, {ty}] = splat(0)")
    for lane in range(4):
        lines.append(f"  va{idx}[{lane}] = {xs[lane]}")
    lines.append(f"  vr{idx} vec[4, {ty}] = {op}(va{idx})")
    for lane in range(4):
        lines.append(f"  if vr{idx}[{lane}] != {expected[lane]} {{ return {idx} }}")
def emit_vec(op, bits, signed, xs, ys, tag):
    lo, hi = bounds(bits, signed)
    builtin = {"add": "add_sat", "sub": "sub_sat", "mulhi": "mul_hi"}[op]
    if op == "add":
        expected = [clamp(x + y, lo, hi) for x, y in zip(xs, ys)]
    elif op == "sub":
        expected = [clamp(x - y, lo, hi) for x, y in zip(xs, ys)]
    else:
        expected = [clamp((x * y) >> bits, lo, hi) for x, y in zip(xs, ys)]
    ty = ("i" if signed else "u") + str(bits)
    idx = len(checks)
    checks.append((tag, op, bits, signed, xs, ys, expected))
    lines.append(f"  va{idx} vec[4, {ty}] = splat(0)")
    lines.append(f"  vb{idx} vec[4, {ty}] = splat(0)")
    for lane in range(4):
        lines.append(f"  va{idx}[{lane}] = {xs[lane]}")
        lines.append(f"  vb{idx}[{lane}] = {ys[lane]}")
    lines.append(f"  vr{idx} vec[4, {ty}] = {builtin}(va{idx}, vb{idx})")
    for lane in range(4):
        lines.append(
            f"  if vr{idx}[{lane}] != {expected[lane]} {{ return {idx} }}"
        )


emit_vec("add", 8, False, [255, 0, 1, 128], [1, 0, 2, 127], "vec-u8")
emit_vec("sub", 8, False, [255, 0, 3, 128], [1, 1, 127, 127], "vec-u8")
emit_vec("add", 32, True, [-2147483648, -1, 2147483647, 0], [-1, 1, 1, 0], "vec-i32")
emit_vec("sub", 32, True, [-2147483648, 2147483647, 0, -5], [1, -1, 0, 10], "vec-i32")
emit_vec("add", 64, True, [-9223372036854775808, 9223372036854775807, 4, -4], [-1, 1, 5, -5], "vec-i64")
emit_vec("sub", 64, True, [-9223372036854775808, 9223372036854775807, 4, -4], [1, -1, -5, 5], "vec-i64")
emit_vec("mulhi", 8, False, [255, 2, 3, 0], [255, 3, 100, 9], "vec-u8")
emit_vec("mulhi", 32, True, [-2147483648, -1, 2147483647, 1000], [-2147483648, -1, 2147483647, -1000], "vec-i32")
emit_vec("mulhi", 64, True, [-9223372036854775808, 9223372036854775807, 4294967296, -3], [-1, 2, 4294967296, 7], "vec-i64")
emit_vec("mulhi", 64, False, [18446744073709551615, 4294967296, 3, 0], [18446744073709551615, 4294967296, 7, 9], "vec-u64")
emit_vec_unary("popcount", 8, False, [255, 2, 3, 0], "vec-u8")
emit_vec_unary("clz", 8, False, [255, 2, 3, 0], "vec-u8")
emit_vec_unary("ctz", 32, True, [-2147483648, 0, 1, 6], "vec-i32")

lines.append("  return 0")
lines.append("}")
with open(f"{out}/sat.fas", "w") as handle:
    handle.write("fn main() i32 {\n" + "\n".join(lines) + "\n")
with open(f"{out}/oracle.txt", "w") as handle:
    for entry in checks[1:]:
        handle.write(repr(entry) + "\n")
EOF

"$OCAML_FAS" --emit-llvm "$SAT_TMP/sat.fas" >"$SAT_TMP/sat.ll"
for needle in 'llvm.uadd.sat' 'llvm.sadd.sat' 'llvm.usub.sat' 'llvm.ssub.sat' 'mul i128' 'mul <4 x i16>' 'llvm.ctpop.i8' 'llvm.ctlz.i32' 'llvm.cttz.i64' 'llvm.ctpop.v4i8'; do
  if ! grep -q "$needle" "$SAT_TMP/sat.ll"; then
    printf 'saturating arithmetic: missing %s in runtime IR\n' "$needle" >&2
    exit 1
  fi
done
if grep -E '\b(poison|undef)\b' "$SAT_TMP/sat.ll" >/dev/null; then
  printf 'saturating arithmetic: undefined value in runtime IR\n' >&2
  exit 1
fi
"$LLVM_OPT" -passes=verify "$SAT_TMP/sat.ll" -disable-output

for level in 0 2 3; do
  case "$level" in
    0) ;;
    *) "$LLVM_OPT" "-passes=default<O$level>" "$SAT_TMP/sat.ll" -S -o "$SAT_TMP/sat.O$level.ll" ;;
  esac
  "$LLVM_LLC" -O"$level" -filetype=obj \
    "$([ "$level" = 0 ] && printf '%s' "$SAT_TMP/sat.ll" || printf '%s' "$SAT_TMP/sat.O$level.ll")" \
    -o "$SAT_TMP/sat.O$level.o"
  "$CC" "$SAT_TMP/sat.O$level.o" -o "$SAT_TMP/sat.O$level"
  set +e
  "$SAT_TMP/sat.O$level"
  status=$?
  set -e
  if [ "$status" -ne 0 ]; then
    printf 'saturating arithmetic: O%d returned %s\n' "$level" "$status" >&2
    exit 1
  fi
done

printf 'saturating arithmetic: ok\n'
