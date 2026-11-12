#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-cc}
OCAML_FAS=${OCAML_FAS:-"$ROOT/_build/default/bin/main.exe"}

MATRIX_TMP=$(mktemp -d)
trap 'rm -rf "$MATRIX_TMP"' EXIT HUP INT TERM
ulimit -c 0 2>/dev/null || true

PYTHONDONTWRITEBYTECODE=1 python3 - "$MATRIX_TMP" << 'EOF'
import sys

out = sys.argv[1]
lines = []
checks = [0]

WIDTHS = {"u8": (8, False), "i8": (8, True), "u16": (16, False), "i16": (16, True),
          "u32": (32, False), "i32": (32, True), "u64": (64, False), "i64": (64, True)}


def lit(v, w, signed):
    if signed and v >= (1 << (w - 1)):
        return str(v - (1 << w))
    return str(v)


def check(expr, expected, tag):
    idx = len(checks)
    checks.append((tag, expr, expected))
    lines.append(f"  if {expr} != {expected} {{ return {idx} }}")


def vec(name, vals, ty, w, signed):
    lines.append(f"  {name} vec[{len(vals)},{ty}] = splat(0)")
    for i, v in enumerate(vals):
        lines.append(f"  {name}[{i}] = {lit(v, w, signed)}")


def raw(v, w):
    return v & ((1 << w) - 1)


seq = 0
for ty, (w, signed) in WIDTHS.items():
    for shape in (1, 3):
        seq += 1
        vals = [raw(0x9E + 37 * i * seq + (1 << (w - 1)) if i == 1 else 0x9E + 37 * i * seq, w) for i in range(shape)]
        vals[0] = raw(0, w) if shape > 1 else vals[0]
        name = f"bc{seq}"
        vec(name, vals, ty, w, signed)
        for op, oracle in (
            ("popcount", lambda v: bin(v).count("1")),
            ("clz", lambda v: w - v.bit_length() if v else w),
            ("ctz", lambda v: (v & -v).bit_length() - 1 if v else w),
        ):
            for i in range(shape):
                expect = oracle(vals[i])
                check(f"{op}({name}[{i}])", lit(expect, w, signed), f"{op}-{ty}-{shape}")

for ty, (w, signed) in WIDTHS.items():
    seq += 1
    a = [raw(0x1234 + 40503 * seq, w), raw(0x7F, w), raw(1 << (w - 1), w), raw(0xFF, w)][: max(1, min(4, 1 + (seq % 3)))]
    b = [raw(0x2345 + 22222 * seq, w), raw(0x02, w), raw(3, w), raw(1, w)][: len(a)]
    name_a, name_b = f"sa{seq}", f"sb{seq}"
    vec(name_a, a, ty, w, signed)
    vec(name_b, b, ty, w, signed)
    for i in range(len(a)):
        prod = a[i] * b[i]
        hi = (prod >> w) & ((1 << w) - 1)
        if signed:
            sa = a[i] - (1 << w) if a[i] >= (1 << (w - 1)) else a[i]
            sb = b[i] - (1 << w) if b[i] >= (1 << (w - 1)) else b[i]
            hi = ((sa * sb) >> w) & ((1 << w) - 1)
        check(f"mul_hi({name_a}[{i}], {name_b}[{i}])", lit(hi, w, signed), f"mul_hi-{ty}")

for ty, (w, signed) in WIDTHS.items():
    seq += 1
    if signed:
        mn = raw(1 << (w - 1), w)
        mx = raw((1 << (w - 1)) - 1, w)
    else:
        mn = 0
        mx = raw((1 << w) - 1, w)
    vals = [mn, mx, 0, raw(0x11, w)]
    name = f"sv{seq}"
    vec(name, vals, ty, w, signed)
    smn = mn - (1 << w) if signed and mn >= (1 << (w - 1)) else mn
    smx = mx - (1 << w) if signed and mx >= (1 << (w - 1)) else mx
    cap_hi = (1 << (w - 1)) - 1 if signed else (1 << w) - 1
    cap_lo = -(1 << (w - 1)) if signed else 0
    add_sat = raw(max(cap_lo, min(smn + smn, cap_hi)), w)
    sub_sat = raw(max(cap_lo, min(smx - smn, cap_hi)), w)
    check(f"add_sat({name}[0], {name}[0])", lit(add_sat, w, signed), f"add_sat-{ty}")
    check(f"sub_sat({name}[1], {name}[0])", lit(sub_sat, w, signed), f"sub_sat-{ty}")

for ty, (w, signed) in (("u8", (8, False)), ("i8", (8, True)), ("u32", (32, False)),
                        ("i32", (32, True)), ("u64", (64, False))):
    for shape in (1, 3, 4):
        seq += 1
        vals = [raw(0x53 * (i + 1) + seq, w) for i in range(shape)]
        name = f"rd{seq}"
        vec(name, vals, ty, w, signed)
        svals = [v - (1 << w) if signed and v >= (1 << (w - 1)) else v for v in vals]
        total = sum(svals)
        check(f"reduce_sum({name})", lit(raw(total, w), w, signed), f"sum-{ty}-{shape}")
        pick_min = min(svals) if signed else min(vals)
        pick_max = max(svals) if signed else max(vals)
        check(f"reduce_min({name})", lit(raw(pick_min, w), w, signed), f"min-{ty}-{shape}")
        check(f"reduce_max({name})", lit(raw(pick_max, w), w, signed), f"max-{ty}-{shape}")
        acc = vals[0]
        for v in vals[1:]:
            acc &= v
        check(f"reduce_and({name})", lit(acc, w, signed), f"and-{ty}-{shape}")
        acc = vals[0]
        for v in vals[1:]:
            acc |= v
        check(f"reduce_or({name})", lit(acc, w, signed), f"or-{ty}-{shape}")
        acc = vals[0]
        for v in vals[1:]:
            acc ^= v
        check(f"reduce_xor({name})", lit(acc, w, signed), f"xor-{ty}-{shape}")

for shape in (2, 3, 4):
    seq += 1
    vals = [raw(0x21 * (i + 1) + seq, 32) for i in range(shape)]
    pick = [(i * 2 + 1) % (shape + 1) for i in range(shape)]
    comp = [vals[i] for i in range(shape) if i % 2 == 0] + [0] * ((shape + 1) // 2)
    comp = comp[:shape]
    cursor = 0
    expd = []
    for i in range(shape):
        if i % 2 == 0:
            expd.append(vals[cursor])
            cursor += 1
        else:
            expd.append(0)
    name, mname = f"rt{seq}", f"rm{seq}"
    vec(name, vals, "u32", 32, False)
    lines.append(f"  {mname} vec[{shape},bool] = splat(false)")
    for i in range(shape):
        if i % 2 == 0:
            lines.append(f"  {mname}[{i}] = true")
    lines.append(f"  tmpC{seq} vec[{shape},u32] = compress({name}, {mname})")
    lines.append(f"  tmpE{seq} vec[{shape},u32] = expand({name}, {mname})")
    for j in range(shape):
        check(f"tmpC{seq}[{j}]", comp[j], f"compress-{shape}-{j}")
        check(f"tmpE{seq}[{j}]", expd[j], f"expand-{shape}-{j}")
    pick = [(i * 2 + 1) % (shape + 1) for i in range(shape)]
    lines.append(f"  {mname}i vec[{shape},u32] = splat(0)")
    for i in range(shape):
        lines.append(f"  {mname}i[{i}] = {pick[i]}")
    permuted = [vals[p] if p < shape else 0 for p in pick]
    lines.append(f"  tmpP{seq} vec[{shape},u32] = permute({name}, {mname}i)")
    for j in range(shape):
        check(f"tmpP{seq}[{j}]", permuted[j], f"permute-{shape}-{j}")

CONV = list(WIDTHS.items()) + [("usize", (64, False)), ("isize", (64, True))]
for src_ty, (sw, ssigned) in CONV:
    seq += 1
    if ssigned:
        vals = [0, raw(1 << (sw - 1), sw)]
    else:
        vals = [0, raw((1 << sw) - 1, sw)]
    name = f"cv{seq}"
    vec(name, vals, src_ty, sw, ssigned)
    for dst_ty, (dw, dsigned) in CONV:
        for i, v in enumerate(vals):
            if dw > sw:
                zexp = v
                sexp = (v | (((1 << dw) - 1) ^ ((1 << sw) - 1))) if (v >> (sw - 1)) & 1 else v
                check(f"zext[{dst_ty}]({name}[{i}])", lit(zexp, dw, dsigned), f"zext-{src_ty}-{dst_ty}")
                check(f"sext[{dst_ty}]({name}[{i}])", lit(sexp, dw, dsigned), f"sext-{src_ty}-{dst_ty}")
            elif dw < sw:
                texp = v & ((1 << dw) - 1)
                check(f"trunc[{dst_ty}]({name}[{i}])", lit(texp, dw, dsigned), f"trunc-{src_ty}-{dst_ty}")
check("zext[usize](true)", 1, "zext-bool-usize")
check("sext[isize](true)", -1, "sext-bool-isize")
lines.append("  bt u8 = 3")
lines.append("  bf u8 = 2")
check("trunc[bool](bt)", "true", "trunc-bool-keep-bit")
check("trunc[bool](bf)", "false", "trunc-bool-drop-bit")

lines.append("  return 0")
lines.append("}")
with open(f"{out}/matrix.fas", "w") as handle:
    handle.write("fn main() i32 {\n" + "\n".join(lines) + "\n")
with open(f"{out}/oracle.txt", "w") as handle:
    for entry in checks[1:]:
        handle.write(repr(entry) + "\n")
print(f"matrix cells: {len(checks) - 1}", file=sys.stderr)
EOF

"$OCAML_FAS" --emit-llvm "$MATRIX_TMP/matrix.fas" >"$MATRIX_TMP/matrix.ll"
if grep -E '\b(poison|undef)\b' "$MATRIX_TMP/matrix.ll" >/dev/null; then
  printf 'simd matrix: undefined value in runtime IR\n' >&2
  exit 1
fi
"$LLVM_OPT" -passes=verify "$MATRIX_TMP/matrix.ll" -disable-output

for level in 0 2 3; do
  case "$level" in
    0) ;;
    *) "$LLVM_OPT" "-passes=default<O$level>" "$MATRIX_TMP/matrix.ll" -S -o "$MATRIX_TMP/matrix.O$level.ll" ;;
  esac
  "$LLVM_LLC" -O"$level" -filetype=obj \
    "$([ "$level" = 0 ] && printf '%s' "$MATRIX_TMP/matrix.ll" || printf '%s' "$MATRIX_TMP/matrix.O$level.ll")" \
    -o "$MATRIX_TMP/matrix.O$level.o"
  "$CC" "$MATRIX_TMP/matrix.O$level.o" -o "$MATRIX_TMP/matrix.O$level"
  set +e
  "$MATRIX_TMP/matrix.O$level"
  status=$?
  set -e
  if [ "$status" -ne 0 ]; then
    printf 'simd matrix: O%d returned %s\n' "$level" "$status" >&2
    exit 1
  fi
done

printf 'simd matrix: ok (%s cells)\n' "$(wc -l <"$MATRIX_TMP/oracle.txt")"
