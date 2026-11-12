#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-cc}
OCAML_FAS=${OCAML_FAS:-"$ROOT/_build/default/bin/main.exe"}

ROUTE_TMP=$(mktemp -d)
trap 'rm -rf "$ROUTE_TMP"' EXIT HUP INT TERM
ulimit -c 0 2>/dev/null || true

PYTHONDONTWRITEBYTECODE=1 python3 - "$ROUTE_TMP" << 'EOF'
import sys

out = sys.argv[1]
lines = []
checks = [0]
a = [1, 2, 3, 4]
b = [5, 6, 7, 8]


def pack(lanes):
    return sum((v & 0xFF) << (8 * i) for i, v in enumerate(lanes))


def check(expr, expected, tag):
    idx = len(checks)
    checks.append((tag, expr, expected))
    lines.append(f"  if {expr} != {expected} {{ return {idx} }}")


def vec(name, vals):
    lines.append(f"  {name} vec[4,u8] = splat(0)")
    for i, v in enumerate(vals):
        lines.append(f"  {name}[{i}] = {v}")


vec("av", a)
vec("bv", b)
sel = [0, 2, 2, 7]
concat = a + b
picked = [concat[i] if i < 8 else 0 for i in sel]
check("bitcast[u32](shuffle(av, bv, I))", pack(picked), "shuffle-pick")
dynamic = [9, 1, 4, 3]
permuted = [a[i] if i < 4 else 0 for i in dynamic]
check("bitcast[u32](permute(av, IV))", pack(permuted), "permute-const-indices")
wide = [257, 1, 0, 65535]
widened = [a[i] if i < 4 else 0 for i in wide]
check("bitcast[u32](permute(av, IW))", pack(widened), "permute-no-truncation")
k_iv = sum((v & 0xFF) << (8 * i) for i, v in enumerate(dynamic))
k_iw = sum((v & 0xFFFF) << (16 * i) for i, v in enumerate(wide))
cv = [1, 2, 9, 4]
mask = [1 if a[i] == cv[i] else 0 for i in range(4)]
assert 0 < sum(mask) < 4
comp = [a[i] for i in range(4) if mask[i]] + [0] * (4 - sum(mask))
expd = []
j = 0
for i in range(4):
    if mask[i]:
        expd.append(a[j])
        j += 1
    else:
        expd.append(0)
assert comp != expd and comp != a
lines.append("  mv vec[4,bool] = av == CV")
lines.append("  lw vec[4,u8] = splat(0)")
lines.append("  lw[IV[1]] = 7")
lines.append("  lw[IV[1]] += 1")
check("bitcast[u32](lw)", pack([0, 8, 0, 0]), "lane-write-compound-dynamic")
check("av[IV[3]]", a[3], "lane-read-dynamic")
check("bitcast[u32](compress(av, mv))", pack(comp), "compress-runtime-mask")
check("bitcast[u32](expand(av, mv))", pack(expd), "expand-runtime-mask")
k_cv = sum((v & 0xFF) << (8 * i) for i, v in enumerate(cv))
lines.append("  return 0")
lines.append("}")
head = (
    "const KS u32 = 117572096\n"
    "const I vec[4,u8] = bitcast[vec[4,u8]](KS)\n"
    f"const KV u32 = {k_iv}\n"
    "const IV vec[4,u8] = bitcast[vec[4,u8]](KV)\n"
    f"const KW u64 = {k_iw}\n"
    "const IW vec[4,u16] = bitcast[vec[4,u16]](KW)\n"
    f"const KC u32 = {k_cv}\n"
    "const CV vec[4,u8] = bitcast[vec[4,u8]](KC)\n"
    "fn main() i32 {\n"
)
with open(f"{out}/route.fas", "w") as handle:
    handle.write(head + "\n".join(lines) + "\n")
with open(f"{out}/oracle.txt", "w") as handle:
    for entry in checks[1:]:
        handle.write(repr(entry) + "\n")

expected_sum = pack(permuted) + pack(widened)
exp_comp = [a[i] for i in range(4) if [1, 1, 0, 1][i]] + [0]
exp_expd = [a[0], a[1], 0, a[2]]
expected_sum2 = pack(exp_comp) + pack(exp_expd)
with open(f"{out}/dyn.fas", "w") as handle:
    handle.write(
        "fn go(IV vec[4,u8], IW vec[4,u16], av vec[4,u8]) u32 {\n"
        "  r vec[4,u8] = permute(av, IV)\n"
        "  w vec[4,u8] = permute(av, IW)\n"
        "  return bitcast[u32](r) + bitcast[u32](w)\n"
        "}\n"
        "fn main() i32 {\n"
        "  av vec[4,u8] = splat(0)\n"
        "  av[0] = 1\n"
        "  av[1] = 2\n"
        "  av[2] = 3\n"
        "  av[3] = 4\n"
        "  IV vec[4,u8] = splat(0)\n"
        "  IV[0] = 9\n"
        "  IV[1] = 1\n"
        "  IV[2] = 4\n"
        "  IV[3] = 3\n"
        "  IW vec[4,u16] = splat(0)\n"
        "  IW[0] = 257\n"
        "  IW[1] = 1\n"
        "  IW[2] = 0\n"
        "  IW[3] = 65535\n"
        "  DM vec[4,bool] = splat(false)\n"
        "  DM[0] = true\n"
        "  DM[1] = true\n"
        "  DM[3] = true\n"
        f"  if go(IV, IW, av) != {expected_sum} {{ return 1 }}\n"
        "  if go2(DM, av) != "
        + str(expected_sum2)
        + " { return 2 }\n"
        "  return 0\n"
        "}\n"
        "fn go2(DM vec[4,bool], av vec[4,u8]) u32 {\n"
        "  r vec[4,u8] = compress(av, DM)\n"
        "  w vec[4,u8] = expand(av, DM)\n"
        "  return bitcast[u32](r) + bitcast[u32](w)\n"
        "}\n"
    )
EOF

"$OCAML_FAS" --emit-llvm "$ROUTE_TMP/route.fas" >"$ROUTE_TMP/route.ll"
for needle in 'shufflevector <4 x i8>' 'insertelement <4 x i8>' 'icmp ult i8'; do
  if ! grep -q "$needle" "$ROUTE_TMP/route.ll"; then
    printf 'lane routing: missing %s in runtime IR\n' "$needle" >&2
    exit 1
  fi
done
if grep -E '\b(poison|undef)\b' "$ROUTE_TMP/route.ll" >/dev/null; then
  printf 'lane routing: undefined value in runtime IR\n' >&2
  exit 1
fi
"$LLVM_OPT" -passes=verify "$ROUTE_TMP/route.ll" -disable-output

for level in 0 2 3; do
  case "$level" in
    0) ;;
    *) "$LLVM_OPT" "-passes=default<O$level>" "$ROUTE_TMP/route.ll" -S -o "$ROUTE_TMP/route.O$level.ll" ;;
  esac
  "$LLVM_LLC" -O"$level" -filetype=obj \
    "$([ "$level" = 0 ] && printf '%s' "$ROUTE_TMP/route.ll" || printf '%s' "$ROUTE_TMP/route.O$level.ll")" \
    -o "$ROUTE_TMP/route.O$level.o"
  "$CC" "$ROUTE_TMP/route.O$level.o" -o "$ROUTE_TMP/route.O$level"
  set +e
  "$ROUTE_TMP/route.O$level"
  status=$?
  set -e
  if [ "$status" -ne 0 ]; then
    printf 'lane routing: O%d returned %s\n' "$level" "$status" >&2
    exit 1
  fi
done

"$OCAML_FAS" --emit-llvm "$ROUTE_TMP/dyn.fas" >"$ROUTE_TMP/dyn.ll"
"$LLVM_OPT" -passes=verify "$ROUTE_TMP/dyn.ll" -disable-output
"$LLVM_OPT" "-passes=default<O2>" "$ROUTE_TMP/dyn.ll" -S -o "$ROUTE_TMP/dyn.O2.ll"
"$LLVM_LLC" -O2 -filetype=obj "$ROUTE_TMP/dyn.O2.ll" -o "$ROUTE_TMP/dyn.o"
"$CC" "$ROUTE_TMP/dyn.o" -o "$ROUTE_TMP/dyn"
set +e
"$ROUTE_TMP/dyn"
status=$?
set -e
if [ "$status" -ne 0 ]; then
  printf 'lane routing: dynamic permute returned %s\n' "$status" >&2
  exit 1
fi

for kind in read write compound; do
  case "$kind" in
    read) body='  r u8 = av[i]
  return zext[u32](r)' ;;
    write) body='  av[i] = 7
  return bitcast[u32](av)' ;;
    compound) body='  av[i] += 1
  return bitcast[u32](av)' ;;
  esac
  cat >"$ROUTE_TMP/trap-$kind.fas" << EOF
fn go(av vec[4,u8], i usize) u32 {
$body
}
fn main() i32 {
  av vec[4,u8] = splat(0)
  r u32 = go(av, 7)
  return 0
}
EOF
  "$OCAML_FAS" --emit-llvm "$ROUTE_TMP/trap-$kind.fas" >"$ROUTE_TMP/trap-$kind.ll"
  if ! grep -q 'llvm.trap' "$ROUTE_TMP/trap-$kind.ll"; then
    printf 'lane routing: %s trap missing in IR\n' "$kind" >&2
    exit 1
  fi
  "$LLVM_OPT" -passes=verify "$ROUTE_TMP/trap-$kind.ll" -disable-output
  "$LLVM_OPT" '-passes=default<O2>' "$ROUTE_TMP/trap-$kind.ll" -S -o "$ROUTE_TMP/trap-$kind.O2.ll"
  "$LLVM_LLC" -O2 -filetype=obj "$ROUTE_TMP/trap-$kind.O2.ll" -o "$ROUTE_TMP/trap-$kind.o"
  "$CC" "$ROUTE_TMP/trap-$kind.o" -o "$ROUTE_TMP/trap-$kind"
  set +e
  "$ROUTE_TMP/trap-$kind"
  status=$?
  set -e
  if [ "$status" -eq 0 ]; then
    printf 'lane routing: %s accepted an invalid lane index\n' "$kind" >&2
    exit 1
  fi
done

printf 'lane routing: ok\n'
