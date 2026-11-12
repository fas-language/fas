#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
SHIFT_TMP=$(mktemp -d)
trap 'rm -rf "$SHIFT_TMP"' EXIT HUP INT TERM

PYTHONDONTWRITEBYTECODE=1 python3 - "$SHIFT_TMP" << 'EOF'
import sys, os

out = sys.argv[1]

def mask(w):
    return (1 << w) - 1

def sval(bits, w):
    bits &= mask(w)
    return bits - (1 << w) if bits >> (w - 1) else bits

def shl(x_bits, n_value, w):
    return (x_bits << (n_value % w)) & mask(w)

def shr_u(x_bits, n_value, w):
    return x_bits >> (n_value % w)

def shr_s(x_bits, n_value, w):
    return (sval(x_bits, w) >> (n_value % w)) & mask(w)

W = 8
su, hu, ss, hs = [], [], [], []
for x in range(256):
    for n in range(256):
        su.append(shl(x, n, W))
        hu.append(shr_u(x, n, W))
        ss.append(shl(x, sval(n, 8), W))
        hs.append(shr_s(x, sval(n, 8), W))

def arr(name, values):
    return "const %s arr[65536,u8] = { %s }\n" % (name, ",".join(map(str, values)))

lines = [arr("SU", su), arr("HU", hu), arr("SS", ss), arr("HS", hs)]
lines.append("""
fn main() i32 {
  x u16 = 0
  while x < 256 {
    n u16 = 0
    while n < 256 {
      xu u8 = trunc[u8](x)
      xi i8 = bitcast[i8](xu)
      nu u8 = trunc[u8](n)
      ni i8 = bitcast[i8](nu)
      i usize = zext[usize](x) * 256 + zext[usize](n)
      if xu << nu != SU[i] { return 1 }
      if xu >> nu != HU[i] { return 2 }
      if bitcast[u8](xi << ni) != SS[i] { return 3 }
      if bitcast[u8](xi >> ni) != HS[i] { return 4 }
      n += 1
    }
    x += 1
  }
  return 0
}
""")
open(os.path.join(out, "exhaustive8.fas"), "w").write("".join(lines))

widths = [("u8", 8, False), ("i8", 8, True), ("u16", 16, False), ("i16", 16, True),
          ("u32", 32, False), ("i32", 32, True), ("u64", 64, False), ("i64", 64, True),
          ("usize", 64, False), ("isize", 64, True)]
count_types = [(8, False), (8, True), (32, False), (32, True), (64, False), (64, True)]

def hexlit(bits, w):
    return "0x%X" % (bits & mask(w))

def var(name, ty, w, signed, bits):
    h = hexlit(bits, w)
    if signed:
        mid = "u64" if w == 64 else "u%d" % w
        return "  %s_bits %s = %s\n  %s %s = bitcast[%s](%s_bits)\n" % (name, mid, h, name, ty, ty, name)
    return "  %s %s = %s\n" % (name, ty, h)

body = []
body.append("fn main() i32 {\n")
idx = [0]
for (vt, vw, vs) in widths:
    for (nw, ns) in count_types:
        xbits_pool = [0, mask(vw), 1 << (vw - 1), (1 << (vw - 1)) - 1, 1]
        nbits_pool = [b & mask(nw) for b in
                      ([0, 1, vw - 1, vw, vw + 1, mask(nw), 1 << (nw - 1), (1 << (nw - 1)) - 1]
                       if ns else [0, 1, vw - 1, vw, vw + 1, mask(nw)])]
        for x_bits in xbits_pool:
            for n_bits in dict.fromkeys(nbits_pool):
                n_value = sval(n_bits, nw) if ns else n_bits
                exp_l = shl(x_bits, n_value, vw)
                exp_r = (shr_s if vs else shr_u)(x_bits, n_value, vw)
                idx[0] += 2
                nt = "u%d" % nw if not ns else "i%d" % nw
                body.append(var("x%d" % idx[0], vt, vw, vs, x_bits))
                body.append(var("n%d" % idx[0], nt, nw, ns, n_bits))
                body.append(var("e%d" % idx[0], vt, vw, vs, exp_l))
                body.append(var("right%d" % idx[0], vt, vw, vs, exp_r))
                body.append("  if x%d << n%d != e%d { return %d }\n" % (idx[0], idx[0], idx[0], idx[0]))
                body.append("  if x%d >> n%d != right%d { return %d }\n" % (idx[0], idx[0], idx[0], idx[0] + 1))
body.append("  return 0\n}\n")
open(os.path.join(out, "boundaries.fas"), "w").write("".join(body))

chains = []
chains.append("struct Box2 { slot2 u32 }\n")
chains.append("const G2 arr[2,u32] = { 2147483648, 0 }\n")
chains.append("const N u8 = 7\n")
chains.append("fn shg2[T](x T, n u32) T { return x << n }\n")
chains.append("fn lit_u8(n u64) u8 { return 1 << n }\n")
chains.append("fn lit_u64(n i8) u64 { return 1 << n }\n")
chains.append("fn pack4(a u8, b u8, c u8, d u8) u32 {\n")
chains.append("    return zext[u32](a) | (zext[u32](b) << 8) | (zext[u32](c) << 16) | (zext[u32](d) << 24)\n")
chains.append("}\n")
chains.append("fn use_const(sh u8) u8 { return sh << N }\n")
chains.append("fn use_local(sh u8) u8 { N u8 = 3\n  return sh << N }\n")
chains.append("fn main() i32 {\n")
chains.append("  x u8 = 1\n  a u8 = 4\n  b u8 = 4\n")
chains.append("  if x << a << b != 0 { return 1 }\n")
chains.append("  if x << (a + b) != 1 { return 2 }\n")
chains.append("  y u32 = 1\n")
chains.append("  if y << 33 != 2 { return 3 }\n")
chains.append("  if 1 + 2 << 3 != 24 { return 4 }\n")
chains.append("  if 1 << 2 + 1 != 8 { return 5 }\n")
chains.append("  if (0xF0 >> 4 & 3) != 3 { return 6 }\n")
chains.append("  if (1 << 2 < 5) != true { return 7 }\n")
chains.append("  if 8 >> 1 >> 1 != 2 { return 8 }\n")
chains.append("  n i64 = -1\n  z u64 = 1\n")
chains.append("  if z << n != 9223372036854775808 { return 9 }\n")
chains.append("  w i32 = -2\n")
chains.append("  if w >> n != -1 { return 10 }\n")
chains.append("  c u8 = 1\n  c <<= 33\n")
chains.append("  if c != 2 { return 11 }\n")
chains.append("  st Box2 = { 2147483648 }\n  st.slot2 >>= 33\n")
chains.append("  if st.slot2 != 1073741824 { return 12 }\n")
chains.append("  ar arr[2,u32]\n  copy(ar, G2)\n  ar[0] <<= 2\n")
chains.append("  if ar[0] != 0 { return 13 }\n")
chains.append("  v vec[4,u32] = splat(1)\n  v <<= 33\n")
chains.append("  if v[0] != 2 || v[3] != 2 { return 14 }\n")
chains.append("  v[1] <<= 33\n")
chains.append("  if v[1] != 4 { return 15 }\n")
chains.append("  counts vec[4,u32] = splat(0)\n  counts[0] = 1\n  counts[1] = 2\n  counts[2] = 33\n  counts[3] = 0\n  r vec[4,u32] = v << counts\n")
chains.append("  if r[0] != 4 || r[1] != 16 || r[2] != 4 || r[3] != 2 { return 16 }\n")
chains.append("  if use_const(3) != 128 { return 17 }\n")
chains.append("  if use_local(3) != 24 { return 18 }\n")
chains.append("  s i32 = 0\n  for i u8 = 1; i < 8; i <<= 1 { s += zext[i32](i) }\n")
chains.append("  if s != 7 { return 19 }\n")
chains.append("  if pack4(1, 2, 3, 4) != 0x04030201 { return 20 }\n")
chains.append("  if shg2[u8](3, 2) != 12 { return 21 }\n")
chains.append("  if shg2[i32](3, 2) != 12 { return 22 }\n")
chains.append("  cnt u64 = 33\n  zl u64 = 1 << cnt\n  if zl != 8589934592 { return 23 }\n")
chains.append("  if lit_u8(33) != 2 { return 24 }\n")
chains.append("  if lit_u64(-1) != 9223372036854775808 { return 25 }\n")
chains.append("  if shg2[u8](1, 8) != 1 { return 26 }\n")
chains.append("  rb vec[4,u32] = splat(1)\n  rc vec[4,u32] = rotl(rb, 1)\n  if rc[0] != 2 || rc[3] != 2 { return 27 }\n")
chains.append("  return 0\n}\n")
open(os.path.join(out, "chains.fas"), "w").write("".join(chains))
EOF

"$OCAML_FAS" --emit-llvm "$SHIFT_TMP/exhaustive8.fas" >"$SHIFT_TMP/exhaustive8.ll"
"$LLVM_OPT" -passes=verify "$SHIFT_TMP/exhaustive8.ll" -disable-output
grep -q 'and i8' "$SHIFT_TMP/exhaustive8.ll" || {
  echo "shift operator edges: missing count normalization" >&2
  exit 1
}
"$OCAML_FAS" --emit-llvm "$SHIFT_TMP/boundaries.fas" >"$SHIFT_TMP/boundaries.ll"
"$LLVM_OPT" -passes=verify "$SHIFT_TMP/boundaries.ll" -disable-output
"$OCAML_FAS" --emit-llvm "$SHIFT_TMP/chains.fas" >"$SHIFT_TMP/chains.ll"
"$LLVM_OPT" -passes=verify "$SHIFT_TMP/chains.ll" -disable-output
for ll in exhaustive8 boundaries chains; do
  if grep -E ' (nuw|nsw|exact) ' "$SHIFT_TMP/$ll.ll" >/dev/null; then
    echo "shift operator edges: unexpected shift flags in $ll.ll" >&2
    exit 1
  fi
  if grep -E 'shufflevector.*(undef|poison)' "$SHIFT_TMP/$ll.ll" >/dev/null; then
    echo "shift operator edges: undefined shuffle operand in $ll.ll" >&2
    exit 1
  fi
done

ulimit -c 0 || true
for level in 0 2 3; do
  for prog in exhaustive8 boundaries chains; do
    FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC" \
      "$OCAML_FAS" -O"$level" "$SHIFT_TMP/$prog.fas" -o "$SHIFT_TMP/$prog-$level"
    set +e
    timeout 30 "$SHIFT_TMP/$prog-$level"
    got=$?
    set -e
    if [ "$got" -ne 0 ]; then
      echo "shift operator edges: $prog at -O$level: want 0 got $got" >&2
      exit 1
    fi
  done
done

printf 'fn f(x u32, n u32) u32 { return shl(x, n) }\n' >"$SHIFT_TMP/bad1.fas"
set +e
"$OCAML_FAS" --emit-llvm "$SHIFT_TMP/bad1.fas" >"$SHIFT_TMP/bad1.ll" 2>"$SHIFT_TMP/bad1.err"
bad1=$?
set -e
if [ "$bad1" -eq 0 ] || ! grep -q "unknown function \`shl\`" "$SHIFT_TMP/bad1.err"; then
  echo "shift operator edges: removed builtin shl was not rejected as unknown" >&2
  exit 1
fi

badno=1
for badcase in 'fn f(x u32, y u32, n u32) u32 { return x < < y }' \
               'fn f(x u32, y u32) u32 { return (x <<= y) }' \
               'fn f(x u32, y u32) u32 { return x >>> y }' \
               'fn f(x u32) u32 { return x << }'; do
  badno=$((badno + 1))
  printf '%s\n' "$badcase" >"$SHIFT_TMP/bad$badno.fas"
  set +e
  "$OCAML_FAS" --emit-llvm "$SHIFT_TMP/bad$badno.fas" >"$SHIFT_TMP/bad$badno.ll" 2>"$SHIFT_TMP/bad$badno.err"
  bad=$?
  set -e
  if [ "$bad" -eq 0 ]; then
    echo "shift operator edges: malformed source accepted: $badcase" >&2
    exit 1
  fi
done

echo "shift operator edges: ok"
