#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/widening.fas" >"$TMP/input.ll"
"$LLVM_OPT" -passes=verify "$TMP/input.ll" -disable-output

cat >"$TMP/implicit.fas" <<'EOF'
fn widen_unsigned(value u8) i16 { return value }
fn widen_signed(value i8) i64 { return value }
fn mixed_value(left u32, right i64) i64 { return left + right }
fn call_value(value u8) i64 { return consume(value) }
fn consume(value i64) i64 { return value }
fn branch_zero(flag bool, narrow u16, wide u32) u64 { return zext[u64](if flag { narrow } else { wide }) }
fn branch_sign(flag bool, narrow i16, wide i32) i64 { return sext[i64](if flag { narrow } else { wide }) }
EOF
cat >"$TMP/explicit.fas" <<'EOF'
fn widen_unsigned(value u8) i16 { return zext[i16](value) }
fn widen_signed(value i8) i64 { return sext[i64](value) }
fn mixed_value(left u32, right i64) i64 { return zext[i64](left) + right }
fn call_value(value u8) i64 { return consume(zext[i64](value)) }
fn consume(value i64) i64 { return value }
fn branch_zero(flag bool, narrow u16, wide u32) u64 { return zext[u64](if flag { zext[u32](narrow) } else { wide }) }
fn branch_sign(flag bool, narrow i16, wide i32) i64 { return sext[i64](if flag { sext[i32](narrow) } else { wide }) }
EOF
"$OCAML_FAS" --emit-llvm "$TMP/implicit.fas" >"$TMP/implicit.ll"
"$OCAML_FAS" --emit-llvm "$TMP/explicit.fas" >"$TMP/explicit.ll"
"$LLVM_OPT" -passes=verify "$TMP/implicit.ll" -disable-output
"$LLVM_OPT" -passes=verify "$TMP/explicit.ll" -disable-output
cmp -s "$TMP/implicit.ll" "$TMP/explicit.ll" || {
  echo "widening: implicit and explicit LLVM differ" >&2
  exit 1
}

cat >"$TMP/branches.fas" <<'EOF'
extern "C" {
  fn widening_branch_report(zero_low u64, zero_high u64, sign_low i64, sign_high i64) void
  fn widening_chain_report(u8_a u64, u16_b u64, u32_c u64, u16_d u64,
                           reverse_u8_a u64, reverse_u16_b u64,
                           reverse_u32_c u64, reverse_u16_d u64,
                           i8_a i64, i16_b i64, i32_c i64, i16_d i64,
                           reverse_i8_a i64, reverse_i16_b i64,
                           reverse_i32_c i64, reverse_i16_d i64) void
}
fn choose_unsigned(flag bool, narrow u16, wide u32) u64 {
  return zext[u64](if flag { narrow } else { wide })
}
fn choose_signed(flag bool, narrow i16, wide i32) i64 {
  return sext[i64](if flag { narrow } else { wide })
}
fn choose_unsigned_chain(c bool, k bool, m bool, a u8, b u16, w u32, x u16) u64 {
  return zext[u64](if c { a } else if k { b } else if m { w } else { x })
}
fn choose_unsigned_chain_reversed(c bool, k bool, m bool, a u8, b u16, w u32, x u16) u64 {
  return zext[u64](if !c { if !k { if !m { x } else { w } } else { b } } else { a })
}
fn choose_signed_chain(c bool, k bool, m bool, a i8, b i16, w i32, x i16) i64 {
  return sext[i64](if c { a } else if k { b } else if m { w } else { x })
}
fn choose_signed_chain_reversed(c bool, k bool, m bool, a i8, b i16, w i32, x i16) i64 {
  return sext[i64](if !c { if !k { if !m { x } else { w } } else { b } } else { a })
}
fn main() i32 {
  widening_branch_report(choose_unsigned(true, 0, 0), choose_unsigned(true, 65535, 0),
                         choose_signed(true, -32768, 0), choose_signed(true, 32767, 0))
  widening_chain_report(
    choose_unsigned_chain(true, false, false, 255, 65535, 4294967295, 32768),
    choose_unsigned_chain(false, true, false, 255, 65535, 4294967295, 32768),
    choose_unsigned_chain(false, false, true, 255, 65535, 4294967295, 32768),
    choose_unsigned_chain(false, false, false, 255, 65535, 4294967295, 32768),
    choose_unsigned_chain_reversed(true, false, false, 255, 65535, 4294967295, 32768),
    choose_unsigned_chain_reversed(false, true, false, 255, 65535, 4294967295, 32768),
    choose_unsigned_chain_reversed(false, false, true, 255, 65535, 4294967295, 32768),
    choose_unsigned_chain_reversed(false, false, false, 255, 65535, 4294967295, 32768),
    choose_signed_chain(true, false, false, -128, -32768, -2147483648, 32767),
    choose_signed_chain(false, true, false, -128, -32768, -2147483648, 32767),
    choose_signed_chain(false, false, true, -128, -32768, -2147483648, 32767),
    choose_signed_chain(false, false, false, -128, -32768, -2147483648, 32767),
    choose_signed_chain_reversed(true, false, false, -128, -32768, -2147483648, 32767),
    choose_signed_chain_reversed(false, true, false, -128, -32768, -2147483648, 32767),
    choose_signed_chain_reversed(false, false, true, -128, -32768, -2147483648, 32767),
    choose_signed_chain_reversed(false, false, false, -128, -32768, -2147483648, 32767))
  return 0
}
EOF
"$OCAML_FAS" --emit-llvm "$TMP/branches.fas" >"$TMP/branches.ll"
"$LLVM_OPT" -passes=verify "$TMP/branches.ll" -disable-output

for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/input.ll" \
    -o "$TMP/input.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/input.O$level.ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/branches.ll" \
    -o "$TMP/branches.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/branches.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$TMP/fas.O$level" \
    "$ROOT/test/widening.fas" "$ROOT/test/widening_oracle.c"
  "$OCAML_FAS" -O"$level" -o "$TMP/branches.O$level" \
    "$TMP/branches.fas" "$ROOT/test/widening_oracle.c"
  "$CC" -Werror -std=c17 -O"$level" -DWIDENING_ORACLE \
    "$ROOT/test/widening_oracle.c" -o "$TMP/oracle.O$level"
  "$CC" -Werror -std=c17 -O"$level" -DWIDENING_BRANCH_ORACLE \
    "$ROOT/test/widening_oracle.c" -o "$TMP/branch-oracle.O$level"
  timeout 30 "$TMP/fas.O$level" >"$TMP/fas.O$level.out"
  timeout 30 "$TMP/branches.O$level" >"$TMP/branches.O$level.out"
  timeout 30 "$TMP/oracle.O$level" >"$TMP/oracle.O$level.out"
  timeout 30 "$TMP/branch-oracle.O$level" >"$TMP/branch-oracle.O$level.out"
  cmp -s "$TMP/fas.O$level.out" "$TMP/oracle.O$level.out" || {
    echo "widening: Fas and C results differ at O$level" >&2
    exit 1
  }
  cmp -s "$TMP/branches.O$level.out" "$TMP/branch-oracle.O$level.out" || {
    echo "widening: unified branches and C results differ at O$level" >&2
    exit 1
  }
done

echo "widening: branch zero/sign fill at boundaries, C oracle, mixed comparisons and LLVM equivalence at O0/O2: ok"
