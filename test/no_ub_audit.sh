#!/bin/sh
# C4 no-UB audit (roadmap v0.5): test/no_ub_audit.fas checks every rule
# that LANGUAGE.md makes defined where C has undefined behaviour (wrapping,
# shift counts modulo the width, saturating cvt, fresh memory, bool bytes,
# misaligned plain and atomic accesses, enum_is), and the programs below
# each hit one of the runtime traps, which must trap and nothing else.
# Both backends, at -O0 and -O2. The LLVM backend is reached through the
# hidden test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
fail() { echo "no_ub_audit: $*" >&2; exit 1; }

want='wraps=0 shifts=0 conversions=0 fresh=0 bools=0 misaligned=0 enums=0'
for be in native llvm; do
  for o in 0 2; do
    "$OCAML_FAS" --backend=$be -O$o -o "$TMP/audit" "$ROOT/test/no_ub_audit.fas"
    got=$("$TMP/audit")
    [ "$got" = "$want" ] || fail "$be -O$o: $got"
  done
done

# A5: atomics through an `addr` are `align 1`, through `&place` natural;
# A2: the LLVM path freezes every load. The program runs on both.
ir=$("$OCAML_FAS" --emit-ir "$ROOT/test/no_ub_atomic_align.fas")
echo "$ir" | grep -q 'atomic_rmw add i64 %v[0-9]*, i64 1, align 1 seq_cst' || fail "addr atomic not align 1"
echo "$ir" | grep -q 'atomic_rmw add i64 ptr @C, i64 1, align 8 seq_cst' || fail "&place atomic not align 8"
ll=$("$OCAML_FAS" --emit-llvm "$ROOT/test/no_ub_atomic_align.fas")
echo "$ll" | grep -q 'load atomic i64, ptr %v[0-9]* monotonic, align 1' || fail "LLVM addr atomic load not align 1"
loads=$(echo "$ll" | grep -c ' = load ' || true)
freezes=$(echo "$ll" | grep -c ' = freeze ' || true)
[ "$freezes" -ge "$loads" ] || fail "LLVM: $loads loads, only $freezes freezes"
for be in native llvm; do
  for o in 0 2; do
    "$OCAML_FAS" --backend=$be -O$o -o "$TMP/atomic" "$ROOT/test/no_ub_atomic_align.fas"
    status=0; "$TMP/atomic" || status=$?
    [ "$status" = 1 ] || fail "misaligned atomics ($be -O$o): status $status, want 1"
  done
done

# trap NAME BODY: BODY (statements of main, with the volatile inputs below)
# must end the program with a signal before it prints
traps=0
check_trap() {
  name=$1
  cat >"$TMP/$name.fas" <<EOF
use "C" <stdio.h> as c
var I8 arr[2, i8] = {-128, -1}
var I32 arr[3, i32] = {-2147483647 - 1, -1, 0}
var I64 arr[2, i64] = {-9223372036854775807 - 1, -1}
var U8 arr[2, u8] = {3, 9}
enum Tok u8 { A = 1, B = 2, C = 7 }
fn i8v(i u64) i8 { return volatile_load[i8](&I8 + i) }
fn i32v(i u64) i32 { return volatile_load[i32](&I32 + i * 4) }
fn i64v(i u64) i64 { return volatile_load[i64](&I64 + i * 8) }
fn u8v(i u64) u8 { return volatile_load[u8](&U8 + i) }
fn main() i32 {
$2
    c.printf(c"no trap\n")
    return 0
}
EOF
  for be in native llvm; do
    for o in 0 2; do
      "$OCAML_FAS" --backend=$be -O$o -o "$TMP/$name" "$TMP/$name.fas"
      status=0
      "$TMP/$name" >"$TMP/out" 2>/dev/null || status=$?
      [ "$status" -gt 128 ] || fail "$name ($be -O$o): status $status, want a trap"
      [ ! -s "$TMP/out" ] || fail "$name ($be -O$o): printed $(cat "$TMP/out")"
    done
  done
  traps=$((traps + 1))
}

check_trap div_zero '    x i32 = i32v(1) / i32v(2)
    c.printf(c"%d\n", x)'
check_trap rem_zero '    x i32 = i32v(1) % i32v(2)
    c.printf(c"%d\n", x)'
check_trap div_min32 '    x i32 = i32v(0) / i32v(1)
    c.printf(c"%d\n", x)'
check_trap div_min64 '    x i64 = i64v(0) / i64v(1)
    c.printf(c"%ld\n", x)'
check_trap div_min8 '    x i8 = i8v(0) / i8v(1)
    c.printf(c"%d\n", sext[i32](x))'
check_trap enum_from '    t Tok = enum_from[Tok](u8v(0))
    c.printf(c"%u\n", zext[u32](enum_value(t)))'
check_trap enum_name '    t Tok = (&U8 + 1)[Tok]
    c.printf(c"%s\n", enum_name(t))'
check_trap enum_switch '    t Tok = (&U8)[Tok]
    switch t {
        case Tok.A: c.printf(c"a\n")
        case Tok.B: c.printf(c"b\n")
        case Tok.C: c.printf(c"c\n")
    }'
check_trap lane_index '    v vec[4, u32] = splat(7)
    x u32 = v[zext[u64](u8v(1))]
    c.printf(c"%u\n", x)'

echo "no_ub_audit: defined-behaviour rules and $traps traps, native and LLVM at O0/O2: ok"
