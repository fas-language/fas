#!/bin/sh
# Fas enums: test/enums.fas on the native backend and on the hidden LLVM
# test oracle (both must print the same thing), the traps of a value that
# is no member, the C header export, and the rejections.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=$(printenv CC || printf 'clang-22')
OCAML_FAS=$(printenv OCAML_FAS || printf '%s/_build/default/bin/main.exe' "$ROOT")
export FAS_CC="$CC"
EN_TMP=$(mktemp -d)
trap 'rm -rf "$EN_TMP"' EXIT HUP INT TERM

fail() {
  echo "enums: $*" >&2
  exit 1
}

# run NAME EXPECTED_EXIT FILE... : build with both backends, compare output.
run() {
  name=$1
  want=$2
  shift 2
  for backend in native llvm; do
    if [ "$backend" = llvm ]; then
      FAS_ORACLE=llvm "$OCAML_FAS" -o "$EN_TMP/$name-$backend" "$@"
    else
      env -u FAS_ORACLE "$OCAML_FAS" -o "$EN_TMP/$name-$backend" "$@"
    fi
    set +e
    "$EN_TMP/$name-$backend" >"$EN_TMP/$name-$backend.out" 2>/dev/null
    got=$?
    set -e
    [ "$got" -eq "$want" ] ||
      fail "$name ($backend) exited $got, expected $want"
  done
  cmp "$EN_TMP/$name-native.out" "$EN_TMP/$name-llvm.out" ||
    fail "$name: native and llvm output differ"
}

cd "$ROOT/test"

run enums 0 enums.fas
cat >"$EN_TMP/enums.want" <<'WANT'
prec 10 10 -1
value 11 4 10 11
name Minus Eof
is 1 0 1
sig -3 Neg 1 0
sig_is 1 0
sparse 1 0 3 B
big 1 Huge 0
small Lo 0
node Plus Pos
pair Ident Zero
table Plus 1 1
fails Plus Eof
WANT
cmp "$EN_TMP/enums-native.out" "$EN_TMP/enums.want" ||
  fail "enums.fas printed the wrong output"

# A value that is no member traps (SIGILL): `enum_from` of a runtime
# integer, and a switch without `default` or `enum_name` over memory that
# holds a non-member (written as raw bytes, as C could).
cat >"$EN_TMP/from_trap.fas" <<'FAS'
enum Tok u8 { A, B, C = 7 }
fn main(argc i32, argv addr) i32 {
    t Tok = enum_from[Tok](trunc[u8](argc) + 4)
    return zext[i32](enum_value(t))
}
FAS
cat >"$EN_TMP/switch_trap.fas" <<'FAS'
enum Tok u8 { A, B, C = 7 }
fn f(t Tok) i32 {
    switch t {
        case Tok.A: return 1
        case Tok.B: return 2
        case Tok.C: return 3
    }
    return 0
}
fn main() i32 {
    t Tok = Tok.A
    volatile_store[u8](&t, 5)
    return f(t)
}
FAS
cat >"$EN_TMP/name_trap.fas" <<'FAS'
extern "C" { fn puts(s addr) i32 }
enum Tok u8 { A, B, C = 7 }
fn main() i32 {
    t Tok = Tok.A
    volatile_store[u8](&t, 5)
    puts(enum_name(t))
    return 0
}
FAS
run from_trap 132 "$EN_TMP/from_trap.fas"
run switch_trap 132 "$EN_TMP/switch_trap.fas"
run name_trap 132 "$EN_TMP/name_trap.fas"

# C sees an enum as its integer type plus one #define per member.
cat >"$EN_TMP/export.fas" <<'FAS'
enum Tok u8 { Eof, Ident, Plus = 10 }
enum Sig i32 { Neg = -3, Pos = 3 }
extern "C" {
    fn tok_next(t Tok) Tok { return if t == Tok.Eof { Tok.Plus } else { Tok.Eof } }
    fn sig_of(s Sig) i32 { return enum_value(s) }
}
FAS
cat >"$EN_TMP/export_use.c" <<'C'
#include "export.h"
#include <stdio.h>
int main(void) {
  printf("%d %d %d\n", tok_next(Tok_Eof), sig_of(Sig_Neg), (int)sizeof(Tok));
  return Tok_Plus == 10 && Sig_Pos == 3 ? 0 : 1;
}
C
"$OCAML_FAS" --emit-header -o "$EN_TMP/export.h" "$EN_TMP/export.fas"
grep -F -q 'typedef uint8_t Tok;' "$EN_TMP/export.h" || fail "header lacks the Tok typedef"
grep -F -q '#define Sig_Neg ((Sig)(-3))' "$EN_TMP/export.h" || fail "header lacks Sig_Neg"
"$OCAML_FAS" -c -o "$EN_TMP/export.o" "$EN_TMP/export.fas"
"$CC" -std=c23 -I"$EN_TMP" -o "$EN_TMP/export_use" "$EN_TMP/export_use.c" "$EN_TMP/export.o"
[ "$("$EN_TMP/export_use")" = "10 -3 1" ] || fail "C consumer printed the wrong output"

# Rejections: each program must fail `check` with the given message.
reject() {
  name=$1
  message=$2
  printf '%s\nfn main() i32 { return 0 }\n' "$3" >"$EN_TMP/$name.fas"
  if "$OCAML_FAS" check "$EN_TMP/$name.fas" >"$EN_TMP/$name.diag" 2>&1; then
    fail "$name: accepted"
  fi
  grep -F -q -- "$message" "$EN_TMP/$name.diag" ||
    { cat "$EN_TMP/$name.diag" >&2; fail "$name: wrong diagnostic"; }
}

reject no_type 'an enum needs its underlying integer type' 'enum Tok { A }'
reject float_type "an enum's underlying type must be an integer type" 'enum Tok f32 { A }'
reject empty 'enum `Tok` has no members' 'enum Tok u8 { }'
reject dup_name 'duplicate member `A` in enum `Tok`' 'enum Tok u8 { A, B, A }'
reject dup_value '`Tok.A` and `Tok.B` have the same value 1' 'enum Tok u8 { A = 1, B = 1 }'
reject no_fit '`Tok.A` = 300 does not fit `u8`' 'enum Tok u8 { A = 300 }'
reject count_past '`Tok.B` counts past the largest `u8`; give it a value' 'enum Tok u8 { A = 255, B }'
reject int_to_enum 'an integer literal is not a `Tok`' 'enum Tok u8 { A, B }
fn f() Tok { return 1 }'
reject enum_to_int 'return value is `Tok`, expected `u8`' 'enum Tok u8 { A, B }
fn f(t Tok) u8 { return t }'
reject arithmetic 'operator `+` needs integer or integer vector operands, got `Tok`' 'enum Tok u8 { A, B }
fn f(t Tok) Tok { return t + Tok.A }'
reject cross_enum 'operands of `==` have different types: `Tok` and `Op`' 'enum Tok u8 { A, B }
enum Op u8 { X }
fn f(t Tok) bool { return t == Op.X }'
reject no_member 'enum `Tok` has no member `Q`' 'enum Tok u8 { A, B }
fn f() Tok { return Tok.Q }'
reject non_exhaustive 'switch on enum `Tok` does not handle `Tok.B`' 'enum Tok u8 { A, B }
fn f(t Tok) i32 { switch t { case Tok.A: return 1 }
 return 0 }'
reject dup_case 'duplicate case label `Tok.A`' 'enum Tok u8 { A, B }
fn f(t Tok) i32 { switch t { case Tok.A, Tok.B: return 1
 case Tok.A: return 2 }
 return 0 }'
reject const_from '5 is not a value of enum `Tok`' 'enum Tok u8 { A, B }
fn f() Tok { return enum_from[Tok](5) }'
reject from_operand '`enum_from` needs a `u8` (the enum'"'"'s integer type), got `u32`' 'enum Tok u8 { A, B }
fn f(n u32) Tok { return enum_from[Tok](n) }'
reject zext_enum 'illegal `zext` from `Tok` to `u16`' 'enum Tok u8 { A, B }
fn f(t Tok) u16 { return zext[u16](t) }'
reject vector 'vector element type must be `bool` or an integer type' 'enum Tok u8 { A, B }
var v vec[4, Tok]'
reject local_name '`Tok` is an enum and cannot be used as a local name' 'enum Tok u8 { A, B }
fn f() i32 { Tok i32 = 3
 return Tok }'
reject value_of_int '`enum_value` needs an enum, got `i32`' 'fn f(n i32) i32 { return enum_value(n) }'

# `enum` is a keyword only at the start of an item, before a name.
cat >"$EN_TMP/enum_name.fas" <<'FAS'
fn main() i32 { enum i32 = 3
 return enum }
FAS
"$OCAML_FAS" check "$EN_TMP/enum_name.fas" ||
  fail "enum_name: \`enum\` as a local name was refused"

echo "enums: ok"
