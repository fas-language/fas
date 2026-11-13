#!/bin/sh
# Fas value structs: `value struct` values passed, returned and assigned by
# value, with fails, and across the C boundary (the SysV small-struct ABI,
# checked from both sides against clang). Every program runs on the native
# backend and on the hidden LLVM test oracle (removal A3, Cli.oracle); both
# must print the same thing.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=$(printenv CC || printf 'clang-22')
OCAML_FAS=$(printenv OCAML_FAS || printf '%s/_build/default/bin/main.exe' "$ROOT")
export FAS_CC="$CC"
VS_TMP=$(mktemp -d)
trap 'rm -rf "$VS_TMP"' EXIT HUP INT TERM

fail() {
  echo "value_structs: $*" >&2
  exit 1
}

# run NAME EXPECTED_EXIT FILE... : build with both backends, compare output.
run() {
  name=$1
  want=$2
  shift 2
  for backend in native llvm; do
    if [ "$backend" = llvm ]; then
      FAS_ORACLE=llvm "$OCAML_FAS" -o "$VS_TMP/$name-$backend" "$@"
    else
      env -u FAS_ORACLE "$OCAML_FAS" -o "$VS_TMP/$name-$backend" "$@"
    fi
    set +e
    "$VS_TMP/$name-$backend" >"$VS_TMP/$name-$backend.out"
    got=$?
    set -e
    [ "$got" -eq "$want" ] ||
      fail "$name ($backend) exited $got, expected $want"
  done
  cmp "$VS_TMP/$name-native.out" "$VS_TMP/$name-llvm.out" ||
    fail "$name: native and llvm output differ"
}

cd "$ROOT/test"

run value_structs 0 value_structs.fas
cat >"$VS_TMP/value_structs.want" <<'WANT'
add 1 2 111 44 100 22
pair 2 1 1.5 0.5
box 3 4 9
holder 3 4 5
arr 7 8 14 16
raw 11 12
zero 0 1 0 0
pick 1 111
if 2
span hello
res Neg 0
owned 1 3
fails 4 9 9 9
big 41 -1
loop 6 4
indirect 2 4
WANT
cmp "$VS_TMP/value_structs-native.out" "$VS_TMP/value_structs.want" ||
  fail "value_structs.fas printed the wrong output"

# The C ABI, both ways: C calls Fas, Fas calls C, plain and fails results.
run value_structs_abi 0 value_structs_abi.fas value_structs_abi.c
[ "$(cat "$VS_TMP/value_structs_abi-native.out")" = "c_drive 0 ck 0" ] ||
  fail "value_structs_abi printed the wrong output"

# Rejections: each program must fail `check` with the given message.
reject() {
  name=$1
  message=$2
  printf '%s\nfn main() i32 { return 0 }\n' "$3" >"$VS_TMP/$name.fas"
  if "$OCAML_FAS" check "$VS_TMP/$name.fas" >"$VS_TMP/$name.diag" 2>&1; then
    fail "$name: accepted"
  fi
  grep -F -q -- "$message" "$VS_TMP/$name.diag" ||
    { cat "$VS_TMP/$name.diag" >&2; fail "$name: wrong diagnostic"; }
}

reject too_big 'value struct `B` is 24 bytes; a value struct is at most 16' \
  'value struct B { a i64, b i64, c i8 }'
reject array_field 'field `a` of value struct `B` has type `arr[2, i32]`' \
  'value struct B { a arr[2, i32] }'
reject vec_field 'field `a` of value struct `B` has type `vec[4, f32]`' \
  'value struct B { a vec[4, f32] }'
reject plain_field 'field `p` of value struct `B` has type `P`' \
  'struct P { a i32 }
value struct B { p P }'
reject generic_field 'field `a` of value struct `W[arr[4, i64]]` has type `arr[4, i64]`' \
  'value struct W[T] { a T }
fn f(w W[arr[4, i64]]) i32 { return 0 }'
reject no_equality 'operator `==` does not accept `P`' \
  'value struct P { a i32 }
fn g(x P, y P) bool { return x == y }'
reject plain_param 'aggregate parameter `p` of type `P` cannot be passed by value' \
  'struct P { a i32 }
fn f(p P) i32 { return p.a }'
reject plain_result 'aggregate result `P` cannot be returned by value' \
  'struct P { a i32 }
fn f() P { return {1} }'
reject plain_fails 'a `fails` function cannot return `P`' \
  'struct P { a i32 }
fn f() P fails { return {1} }'
reject untyped_brace 'initializer needs an array, struct, or vector type, got `addr`' \
  'value struct P { a i32 }
fn f() i32 { x addr = {1}
 return 0 }'

# `value` is a keyword only right before `struct`.
cat >"$VS_TMP/value_name.fas" <<'FAS'
fn main() i32 { value i32 = 3
 return value }
FAS
"$OCAML_FAS" check "$VS_TMP/value_name.fas" ||
  fail "value_name: \`value\` as a local name was refused"

echo "value_structs: ok"
