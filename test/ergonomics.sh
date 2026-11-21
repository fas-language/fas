#!/bin/sh
# Language ergonomics (handles, type aliases, byte strings, keyword fields,
# if-expression literals, generic inference, generic fn forwarding, generic null)
# on both native and LLVM backends at O0 and O2, plus negative diagnostics.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ERGO_TMP=$(mktemp -d)
trap 'rm -rf "$ERGO_TMP"' EXIT HUP INT TERM

for oracle in native llvm; do
  for level in 0 2; do
    if [ "$oracle" = llvm ]; then
      FAS_ORACLE=llvm "$OCAML_FAS" -O"$level" "$ROOT/test/ergonomics.fas" -o "$ERGO_TMP/b"
    else
      env -u FAS_ORACLE "$OCAML_FAS" -O"$level" "$ROOT/test/ergonomics.fas" -o "$ERGO_TMP/b"
    fi
    status=0
    "$ERGO_TMP/b" || status=$?
    [ "$status" = 0 ] || {
      echo "ergonomics: $oracle -O$level failed check $status" >&2
      exit 1
    }
  done
done

expect_err() {
  name=$1
  want=$2
  src=$3
  printf '%s\n' "$src" > "$ERGO_TMP/$name.fas"
  if out=$("$OCAML_FAS" check "$ERGO_TMP/$name.fas" 2>&1); then
    echo "ergonomics: $name unexpectedly succeeded" >&2
    exit 1
  fi
  if ! printf '%s\n' "$out" | grep -Fq -- "$want"; then
    echo "ergonomics: $name missing expected diagnostic: $want" >&2
    echo "got: $out" >&2
    exit 1
  fi
}

# 1. Mismatched struct handle from &x
expect_err mismatch_handle \
  "value for \`h\` is \`addr\` to \`A\`, expected \`handle[B]\`" \
  'struct A { x u64 }
struct B { x u64 }
fn f() void { a A
 h handle[B] = &a }'

# 2. handle_from_addr without expected handle type
expect_err handle_no_expected \
  "\`handle_from_addr\` needs a type argument: \`handle_from_addr[T](p)\`" \
  'struct A { x u64 }
fn f(p addr) addr { return handle_from_addr(p) }'

# 3. Cyclic type aliases & parameterized alias & duplicate alias
expect_err alias_cycle_mutual \
  "cyclic type alias \`A\`" \
  'type A = B
type B = A
fn main() i32 { return 0 }'

expect_err alias_cycle_self \
  "cyclic type alias \`A\`" \
  'type A = handle[A]
fn main() i32 { return 0 }'

expect_err alias_generic_params \
  "type aliases cannot have generic parameters" \
  'type A[T] = T'

expect_err alias_duplicate \
  "duplicate type alias \`A\`" \
  'type A = u64
type A = u32'

# 6. Byte string length mismatch, wrong element type, standalone use
expect_err bstr_len_mismatch \
  "array of 4 elements, got 3" \
  'const M arr[4, u8] = b"abc"'

expect_err bstr_wrong_elem \
  "byte string initializer requires \`arr[N, u8]\`, got \`arr[_, u16]\`" \
  'const M arr[4, u16] = b"abcd"'

expect_err bstr_standalone \
  "byte string literal can only initialize \`arr[N, u8]\` or be passed to \`len\`" \
  'fn f() addr { return b"abc" }'

# 7. Builtin name still rejected as local
expect_err kw_local_rejected \
  "\`len\` is a builtin and cannot be used as a name" \
  'fn f() void { len u64 = 1 }'

echo "ergonomics: ok"
