#!/bin/sh
# `const assert`: test/const_assert.fas compiles and runs (every assertion
# holds); each failing case below is a compile error with the expected first
# line, and a failure inside a generic names the specialisation.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
CA_TMP=$(mktemp -d)
trap 'rm -rf "$CA_TMP"' EXIT HUP INT TERM

cp "$ROOT/test/const_assert.fas" "$CA_TMP/ok.fas"
(cd "$CA_TMP" && "$OCAML_FAS" ok.fas -o ok)
"$CA_TMP/ok" || { echo "const_assert: ok.fas exited $?" >&2; exit 1; }

# fails <name> <expected stderr substring>; the program is on stdin
fails() {
  name=$1 want=$2
  cat >"$CA_TMP/$name.fas"
  if (cd "$CA_TMP" && "$OCAML_FAS" "$name.fas" -o "$name") 2>"$CA_TMP/$name.err"; then
    echo "const_assert: $name compiled, want an error" >&2
    exit 1
  fi
  grep -qF -- "$want" "$CA_TMP/$name.err" || {
    echo "const_assert: $name: want '$want' in:" >&2
    cat "$CA_TMP/$name.err" >&2
    exit 1
  }
}

fails top 'top.fas:2:1: error: const assert failed: N too small' <<'FAS'
const N usize = 2
const assert(N > 4, "N too small")
fn main() i32 { return 0 }
FAS

fails stmt 'stmt.fas:2:5: error: const assert failed' <<'FAS'
fn main() i32 {
    const assert(sizeof[u16] == 4)
    return 0
}
FAS

fails generic_const 'note: while instantiating `first[i32, 0]`' <<'FAS'
struct Arr[T, M const usize] { v arr[M, T] }
fn first[T, M const usize](a handle[Arr[T, M]]) T {
    const assert(M > 0, "empty array")
    return a.v[0]
}
fn main() i32 {
    a Arr[i32, 3] = {}
    b Arr[i32, 0] = {}
    x i32 = first[i32, 3](handle_from_addr[Arr[i32, 3]](&a))
    return x + first[i32, 0](handle_from_addr[Arr[i32, 0]](&b))
}
FAS
grep -qF 'const assert failed: empty array' "$CA_TMP/generic_const.err"
if grep -qF 'first[i32, 3]' "$CA_TMP/generic_const.err"; then
  echo "const_assert: the passing specialisation was reported" >&2
  exit 1
fi

fails generic_type 'note: while instantiating `w[u8]`' <<'FAS'
fn w[T]() usize {
    const assert(sizeof[T] == 4, "needs a 4-byte type")
    return sizeof[T]
}
fn main() i32 { return trunc[i32](w[i32]() + w[u8]()) }
FAS

fails runtime_local 'error: const assert condition is not a compile-time constant' <<'FAS'
fn main() i32 {
    x i32 = 3
    const assert(x > 0)
    return x
}
FAS

fails not_bool 'error: integer literal is not a `bool`' <<'FAS'
const assert(3)
fn main() i32 { return 0 }
FAS

fails message_literal 'error: a const assert message must be a string literal' <<'FAS'
const M usize = 1
const assert(M == 1, M)
fn main() i32 { return 0 }
FAS

echo "const_assert: ok"
