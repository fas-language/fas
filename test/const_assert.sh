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

fails runtime_local 'error: const assert condition uses the local `x`, which has no compile-time value' <<'FAS'
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

# statement-level assertions that call functions run in the interpreter, per
# specialisation (the passing ones are in const_assert.fas)
fails call_generic 'note: while instantiating `work[u64]`' <<'FAS'
fn sz[T]() usize { return sizeof[T] }
fn work[T](x T) T {
    const assert(sz[T]() <= 4, "too wide")
    return x
}
fn main() i32 {
    a u32 = work[u32](1)
    b u64 = work[u64](2)
    return 0
}
FAS
grep -qF 'const assert failed: too wide' "$CA_TMP/call_generic.err"
if grep -qF 'work[u32]' "$CA_TMP/call_generic.err"; then
  echo "const_assert: call_generic blamed work[u32]" >&2
  exit 1
fi

fails call_local 'const assert condition uses the local `n`' <<'FAS'
fn id(x i32) i32 { return x }
fn main() i32 {
    n i32 = 3
    const assert(id(n) == 3)
    return 0
}
FAS

# function generic arguments: each instance asserts with its own argument
fails call_fn_generic 'while instantiating `best[gt64][u64]`' <<'FAS'
fn lt32(a u32, b u32) bool { return a < b }
fn gt64(a u64, b u64) bool { return a > b }
fn best[T, LESS fn(T, T) bool](a T, b T) T {
    const assert(LESS(bitcast[T](1), bitcast[T](2)))
    if LESS(a, b) { return a }
    return b
}
fn main() i32 {
    if best[u32, lt32](3, 4) != 3 { return 1 }
    if best[u64, gt64](3, 4) != 4 { return 2 }
    return 0
}
FAS

fails call_cycle 'this const assert calls a function that contains it' <<'FAS'
fn f() i32 {
    const assert(f() == 1)
    return 1
}
fn main() i32 { return f() - 1 }
FAS

fails call_var 'touches the variable global' <<'FAS'
var G i32 = 0
fn g() i32 { return G }
fn main() i32 {
    const assert(g() == 0)
    return 0
}
FAS

fails call_trap 'this const assert fails at compile time: its evaluation divides by zero' <<'FAS'
fn d(x i32) i32 { return 10 / x }
fn main() i32 {
    const assert(d(0) == 1)
    return 0
}
FAS

echo "const_assert: ok"
