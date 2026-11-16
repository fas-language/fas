#!/bin/sh
# `const { }` blocks: test/const_blocks.fas compiles and runs (every filled
# value is right); each failing case below is a compile error with the
# expected text.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
CB_TMP=$(mktemp -d)
trap 'rm -rf "$CB_TMP"' EXIT HUP INT TERM

cp "$ROOT/test/const_blocks.fas" "$CB_TMP/ok.fas"
(cd "$CB_TMP" && "$OCAML_FAS" ok.fas -o ok)
"$CB_TMP/ok" || { echo "const_blocks: ok.fas exited $?" >&2; exit 1; }

# fails <name> <expected stderr substring>; the program is on stdin
fails() {
  name=$1 want=$2
  cat >"$CB_TMP/$name.fas"
  if (cd "$CB_TMP" && "$OCAML_FAS" "$name.fas" -o "$name") 2>"$CB_TMP/$name.err"; then
    echo "const_blocks: $name compiled, want an error" >&2
    exit 1
  fi
  grep -qF -- "$want" "$CB_TMP/$name.err" || {
    echo "const_blocks: $name: want '$want' in:" >&2
    cat "$CB_TMP/$name.err" >&2
    exit 1
  }
}

fails return 'return.fas:3:5: error: a `const { }` block cannot `return`; the block of `T` ends at its closing brace' <<'FAS'
const T arr[2, u8] = const {
    T[0] = 1
    return
}
fn main() i32 { return 0 }
FAS

fails catch_return 'error: a `const { }` block cannot `return`' <<'FAS'
error Bad
fn may() u8 fails { fail Bad }
const T arr[2, u8] = const {
    T[0] = may() catch { return }
}
fn main() i32 { return 0 }
FAS

fails var_global 'var_global.fas:2:22: error: constant `T` cannot be evaluated at compile time: its evaluation touches the variable' <<'FAS'
var g u8 = 3
const T arr[2, u8] = const {
    T[0] = g
}
fn main() i32 { return 0 }
FAS

fails out_of_range 'error: constant `T` cannot be evaluated at compile time: its evaluation accesses 1 bytes' <<'FAS'
const T arr[2, u8] = const {
    for i usize in 0..3 { T[i] = 1 }
}
fn main() i32 { return 0 }
FAS

fails assert 'error: constant `T` fails at compile time:' <<'FAS'
const T arr[2, u8] = const {
    assert(T[0] == 5, "first must be five")
}
fn main() i32 { return 0 }
FAS
grep -qF 'assertion failed: first must be five' "$CB_TMP/assert.err"

fails address 'error: constant `T` cannot be compile-time data: byte 0 holds an address' <<'FAS'
const T arr[2, addr] = const {
    T[0] = &T
}
fn main() i32 { return 0 }
FAS

fails steps 'runs for more than 10000000 steps' <<'FAS'
const T arr[1, u64] = const {
    while true { T[0] += 1 }
}
fn main() i32 { return 0 }
FAS

fails cycle 'depends on itself through the functions its initializer calls' <<'FAS'
fn peek() u8 { return T[0] }
const N u8 = peek()
const T arr[2, u8] = const {
    T[0] = N
}
fn main() i32 { return 0 }
FAS

fails body_error 'note: while evaluating constant `T` at compile time' <<'FAS'
const T arr[2, u8] = const {
    T[0] = 300
}
fn main() i32 { return 0 }
FAS

# only the block writes the constant: a function is part of the program,
# where the constant is read-only
fails helper_writes 'cannot modify constant `T`' <<'FAS'
fn put(v u8) void { T[0] = v }
const T arr[2, u8] = const {
    put(4)
}
fn main() i32 { return 0 }
FAS

fails c_call 'cannot be evaluated at compile time' <<'FAS'
use "C" <stdlib.h> as c
const T arr[1, i32] = const {
    T[0] = c.atoi(c"7")
}
fn main() i32 { return 0 }
FAS

echo "const_blocks: ok"
