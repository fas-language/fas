#!/bin/sh
# constants from calls: test/const_calls.fas compiles and runs (every value
# is right); each failing case below is a compile error with the expected
# text.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
CC_TMP=$(mktemp -d)
trap 'rm -rf "$CC_TMP"' EXIT HUP INT TERM

cp "$ROOT/test/const_calls.fas" "$CC_TMP/ok.fas"
(cd "$CC_TMP" && "$OCAML_FAS" ok.fas -o ok)
"$CC_TMP/ok" || { echo "const_calls: ok.fas exited $?" >&2; exit 1; }

# fails <name> <expected stderr substring>; the program is on stdin
fails() {
  name=$1 want=$2
  cat >"$CC_TMP/$name.fas"
  if (cd "$CC_TMP" && "$OCAML_FAS" "$name.fas" -o "$name") 2>"$CC_TMP/$name.err"; then
    echo "const_calls: $name compiled, want an error" >&2
    exit 1
  fi
  grep -qF -- "$want" "$CC_TMP/$name.err" || {
    echo "const_calls: $name: want '$want' in:" >&2
    cat "$CC_TMP/$name.err" >&2
    exit 1
  }
}

fails cycle 'error: constant `A` depends on itself through the functions its initializer calls' <<'FAS'
fn f() i32 { return A + 1 }
const A i32 = f()
fn main() i32 { return A }
FAS

fails trap 'trap.fas:2:15: error: constant `Z` fails at compile time: its evaluation divides by zero' <<'FAS'
fn d(x i32) i32 { return 10 / x }
const Z i32 = d(0)
fn main() i32 { return Z }
FAS

fails assert 'error: constant `P` fails at compile time: assert.fas:2: assertion failed: x must be positive' <<'FAS'
fn check(x i32) i32 {
    assert(x > 0, "x must be positive")
    return x
}
const P i32 = check(-1)
fn main() i32 { return P }
FAS

fails c_call 'its evaluation calls `atoi`, which has no Fir body (a C function)' <<'FAS'
use "C" <stdlib.h> as c
fn a() i32 { return c.atoi(c"3") }
const X i32 = a()
fn main() i32 { return X }
FAS

fails var_global 'its evaluation touches the variable global @counter' <<'FAS'
var counter i32 = 0
fn bump() i32 {
    counter += 1
    return counter
}
const X i32 = bump()
fn main() i32 { return X }
FAS

fails steps 'its evaluation runs for more than' <<'FAS'
fn spin() i32 {
    i i32 = 0
    while true { i += 1 }
    return i
}
const X i32 = spin()
fn main() i32 { return X }
FAS

fails const_assert 'error: const assert failed: fib(7) is 13' <<'FAS'
fn fib(n i32) i32 {
    if n < 2 { return n }
    return fib(n - 1) + fib(n - 2)
}
const assert(fib(7) == 14, "fib(7) is 13")
fn main() i32 { return 0 }
FAS

fails not_scalar 'must have an integer, bool, float or enum type' <<'FAS'
fn p() addr { return null }
const X addr = p()
fn main() i32 { return 0 }
FAS

echo "const_calls: ok"
