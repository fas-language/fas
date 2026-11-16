#!/bin/sh
# Secrets (LANGUAGE.md "Secrets", roadmap v0.7): the source rules refuse
# each timing leak with its message, the accepted forms compile, and a
# secret function leaves no secret in its frame, the red zone or a
# caller-saved register (test/secrets/zeroize.c probes it), also with the
# allocator held to 3 registers so values spill. The built-in backend only:
# run_all.sh exports the LLVM oracle, which does not zeroize.
set -eu
unset FAS_ORACLE
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
SECRETS_TMP=$(mktemp -d)
trap 'rm -rf "$SECRETS_TMP"' EXIT HUP INT TERM
n=0

# refuse NAME EXPECTED-TEXT: the program on stdin fails with exit 1 and a
# message containing EXPECTED-TEXT
refuse() {
  cat >"$SECRETS_TMP/$1.fas"
  status=0
  "$OCAML_FAS" check "$SECRETS_TMP/$1.fas" >"$SECRETS_TMP/$1.log" 2>&1 || status=$?
  if [ "$status" != 1 ] || ! grep -F -- "$2" "$SECRETS_TMP/$1.log" >/dev/null; then
    echo "secrets: $1: expected exit 1 and \"$2\", got exit $status:" >&2
    cat "$SECRETS_TMP/$1.log" >&2
    exit 1
  fi
  n=$((n + 1))
}

# accept NAME: the program on stdin checks clean
accept() {
  cat >"$SECRETS_TMP/$1.fas"
  "$OCAML_FAS" check "$SECRETS_TMP/$1.fas" >"$SECRETS_TMP/$1.log" 2>&1 || {
    echo "secrets: $1: expected success:" >&2
    cat "$SECRETS_TMP/$1.log" >&2
    exit 1
  }
  n=$((n + 1))
}

refuse if '`if` branches, so its condition must be public' <<'F'
fn f(a secret[u32]) u32 { if a == 0 { return 1 }
 return 0 }
F
refuse for '`for` branches, so its condition must be' <<'F'
fn f(a secret[u32]) u32 { for i u32 = 0; i < a; i += 1 {}
 return 0 }
F
refuse andand '`&&` and `||` branch' <<'F'
fn f(a secret[bool], b bool) bool { return declassify(a && b) }
F
refuse switch '`switch` branches, so its value must be public' <<'F'
fn f(a secret[u32]) u32 { switch a { case 1: return 1
 default: return 0 } }
F
refuse ifexpr 'an if-expression branches' <<'F'
fn f(a secret[bool]) u32 { return if a { 1 } else { 0 } }
F
refuse index 'an index addresses memory, so it must be public' <<'F'
fn f(a secret[usize]) u32 { t arr[4, u32] = {}
 return t[a] }
F
refuse offset 'a secret offset would make a secret address' <<'F'
fn f(p addr, a secret[usize]) u32 { return p[u32, a] }
F
refuse div '`/` on a secret' <<'F'
fn f(a secret[u32], b u32) secret[u32] { return a / b }
F
refuse popcount '`popcount` on a secret has no constant-time' <<'F'
fn f(a secret[u32]) secret[u32] { return popcount(a) }
F
refuse lane 'lane indexing on a secret vector' <<'F'
fn f(a secret[vec[4, u32]]) secret[u32] { return a[0] }
F
refuse float '`secret[T]` needs an integer or `bool` type' <<'F'
fn f(a secret[f64]) void {}
F
refuse local 'a secret stored into the public local `x`' <<'F'
fn f(a secret[u32]) u32 { x u32 = a
 return x }
F
refuse result 'a secret returned as a public result' <<'F'
fn f(a secret[u32]) u32 { return a }
F
refuse global 'a secret stored into the global `g`' <<'F'
var g u32 = 0
fn f(a secret[u32]) void { g = a }
F
refuse access 'a secret stored into a public typed access' <<'F'
fn f(p addr, a secret[u32]) void { p[u32, 0] = a }
F
refuse arg 'argument 1 of `g` is secret, but the parameter' <<'F'
fn g(x u32) u32 { return x }
fn f(a secret[u32]) u32 { return g(a) }
F
refuse c_arg 'a secret passed to the C function `g`' <<'F'
extern "C" { fn g(x u32) u32 }
fn f(a secret[u32]) u32 { return g(a) }
F
refuse generic 'in `id[secret[u32]]`: a secret returned' <<'F'
fn id[T](x T) u32 { return x }
fn f(a secret[u32]) u32 { return id[secret[u32]](a) }
F
refuse inferred "a generic's type argument is inferred public" <<'F'
fn id[T](x T) T { return x }
fn f(a secret[u32]) u32 { return id(a) }
F
accept ops <<'F'
fn f(a secret[u32], b secret[u32]) secret[u32] {
    x secret[u32] = (a << b) | rotl(a, 3)
    return select(a < b, x, b) ^ ~a
}
F
accept declassify <<'F'
fn f(a secret[u32]) u32 { return declassify(a) }
F
accept raw <<'F'
fn f(p addr, a secret[u32]) secret[u32] { p[secret[u32], 0] = a
 return p[secret[u32], 4] }
F
accept export <<'F'
extern "C" { fn g(x secret[u32]) secret[u32] { return x + 1 } }
F
accept wipe <<'F'
fn f(p addr) void { wipe(p, 16) }
F

for regs in "" 3; do
  FAS_MIR_REGS=$regs "$OCAML_FAS" "$ROOT/test/secrets/zeroize.fas" "$ROOT/test/secrets/zeroize.c" -o "$SECRETS_TMP/z"
  "$SECRETS_TMP/z" >"$SECRETS_TMP/z.log" || {
    echo "secrets: zeroization (registers ${regs:-unlimited}) failed:" >&2
    cat "$SECRETS_TMP/z.log" >&2
    exit 1
  }
  n=$((n + 1))
done

echo "secrets: ok ($n checks)"
