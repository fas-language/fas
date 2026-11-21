#!/bin/sh
# Thread-local globals (`thread var`): per-thread isolation, initializers, &x,
# freestanding (static-PIE + static), hosted (native + LLVM O0/O2), and
# -shared/PIC (dlopen via clang -shared and fas-ld -shared), plus diagnostics.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
CC=${CC:-clang-22}
TLS_TMP=$(mktemp -d)
trap 'rm -rf "$TLS_TMP"' EXIT HUP INT TERM

# 1. Native freestanding static-PIE (default)
env -u FAS_ORACLE "$OCAML_FAS" "$ROOT/test/thread_local.fas" -o "$TLS_TMP/tls_pie"
"$TLS_TMP/tls_pie"

# 2. Native freestanding static (local-exec)
env -u FAS_ORACLE "$OCAML_FAS" "$ROOT/test/thread_local.fas" -Wl,-static -o "$TLS_TMP/tls_static"
"$TLS_TMP/tls_static"

# 3. Native hosted PIE
env -u FAS_ORACLE "$OCAML_FAS" --hosted "$ROOT/test/thread_local.fas" -o "$TLS_TMP/tls_hosted"
"$TLS_TMP/tls_hosted"

# 4. LLVM oracle hosted at O0 and O2
for level in 0 2; do
  FAS_ORACLE=llvm "$OCAML_FAS" -O"$level" "$ROOT/test/thread_local.fas" -o "$TLS_TMP/tls_llvm_$level"
  "$TLS_TMP/tls_llvm_$level"
done

# 5. Shared library (-c -fPIC + dlopen from multi-threaded C host)
# Test both native -c -fPIC and LLVM -c, linked by clang -shared AND fas-ld -shared
"$CC" -O2 "$ROOT/test/thread_local_shared_host.c" -ldl -pthread -o "$TLS_TMP/host"

env -u FAS_ORACLE "$OCAML_FAS" -c -fPIC "$ROOT/test/thread_local_shared.fas" -o "$TLS_TMP/dso_nat.o"
readelf -r "$TLS_TMP/dso_nat.o" | grep -q "R_X86_64_TLSGD" || {
  echo "thread_local: native -c -fPIC did not emit R_X86_64_TLSGD" >&2
  exit 1
}
"$CC" -shared "$TLS_TMP/dso_nat.o" -o "$TLS_TMP/libtls_nat_cc.so"
"$TLS_TMP/host" "$TLS_TMP/libtls_nat_cc.so"

# Direct -Wl,-shared via native OCaml linker
env -u FAS_ORACLE "$OCAML_FAS" --freestanding "$ROOT/test/thread_local_shared.fas" -Wl,-shared -o "$TLS_TMP/libtls_nat_ocaml.so"
"$TLS_TMP/host" "$TLS_TMP/libtls_nat_ocaml.so"

# Link the same -c -fPIC object with fas-ld -shared
env -u FAS_ORACLE "$OCAML_FAS" "$ROOT/ld/main.fas" -o "$TLS_TMP/fas-ld"
"$TLS_TMP/fas-ld" -shared "$TLS_TMP/dso_nat.o" -o "$TLS_TMP/libtls_nat_fasld.so"
"$TLS_TMP/host" "$TLS_TMP/libtls_nat_fasld.so"

# LLVM oracle -c -> clang -shared + dlopen
FAS_ORACLE=llvm "$OCAML_FAS" -c "$ROOT/test/thread_local_shared.fas" -o "$TLS_TMP/dso_llvm.o"
"$CC" -shared "$TLS_TMP/dso_llvm.o" -o "$TLS_TMP/libtls_llvm.so"
"$TLS_TMP/host" "$TLS_TMP/libtls_llvm.so"

# 6. Negative diagnostics
expect_err() {
  name=$1
  want=$2
  src=$3
  printf '%s\n' "$src" > "$TLS_TMP/$name.fas"
  if out=$("$OCAML_FAS" check "$TLS_TMP/$name.fas" 2>&1); then
    echo "thread_local: $name unexpectedly succeeded" >&2
    exit 1
  fi
  if ! printf '%s\n' "$out" | grep -Fq -- "$want"; then
    echo "thread_local: $name missing expected diagnostic: $want" >&2
    echo "got: $out" >&2
    exit 1
  fi
}

expect_err tls_addr_in_const \
  "cannot take the address of thread-local \`t\` in a constant initializer" \
  'thread var t u64 = 1
var p addr = &t'

expect_err tls_addr_in_tls_init \
  "cannot take the address of thread-local \`t\` in a constant initializer" \
  'thread var t u64 = 1
thread var p addr = &t'

expect_err thread_non_var \
  "\`thread\` only applies to \`var\` declarations" \
  'thread fn f() void {}'

expect_err tls_nonconst_init \
  "global initializer must be a constant expression" \
  'fn g() u64 { return 1 }
thread var t u64 = g()'

echo "thread_local: ok"
