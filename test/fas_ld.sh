#!/bin/sh
# End-to-end differential tests for the Fas linker (ld/*.fas):
# 1. Builds fas-ld from ld/main.fas.
# 2. Runs every OCaml linker integration test in src/linker/test/dune through a
#    shadow wrapper comparing exit code, stdout, stderr (normalizing --stats
#    timing/GC fields and OCaml's random-hashtable missing-version-node choice),
#    and output bytes, varying FAS_LD_THREADS per invocation.
# 3. Runs a seeded dynamic/shared differential fuzz pass (default 300 cases;
#    override with FAS_LD_CASES=N and FAS_LD_SEED=S).
set -eu
ulimit -f 524288

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
OCAML_LD=${OCAML_LD:-$ROOT/_build/default/src/linker/bin/main.exe}
FAS_LD_CASES=${FAS_LD_CASES:-300}
FAS_LD_SEED=${FAS_LD_SEED:-20261121}

export FAS_CC=${FAS_CC:-clang-22}
export CC=${CC:-clang-22}
export FAS_OPT=${FAS_OPT:-opt-22}
export FAS_LLC=${FAS_LLC:-llc-22}
export LLVM_OPT=${LLVM_OPT:-opt-22}
export LLVM_LLC=${LLVM_LLC:-llc-22}
unset FAS_ORACLE

WORK=$(mktemp -d "${TMPDIR:-/tmp}/fas-ld-test.XXXXXX")
OK=0
cleanup() {
    if [ "$OK" = 1 ]; then
        rm -rf "$WORK"
    else
        echo "fas-ld: artifacts preserved in $WORK" >&2
    fi
}
trap cleanup EXIT HUP INT TERM

timeout 600 "$OCAML_FAS" -o "$WORK/fas-ld" "$ROOT/ld/main.fas"
timeout 600 "$OCAML_FAS" -o "$WORK/apitest" "$ROOT/ld/test/apitest.fas"

status=0

# In-process library API / VFS / re-entrancy / incremental session check
API_DIR="$WORK/api"
mkdir -p "$API_DIR"
reference_cc() {
    python3 "$ROOT/tools/refcache/compiler.py" fas-ld/api "$WORK" "$CC" "$@"
}
printf '.globl _start\n_start:\n  call answer\n  mov %%eax, %%edi\n  mov $60, %%eax\n  syscall\n' >"$API_DIR/start.s"
reference_cc -c "$API_DIR/start.s" -o "$API_DIR/start.o"
printf 'extern int helper(void);\nint answer(void) { return helper() + 1; }\n' >"$API_DIR/main.c"
printf 'int helper(void) { return 41; }\n' >"$API_DIR/helper.c"
printf 'int api_fn(void) { return 7; }\nint hidden_fn(void) { return 3; }\n' >"$API_DIR/shared.c"
printf 'V1 { global: api_fn; local: *; };\n' >"$API_DIR/ver.map"
reference_cc -c -fno-pic -fno-pie "$API_DIR/main.c" -o "$API_DIR/main.o"
reference_cc -c -fno-pic -fno-pie "$API_DIR/helper.c" -o "$API_DIR/helper.o"
ar rcs "$API_DIR/libfoo.a" "$API_DIR/helper.o"
reference_cc -c -fPIC "$API_DIR/shared.c" -o "$API_DIR/shared.o"
"$WORK/fas-ld" -o "$API_DIR/ref_static" "$API_DIR/start.o" "$API_DIR/main.o" -L"$API_DIR" -lfoo
"$WORK/fas-ld" -shared -o "$API_DIR/ref_shared.so" "--version-script=$API_DIR/ver.map" "$API_DIR/shared.o"
api_line=$("$WORK/apitest" "$API_DIR/ref_static" "$API_DIR/ref_shared.so" \
    "$API_DIR/start.o" "$API_DIR/main.o" "$API_DIR/libfoo.a" "$API_DIR/shared.o" "$API_DIR/ver.map") || status=1
echo "$api_line"

shadow_line=$(python3 "$ROOT/test/fas_ld/run_shadow.py" "$ROOT" "$WORK/shadow" "$WORK/fas-ld") || status=1
echo "$shadow_line"

fuzz_out="$WORK/fuzz.out"
if ! python3 "$ROOT/test/fas_ld/ldfuzz_dyn.py" "$OCAML_LD" "$WORK/fas-ld" "$WORK/fuzz" "$FAS_LD_SEED" "$FAS_LD_CASES" >"$fuzz_out"; then
    cat "$fuzz_out" >&2
    status=1
fi
fuzz_line=$(grep '^cases: ' "$fuzz_out" || true)
echo "fuzz: $fuzz_line"

if [ "$status" != 0 ]; then
    exit 1
fi

OK=1
echo "fas-ld: $api_line; $shadow_line; fuzz $fuzz_line: ok"
