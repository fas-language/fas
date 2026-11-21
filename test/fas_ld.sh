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

status=0
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
echo "fas-ld: $shadow_line; fuzz $fuzz_line: ok"
