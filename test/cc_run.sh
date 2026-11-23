#!/usr/bin/env bash
set -euo pipefail
ulimit -f 524288
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
export FAS_CC="${FAS_CC:-clang-22}"
export CC="${CC:-clang-22}"
export OCAML_FAS="${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}"
export FCC="${FCC:-$ROOT/_build/fcc}"
export FCC_FIR="${FCC_FIR:-$ROOT/_build/default/tools/fas_fir/fas_fir.exe}"
export FCC_LD="${FCC_LD:-$ROOT/_build/fas-ld}"
export FCC_INCLUDE="${FCC_INCLUDE:-$ROOT/cc/include}"

if [ ! -x "$FCC" ]; then
    "$OCAML_FAS" "$ROOT/cc/main.fas" -o "$FCC"
fi
if [ ! -x "$FCC_LD" ]; then
    "$OCAML_FAS" "$ROOT/ld/main.fas" -o "$FCC_LD"
fi

case "${1:-all}" in
    --manifest)
        shift
        exec python3 "$ROOT/test/cc_run/run_manifest.py" "$@"
        ;;
    --projects)
        shift
        exec python3 "$ROOT/test/cc_run/test_projects.py" "$@"
        ;;
    --random)
        shift
        exec python3 "$ROOT/test/cc_run/rand_c.py" "$@"
        ;;
    --csmith)
        shift
        exec python3 "$ROOT/test/cc_run/csmith_diff.py" "$@"
        ;;
    --abi)
        exec bash "$ROOT/test/cc_run/ms_abi/check.sh"
        ;;
    --flags)
        exec bash "$ROOT/test/cc_run/flags.sh"
        ;;
    --regressions)
        exec bash "$ROOT/test/cc_run/regressions/check.sh"
        ;;
    all)
        echo "=== 1. Manifest run-agree differential tests (fcc vs clang-22) ==="
        python3 "$ROOT/test/cc_run/run_manifest.py" --show 15
        echo ""
        echo "=== 2. Real-world differential projects ==="
        python3 "$ROOT/test/cc_run/test_projects.py"
        echo ""
        echo "=== 3. Random UB-free C differential fuzzer (100 programs) ==="
        python3 "$ROOT/test/cc_run/rand_c.py" --count 100
        echo ""
        echo "=== 4. Microsoft x64 ABI interoperability ==="
        bash "$ROOT/test/cc_run/ms_abi/check.sh"
        echo ""
        echo "=== 5. Directed compiler regressions ==="
        bash "$ROOT/test/cc_run/regressions/check.sh"
        bash "$ROOT/test/cc_run/flags.sh"
        ;;
    *)
        exec python3 "$ROOT/test/cc_run/run_manifest.py" "$@"
        ;;
esac
