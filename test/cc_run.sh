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

exec python3 "$ROOT/test/cc_run/run_manifest.py" "$@"
