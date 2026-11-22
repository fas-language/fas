#!/usr/bin/env bash
set -euo pipefail
ulimit -f 524288

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FAS="${FAS:-$ROOT/_build/default/bin/main.exe}"
FAS_FIR="${FAS_FIR:-$ROOT/_build/default/tools/fas_fir/fas_fir.exe}"
export FAS_CC="${FAS_CC:-clang-22}"
export CC="$FAS_CC"

LAYOUT_CASES="${FAS_CC_TYPES_CASES:-2000}"
ABI_CASES="${FAS_CC_TYPES_ABI_CASES:-1000}"
SEED="${SEED:-12345}"

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

echo "=== cc_types: 1. Unit tests (types, layout, abi) ==="
"$FAS" "$ROOT/cc/test/types_test.fas" -o "$TMP/types_test"
"$TMP/types_test"

echo "=== cc_types: 2. Fir 0.3 smoke test (fir_out -> fas-fir -> run) ==="
"$FAS" "$ROOT/cc/test/fir_smoke.fas" -o "$TMP/fir_smoke"
"$TMP/fir_smoke" > "$TMP/smoke.fir"
"$FAS_FIR" "$TMP/smoke.fir" -o "$TMP/smoke_bin"
"$TMP/smoke_bin"
echo "fir_smoke: passed"

echo "=== cc_types: 3. Differential layout fuzz ($LAYOUT_CASES cases, seed=$SEED) ==="
"$FAS" "$ROOT/cc/test/layout_probe.fas" -o "$TMP/layout_probe"
FAS_CC_TYPES_CASES="$LAYOUT_CASES" SEED="$SEED" \
    python3 "$ROOT/test/cc_types/layout_fuzz.py" "$TMP/layout_probe"

echo "=== cc_types: 4. Bidirectional differential ABI fuzz ($ABI_CASES cases, seed=$SEED) ==="
"$FAS" "$ROOT/cc/test/abi_probe.fas" -o "$TMP/abi_probe"
FAS_CC_TYPES_ABI_CASES="$ABI_CASES" SEED="$SEED" \
    python3 "$ROOT/test/cc_types/abi_fuzz.py" "$TMP/abi_probe" "$FAS_FIR"

echo "=== cc_types: ALL PASSED ==="
