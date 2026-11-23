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

# The streaming lexer must retain a token straddling a refill and must
# work on a pipe as well as a seekable file. "global" here introduces the
# next declaration, so it uses the parser's two-token lookahead.
python3 - <<'PY_FIR' > "$TMP/refill.fir"
prefix = "declare @unused () -> ()\n"
declaration = "global @value size 4 align 4 = { bytes \"\\2a\\00\\00\\00\" }\n"
part = prefix + ";" + " " * (65536 - len(prefix) - 4) + "\n" + declaration
part += "func @main () -> (i32) global {\n^entry:\n"
part += "  %v = load i32 ptr @value, align 4\n  ret %v\n}\n"
print(part, end="")
PY_FIR
"$FAS_FIR" -c "$TMP/refill.fir" -o "$TMP/refill-file.o"
cat "$TMP/refill.fir" | "$FAS_FIR" -c - -o "$TMP/refill-stdin.o"
cmp "$TMP/refill-file.o" "$TMP/refill-stdin.o"
echo "fir_stream: file and piped input emitted identical objects"

echo "=== cc_types: 3. Differential layout fuzz ($LAYOUT_CASES cases, seed=$SEED) ==="
"$FAS" "$ROOT/cc/test/layout_probe.fas" -o "$TMP/layout_probe"
FAS_CC_TYPES_CASES="$LAYOUT_CASES" SEED="$SEED" \
    python3 "$ROOT/test/cc_types/layout_fuzz.py" "$TMP/layout_probe"

echo "=== cc_types: 4. Bidirectional differential ABI fuzz ($ABI_CASES cases, seed=$SEED) ==="
"$FAS" "$ROOT/cc/test/abi_probe.fas" -o "$TMP/abi_probe"
FAS_CC_TYPES_ABI_CASES="$ABI_CASES" SEED="$SEED" \
    python3 "$ROOT/test/cc_types/abi_fuzz.py" "$TMP/abi_probe" "$FAS_FIR"

echo "=== cc_types: ALL PASSED ==="
