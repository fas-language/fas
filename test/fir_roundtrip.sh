#!/bin/sh
# The native backend tests, compiled through Ir -> Fir -> verify -> Ir
# (src/fir/fir_hook.ml). Checks the Fir converters on real programs.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
FAS_FIR=roundtrip bash "$ROOT/test/native_backend.sh"
echo "fir roundtrip: ok"
