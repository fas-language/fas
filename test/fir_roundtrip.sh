#!/bin/sh
# The native backend tests with FAS_FIR=check (src/fir/fir_hook.ml): every
# lowered module is verified and must reparse from its printed text to the
# same text. Checks the Fir printer and parser on real programs.
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
FAS_FIR=check bash "$ROOT/test/native_backend.sh"
echo "fir roundtrip: ok"
