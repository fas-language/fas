#!/bin/sh
# The Fir conformance programs of fas_fir.sh through the machine-IR backend
# (fas-fir --mir, src/backend/mir).
set -eu
FAS_FIR_FLAGS=--mir exec sh "$(dirname "$0")/fas_fir.sh"
