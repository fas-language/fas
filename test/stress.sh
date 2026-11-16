#!/bin/sh
# Large-input stress test (roadmap v0.7 hardening): test/stress.py builds
# huge programs (1M instructions, 100k blocks, a 10k-case switch, 100k
# functions, big globals, 4096 lanes, Fas nested 100k deep) at 1x and 10x,
# and checks they compile, run correctly and scale roughly linearly.
# Not part of `dune runtest` or run_all.sh (it takes minutes):
#   sh test/stress.sh [FAMILY...]
set -eu
unset FAS_ORACLE
here=$(cd "$(dirname "$0")" && pwd)
exec python3 "$here/stress.py" \
  "${FAS_FIR_TOOL:-$here/../_build/default/tools/fas_fir/fas_fir.exe}" \
  "${OCAML_FAS:-$here/../_build/default/bin/main.exe}" "$@"
