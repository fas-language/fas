#!/bin/sh
# Stack probes (src/backend/mir/mir_emit.ml, roadmap v0.9): rsp never drops
# more than a page below the last touched address, so a big frame or a big
# dynamic allocation faults on the guard page instead of stepping over it.
# test/stack_probes/probes.fas runs a thread on a stack with mapped memory
# below its guard page: unprobed code survives there (checked when the
# probes went in), probed code must die with SIGSEGV.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
FAS_FIR_TOOL=${FAS_FIR_TOOL:-$ROOT/_build/default/tools/fas_fir/fas_fir.exe}
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT HUP INT TERM
fail=0

"$FAS_FIR_TOOL" -c "$ROOT/test/stack_probes/vla.fir" -o "$T/vla.o"
"$OCAML_FAS" "$ROOT/test/stack_probes/probes.fas" "$T/vla.o" -o "$T/probes"

# run MODE EXPECTED_STATUS [EXPECTED_STDOUT]
run() {
  status=0
  # an inner shell waits for it, so its "Segmentation fault" report goes
  # to /dev/null; `; exit $?` keeps it from exec'ing the program directly
  sh -c '"$@" >"$0"; exit $?' "$T/out" "$T/probes" "$1" 2>/dev/null || status=$?
  if [ "$status" -ne "$2" ]; then
    echo "native stack probes: $1 exited $status, expected $2" >&2
    fail=1
  fi
  if [ $# -ge 3 ] && [ "$(cat "$T/out")" != "$3" ]; then
    echo "native stack probes: $1 printed '$(cat "$T/out")', expected '$3'" >&2
    fail=1
  fi
}

ulimit -c 0 2>/dev/null || true
run small 0 "survived 30"
run frame 139
run vla 139

[ "$fail" -eq 0 ] && echo "native stack probes: ok"
exit "$fail"
