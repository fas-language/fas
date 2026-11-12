#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
CFT_TMP=$(mktemp -d)
trap 'rm -rf "$CFT_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/control_flow_timing.fas" >"$CFT_TMP/timing.ll"
"$LLVM_OPT" -passes=verify "$CFT_TMP/timing.ll" -disable-output

printf '%s\n' "note 1" "note 11" "note 9" >"$CFT_TMP/expected-2.log"
printf '%s\n' "note 3" "note 9" >"$CFT_TMP/expected-3.log"
printf '%s\n' "note 70" "note 10" "note 100" "note 1" "note 70" "note 11" "note 21" \
  "note 101" "note 1" "note 70" "note 12" "note 22" "note 102" "note 1" "note 70" \
  "note 9" >"$CFT_TMP/expected-4.log"
printf '%s\n' "note 70" "note 10" "note 100" "note 70" "note 10" "note 100" "note 70" \
  "note 10" "note 100" "note 70" "note 9" >"$CFT_TMP/expected-5.log"
printf '%s\n' "note 10" "note 100" "note 9" >"$CFT_TMP/expected-6.log"
printf '%s\n' "note 2" >"$CFT_TMP/expected-7.log"
printf '%s\n' "note 2" "note 0" >"$CFT_TMP/expected-8.log"
printf '%s\n' "note 2" "note 0" >"$CFT_TMP/expected-9.log"
printf '%s\n' "note 10" "note 100" "note 9" >"$CFT_TMP/expected-10.log"

ulimit -c 0 || true
for level in 0 2; do
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" "$CFT_TMP/timing.ll" \
    "$ROOT/test/control_flow_timing.c" -o "$CFT_TMP/timing-$level"
  for spec in 2:0 3:0 4:0 5:0 6:0 7:0 8:132 9:132 10:0; do
    want=${spec#*:}
    argc=${spec%%:*}
    set +e
    timeout 30 "$CFT_TMP/timing-$level" $(seq 2 "$argc" 2>/dev/null) \
      2>"$CFT_TMP/timing-$level-$argc.log"
    got=$?
    set -e
    if [ "$got" -ne "$want" ]; then
      echo "control flow timing: -O$level argc $argc: want $want got $got" >&2
      exit 1
    fi
    sed -e '/^timeout: the monitored command dumped core$/d' -e '/^Illegal instruction/d' \
      "$CFT_TMP/timing-$level-$argc.log" >"$CFT_TMP/timing-$level-$argc.clean"
    if ! cmp -s "$CFT_TMP/expected-$argc.log" "$CFT_TMP/timing-$level-$argc.clean"; then
      echo "control flow timing: -O$level argc $argc: trace mismatch" >&2
      diff -u "$CFT_TMP/expected-$argc.log" "$CFT_TMP/timing-$level-$argc.clean" >&2 || true
      exit 1
    fi
  done
done

echo "control flow timing: ok"
