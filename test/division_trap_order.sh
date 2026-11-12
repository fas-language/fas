#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
DTO_TMP=$(mktemp -d)
trap 'rm -rf "$DTO_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/division_trap_order.fas" >"$DTO_TMP/order.ll"
"$LLVM_OPT" -passes=verify "$DTO_TMP/order.ll" -disable-output

printf '%s\n' "note 1" "note 0" >"$DTO_TMP/expected-2.log"
printf '%s\n' "note -1" >"$DTO_TMP/expected-3.log"
printf '%s\n' "note 0" >"$DTO_TMP/expected-4.log"
printf '%s\n' "note 10" "note -1" "note 11" "note 0" >"$DTO_TMP/expected-5.log"
printf '%s\n' "note 6" >"$DTO_TMP/expected-6.log"
printf '%s\n' "note 7" >"$DTO_TMP/expected-7.log"
printf '%s\n' "note 8" "note 88" >"$DTO_TMP/expected-8.log"

ulimit -c 0 || true
for level in 0 2; do
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" "$DTO_TMP/order.ll" \
    "$ROOT/test/division_trap_order.c" -o "$DTO_TMP/order-$level"
  for spec in 2:132 3:132 4:132 5:132 6:0 7:132 8:0; do
    want=${spec#*:}
    argc=${spec%%:*}
    set +e
    timeout 30 "$DTO_TMP/order-$level" $(seq 2 "$argc" 2>/dev/null) \
      2>"$DTO_TMP/order-$level-$argc.log"
    got=$?
    set -e
    if [ "$got" -ne "$want" ]; then
      echo "division trap order: -O$level argc $argc: want $want got $got" >&2
      exit 1
    fi
    sed -e '/^timeout: the monitored command dumped core$/d' -e '/^Illegal instruction/d' \
      "$DTO_TMP/order-$level-$argc.log" >"$DTO_TMP/order-$level-$argc.clean"
    if ! cmp -s "$DTO_TMP/expected-$argc.log" "$DTO_TMP/order-$level-$argc.clean"; then
      echo "division trap order: -O$level argc $argc: effect order mismatch" >&2
      diff -u "$DTO_TMP/expected-$argc.log" "$DTO_TMP/order-$level-$argc.clean" >&2 || true
      exit 1
    fi
  done
done

echo "division trap order: ok"
