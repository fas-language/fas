#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
CHACHA_TMP=$(mktemp -d)
trap 'rm -rf "$CHACHA_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/chacha20/mc_chacha20.fas" >"$CHACHA_TMP/chacha20.ll"
"$LLVM_OPT" -passes=verify "$CHACHA_TMP/chacha20.ll" -disable-output

ulimit -c 0 || true
for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$CHACHA_TMP/chacha20.ll" \
    -o "$CHACHA_TMP/chacha20-$level.ll"
  "$LLVM_OPT" -passes=verify "$CHACHA_TMP/chacha20-$level.ll" -disable-output
  "$CC" -Werror -Wno-override-module -x ir -c -O"$level" \
    "$CHACHA_TMP/chacha20-$level.ll" -o "$CHACHA_TMP/fas-$level.o"
  "$CC" -Werror -std=c17 -O"$level" \
    -c "$ROOT/test/chacha20.c" -o "$CHACHA_TMP/harness-$level.o"
  "$CC" -Werror -std=c17 -O"$level" \
    -Dmc_chacha20_block=c_mc_chacha20_block \
    -Dmc_chacha20_init=c_mc_chacha20_init \
    -Dmc_chacha20_update=c_mc_chacha20_update \
    -Dmc_chacha20_keystream_update=c_mc_chacha20_keystream_update \
    -Dmc_chacha20_wipe_ctx=c_mc_chacha20_wipe_ctx \
    -Dmc_chacha20_xor=c_mc_chacha20_xor \
    -Dmc_chacha20_keystream=c_mc_chacha20_keystream \
    -Dmc_chacha20_wipe=c_mc_chacha20_wipe \
    -c "$ROOT/test/chacha20/mc_chacha20.c" -o "$CHACHA_TMP/ref-$level.o"
  "$CC" -Werror "$CHACHA_TMP/fas-$level.o" \
    "$CHACHA_TMP/harness-$level.o" "$CHACHA_TMP/ref-$level.o" \
    -o "$CHACHA_TMP/chacha20-$level"
  if [ "$level" -eq 2 ]; then
    fas_text=$(${SIZE:-size} -A "$CHACHA_TMP/fas-$level.o" | awk '$1 == ".text" {sum += $2} END {print sum + 0}')
    c_text=$(${SIZE:-size} -A "$CHACHA_TMP/ref-$level.o" | awk '$1 == ".text" {sum += $2} END {print sum + 0}')
    echo "chacha20 O2 .text size: Fas $fas_text bytes, C reference $c_text bytes"
    timeout 120 "$CHACHA_TMP/chacha20-$level" --measure
  else
    timeout 120 "$CHACHA_TMP/chacha20-$level"
  fi
done

echo "chacha20: O0/O2 verified and linked: ok"
