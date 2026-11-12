#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
LLVM_DWARFDUMP=${LLVM_DWARFDUMP:-llvm-dwarfdump-22}
RENDER=${DEBUG_INFO_RENDER:-$ROOT/_build/default/test/debug_info_render.exe}
DEBUG_TMP=$(mktemp -d)
trap 'rm -rf "$DEBUG_TMP"' EXIT HUP INT TERM

"$RENDER" >"$DEBUG_TMP/debug.ll"
"$LLVM_OPT" -passes=verify -disable-output "$DEBUG_TMP/debug.ll"
"$LLVM_OPT" -passes='default<O2>' -S "$DEBUG_TMP/debug.ll" -o "$DEBUG_TMP/debug_o2.ll"
"$LLVM_OPT" -passes=verify -disable-output "$DEBUG_TMP/debug_o2.ll"
"$LLVM_LLC" -O0 -filetype=obj "$DEBUG_TMP/debug.ll" -o "$DEBUG_TMP/debug.o"
"$LLVM_LLC" -filetype=obj "$DEBUG_TMP/debug_o2.ll" -o "$DEBUG_TMP/debug_o2.o"
"$LLVM_DWARFDUMP" --debug-line "$DEBUG_TMP/debug.o" >"$DEBUG_TMP/lines.txt"
"$LLVM_DWARFDUMP" --debug-line "$DEBUG_TMP/debug_o2.o" >/dev/null

grep -q 'name: "main.fas"' "$DEBUG_TMP/lines.txt"
grep -q 'name: "util.fas"' "$DEBUG_TMP/lines.txt"
awk '
  $1 ~ /^0x/ { rows[$4 " " $2] = 1 }
  END {
    n = split("0 3 0 4 0 6 1 10 1 11 1 12 1 13 1 14", want, " ")
    for (i = 1; i < n; i += 2) {
      key = want[i] " " want[i + 1]
      if (!(key in rows)) { print "missing line row " key; failed = 1 }
    }
    exit failed
  }
' "$DEBUG_TMP/lines.txt"
