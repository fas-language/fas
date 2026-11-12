#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
PROBE_TMP=$(mktemp -d)
trap 'rm -rf "$PROBE_TMP"' EXIT HUP INT TERM

"$OCAML_FAS" --emit-llvm "$ROOT/test/lowering_probes.fas" >"$PROBE_TMP/probes.ll"
"$LLVM_OPT" -passes=verify "$PROBE_TMP/probes.ll" -disable-output
"$OCAML_FAS" --emit-llvm "$ROOT/test/defined_construction.fas" >"$PROBE_TMP/defined.ll"
"$LLVM_OPT" -passes=verify "$PROBE_TMP/defined.ll" -disable-output
if grep -E '\b(poison|undef)\b' "$PROBE_TMP/defined.ll" >/dev/null; then
  echo "lowering probes: defined construction emitted poison or undef" >&2
  exit 1
fi

copy_ir=$(awk '/^define void @lowering_copy_stack\(/,/^}/' "$PROBE_TMP/probes.ll")
scratch_count=$(printf '%s\n' "$copy_ir" | grep -Ec 'alloca \[(512 x i64|4096 x i8)\]' || true)
if [ "$scratch_count" -ne 1 ]; then
  echo "lowering probes: expected one 4 KiB copy scratch alloca, found $scratch_count" >&2
  exit 1
fi
if ! printf '%s\n' "$copy_ir" | awk '
  /^define / { in_function = 1; next }
  in_function && /^}/ { exit (alloca_count == 1 ? 0 : 1) }
  in_function && /^  br / { branched = 1 }
  in_function && /alloca \[(512 x i64|4096 x i8)\]/ {
    if (branched) exit 1
    alloca_count++
  }
  END { if (in_function && !alloca_count) exit 1 }
'; then
  echo "lowering probes: copy scratch alloca was not in the entry block" >&2
  exit 1
fi

for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$PROBE_TMP/probes.ll" \
    -o "$PROBE_TMP/probes.O$level.ll"
  "$LLVM_OPT" -passes=verify "$PROBE_TMP/probes.O$level.ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O$level>" "$PROBE_TMP/defined.ll" \
    -o "$PROBE_TMP/defined.O$level.ll"
  "$LLVM_OPT" -passes=verify "$PROBE_TMP/defined.O$level.ll" -disable-output
  if grep -E '\b(poison|undef)\b' "$PROBE_TMP/defined.O$level.ll" >/dev/null; then
    echo "lowering probes: O$level defined construction emitted poison or undef" >&2
    exit 1
  fi
  "$CC" -Werror -Wno-override-module -std=c17 -O"$level" \
    "$PROBE_TMP/probes.O$level.ll" "$ROOT/test/lowering_probes.c" \
    -o "$PROBE_TMP/probes.O$level"
  timeout 120 "$PROBE_TMP/probes.O$level"
done

"$LLVM_LLC" -O2 "$PROBE_TMP/probes.O2.ll" -o "$PROBE_TMP/probes.O2.s"
load_asm=$(awk '/^lowering_load32_le:/{in_function=1} in_function {print} in_function && /^\.Lfunc_end0:/{exit}' "$PROBE_TMP/probes.O2.s")
load_count=$(printf '%s\n' "$load_asm" | grep -Ec 'movl[[:space:]]+\(%rdi\), %eax' || true)
if [ "$load_count" -ne 1 ]; then
  echo "lowering probes: byte assembly no longer uses one safe 32-bit load" >&2
  exit 1
fi

div_ir=$(awk '/^define i64 @lowering_div_invariant\(/,/^}/' "$PROBE_TMP/probes.O2.ll")
guard_line=$(printf '%s\n' "$div_ir" | grep -n 'icmp eq i32 %a2, 0' | head -n 1 | cut -d: -f1)
loop_line=$(printf '%s\n' "$div_ir" | grep -n 'phi i64' | head -n 1 | cut -d: -f1)
if [ -z "$guard_line" ] || [ -z "$loop_line" ] || [ "$guard_line" -ge "$loop_line" ]; then
  echo "lowering probes: invariant division guard is not before the loop recurrence" >&2
  exit 1
fi

recurrence_ir=$(awk '/^define void @lowering_add_recurrence\(/,/^}/' "$PROBE_TMP/probes.O2.ll")
if ! printf '%s\n' "$recurrence_ir" | grep -q 'phi i64' ||
   ! printf '%s\n' "$recurrence_ir" | grep -Eq 'add (nuw )?i64 .*1'; then
  echo "lowering probes: loop induction recurrence disappeared" >&2
  exit 1
fi

printf 'lowering probes: recurrence, widening, guards, conditional loads, defined construction, copy scratch: ok\n'
