#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
REAL_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
SANITIZE_TMP=$(mktemp -d)
trap 'rm -rf "$SANITIZE_TMP"' EXIT HUP INT TERM
ulimit -Sv 4194304

fail() {
  printf 'sanitize: %s\n' "$*" >&2
  exit 1
}

verify_address_ir() {
  source=$1
  level=$2
  label=$3
  "$REAL_FAS" --sanitize=address --emit-llvm "$source" \
    >"$SANITIZE_TMP/$label.input.ll"
  "$LLVM_OPT" -passes=verify "$SANITIZE_TMP/$label.input.ll" -disable-output
  "$LLVM_OPT" -asan-use-after-scope -S "-passes=asan,default<O$level>" \
    "$SANITIZE_TMP/$label.input.ll" -o "$SANITIZE_TMP/$label.optimized.ll"
  "$LLVM_OPT" -passes=verify "$SANITIZE_TMP/$label.optimized.ll" -disable-output
}

run_asan_failure() {
  binary=$1
  expected=$2
  status=0
  (ulimit -Sv unlimited; ASAN_OPTIONS=detect_stack_use_after_return=1 "$binary") \
    >"$SANITIZE_TMP/run.out" 2>"$SANITIZE_TMP/run.err" || status=$?
  [ "$status" -ne 0 ] || fail "$expected program unexpectedly succeeded"
  grep -F "$expected" "$SANITIZE_TMP/run.err" >/dev/null \
    || fail "expected $expected; got: $(head -n 5 "$SANITIZE_TMP/run.err")"
}

for level in 0 2; do
  for case_name in oob scope return; do
    verify_address_ir "$ROOT/test/sanitize_$case_name.fas" "$level" "$case_name.O$level"
    "$REAL_FAS" --sanitize=address -O"$level" \
      "$ROOT/test/sanitize_$case_name.fas" -o "$SANITIZE_TMP/$case_name.O$level"
  done
  run_asan_failure "$SANITIZE_TMP/oob.O$level" stack-buffer-overflow
  run_asan_failure "$SANITIZE_TMP/scope.O$level" stack-use-after-scope
  run_asan_failure "$SANITIZE_TMP/return.O$level" stack-use-after-return
  verify_address_ir "$ROOT/test/sanitize_heap.fas" "$level" "heap.O$level"
  "$REAL_FAS" --sanitize=address -O"$level" "$ROOT/test/sanitize_heap.fas" \
    "$ROOT/test/sanitize_heap.c" -o "$SANITIZE_TMP/heap.O$level"
  run_asan_failure "$SANITIZE_TMP/heap.O$level" heap-buffer-overflow

  "$REAL_FAS" --sanitize=undefined --emit-llvm "$ROOT/test/sanitize_ubsan.fas" \
    >"$SANITIZE_TMP/ubsan.O$level.input.ll"
  "$LLVM_OPT" -passes=verify "$SANITIZE_TMP/ubsan.O$level.input.ll" -disable-output
  "$LLVM_OPT" -S "-passes=default<O$level>" "$SANITIZE_TMP/ubsan.O$level.input.ll" \
    -o "$SANITIZE_TMP/ubsan.O$level.optimized.ll"
  "$LLVM_OPT" -passes=verify "$SANITIZE_TMP/ubsan.O$level.optimized.ll" -disable-output
  "$REAL_FAS" --sanitize=undefined -O"$level" "$ROOT/test/sanitize_ubsan.fas" \
    -o "$SANITIZE_TMP/ubsan.O$level"
  (ulimit -Sv unlimited; "$SANITIZE_TMP/ubsan.O$level") \
    >"$SANITIZE_TMP/ubsan.O$level.out" 2>"$SANITIZE_TMP/ubsan.O$level.err"
  grep -F 'runtime error:' "$SANITIZE_TMP/ubsan.O$level.err" >/dev/null \
    || fail "C signed overflow did not report under UBSan at O$level"

  "$REAL_FAS" --sanitize=undefined --emit-llvm "$ROOT/test/sanitize_wrapping.fas" \
    >"$SANITIZE_TMP/wrapping.O$level.input.ll"
  "$LLVM_OPT" -passes=verify "$SANITIZE_TMP/wrapping.O$level.input.ll" -disable-output
  "$REAL_FAS" --sanitize=undefined -O"$level" "$ROOT/test/sanitize_wrapping.fas" \
    -o "$SANITIZE_TMP/wrapping.O$level"
  (ulimit -Sv unlimited; "$SANITIZE_TMP/wrapping.O$level") \
    >"$SANITIZE_TMP/wrapping.O$level.out" 2>"$SANITIZE_TMP/wrapping.O$level.err" \
    || fail "Fas wrapping arithmetic failed at O$level"
  [ ! -s "$SANITIZE_TMP/wrapping.O$level.out" ] \
    && [ ! -s "$SANITIZE_TMP/wrapping.O$level.err" ] \
    || fail "Fas wrapping arithmetic reported under UBSan at O$level"
done
printf 'sanitize: invalid cases and defined arithmetic passed at O0/O2\n'

FIXTURE_LOG=$SANITIZE_TMP/fixtures.log
cat >"$SANITIZE_TMP/fas-wrapper" <<'WRAPPER'
#!/bin/sh
set -eu
non_executable=0
output=
source=
level=default
next_output=0
for argument do
  if [ "$next_output" -eq 1 ]; then
    output=$argument
    next_output=0
    continue
  fi
  case "$argument" in
    -o) next_output=1 ;;
    -c|-S|--emit-llvm|--emit-ir|--emit-asm|--emit-obj|--emit-header) non_executable=1 ;;
    -O0) level=0 ;;
    -O2) level=2 ;;
    *.fas) [ -n "$source" ] || source=$argument ;;
  esac
done
if [ "$non_executable" -eq 1 ] || [ -z "$output" ]; then
  exec "$REAL_FAS" "$@"
fi
if "$REAL_FAS" --sanitize=address "$@"; then
  :
else
  status=$?
  exit "$status"
fi
if [ ! -f "$output" ]; then
  exit 0
fi
kind=$(readelf -h "$output" 2>/dev/null | sed -n 's/.*Type:[[:space:]]*\([^ ]*\).*/\1/p')
case "$kind" in
  DYN|EXEC)
    inner=$output.fas-asan
    mv "$output" "$inner"
    printf '%s|%s\n' "$source" "$level" >>"$FIXTURE_LOG"
    printf '#!/bin/sh\nulimit -Sv unlimited\nexec "%s" "$@"\n' "$inner" >"$output"
    chmod +x "$output"
    ;;
esac
WRAPPER
chmod +x "$SANITIZE_TMP/fas-wrapper"
export REAL_FAS FIXTURE_LOG ASAN_OPTIONS=detect_stack_use_after_return=1:detect_leaks=0

run_fixture_component() {
  component=$1
  printf 'sanitize: running %s\n' "$component"
  FAS_CC="$CC" FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" \
    OCAML_FAS="$SANITIZE_TMP/fas-wrapper" sh "$ROOT/test/$component.sh"
}

for component in \
  c_import c_records c_array_globals record_literals simd_value_builtins vector_literals \
  literal_aggregates record_attributes opaque_types generic_layout c_interop \
  defer_unwinding lexical_scopes generic_type_slots c_import_types packed_layouts; do
  run_fixture_component "$component"
done

for fixture in \
  test/c_import/program.fas test/c_records.fas test/c_import/arrays.fas \
  test/record_literals.fas test/simd_value_builtins.fas test/vector_literals.fas \
  test/literal_aggregates.fas test/record_attributes.fas test/opaque_types.fas \
  test/generic_layout.fas test/c_interop.fas test/defer_unwinding.fas \
  test/lexical_scopes.fas test/generic_type_slots.fas test/c_import_types.fas \
  test/packed_layouts.fas; do
  for level in 0 2; do
    grep -F "$ROOT/$fixture|$level" "$FIXTURE_LOG" >/dev/null \
      || fail "$fixture did not run under address sanitizer at O$level"
  done
done

fixture_count=$(cut -d'|' -f1 "$FIXTURE_LOG" | sort -u | wc -l | tr -d ' ')
[ "$fixture_count" -ge 15 ] || fail "only $fixture_count existing fixtures ran under ASan"
printf 'sanitize: %s valid fixtures passed at O0/O2\n' "$fixture_count"
