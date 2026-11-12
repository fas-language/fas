#!/bin/sh
# --sanitize=undefined is trap-mode UB checking of C code (removal A6): C
# blocks and .c inputs are compiled with clang's -fsanitize=undefined
# -fsanitize-trap=all, the checks of the Fir SPEC section 11 table, and no
# sanitizer runtime is linked. A caught program dies by SIGILL (ud2). Fas
# arithmetic is defined (it wraps), so Fas code is unchanged. Both the
# built-in backend (the default) and the hidden LLVM oracle are checked.
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
REAL_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
export FAS_CC=${FAS_CC:-$CC}
SANITIZE_TMP=$(mktemp -d)
trap 'rm -rf "$SANITIZE_TMP"' EXIT HUP INT TERM

fail() {
  printf 'sanitize: %s\n' "$*" >&2
  exit 1
}

# run BINARY: sets $status to its exit status, output discarded
run() {
  status=0
  "$1" >"$SANITIZE_TMP/run.out" 2>"$SANITIZE_TMP/run.err" || status=$?
}

cat >"$SANITIZE_TMP/overflow.c" <<'C'
int c_overflow(int x) { return x + 1; }
C
cat >"$SANITIZE_TMP/overflow.fas" <<'FAS'
extern "C" { fn c_overflow(x i32) i32 }
fn main() i32 {
  c_overflow(2147483647)
  return 0
}
FAS

for oracle in "" llvm; do
  label=${oracle:-native}
  for level in 0 2; do
    tag=$label.O$level
    # a C block
    FAS_ORACLE=$oracle "$REAL_FAS" --sanitize=undefined -O"$level" \
      "$ROOT/test/sanitize_ubsan.fas" -o "$SANITIZE_TMP/ubsan.$tag"
    run "$SANITIZE_TMP/ubsan.$tag"
    [ "$status" -eq 132 ] || fail "C block overflow did not trap ($tag, exit $status)"
    if nm "$SANITIZE_TMP/ubsan.$tag" 2>/dev/null | grep -F __ubsan_ >/dev/null; then
      fail "trap mode linked the UBSan runtime ($tag)"
    fi
    FAS_ORACLE=$oracle "$REAL_FAS" -O"$level" "$ROOT/test/sanitize_ubsan.fas" \
      -o "$SANITIZE_TMP/plain.$tag"
    run "$SANITIZE_TMP/plain.$tag"
    [ "$status" -eq 0 ] || fail "unsanitized C block failed ($tag, exit $status)"

    # a .c input
    FAS_ORACLE=$oracle "$REAL_FAS" --sanitize=undefined -O"$level" \
      "$SANITIZE_TMP/overflow.fas" "$SANITIZE_TMP/overflow.c" -o "$SANITIZE_TMP/input.$tag"
    run "$SANITIZE_TMP/input.$tag"
    [ "$status" -eq 132 ] || fail ".c input overflow did not trap ($tag, exit $status)"

    # Fas wrapping arithmetic is not undefined
    FAS_ORACLE=$oracle "$REAL_FAS" --sanitize=undefined -O"$level" \
      "$ROOT/test/sanitize_wrapping.fas" -o "$SANITIZE_TMP/wrapping.$tag"
    run "$SANITIZE_TMP/wrapping.$tag"
    [ "$status" -eq 0 ] && [ ! -s "$SANITIZE_TMP/run.out" ] && [ ! -s "$SANITIZE_TMP/run.err" ] \
      || fail "Fas wrapping arithmetic trapped or failed ($tag, exit $status)"
  done
  status=0
  FAS_ORACLE=$oracle "$REAL_FAS" --sanitize=address "$ROOT/test/sanitize_wrapping.fas" \
    -o "$SANITIZE_TMP/address" 2>"$SANITIZE_TMP/address.err" || status=$?
  [ "$status" -eq 2 ] && grep -F '`--sanitize=address` was removed' "$SANITIZE_TMP/address.err" >/dev/null \
    || fail "--sanitize=address was not rejected ($label)"
done
printf 'sanitize: C UB traps and Fas arithmetic passes, native and oracle, O0/O2\n'

# Existing fixtures with C code must not trap: rebuild them all with
# --sanitize=undefined. The wrapper adds the flag to builds (not to the run,
# check and lsp subcommands, whose options follow the subcommand).
FIXTURE_LOG=$SANITIZE_TMP/fixtures.log
cat >"$SANITIZE_TMP/fas-wrapper" <<'WRAPPER'
#!/bin/sh
set -eu
case "${1-}" in
  run|check|lsp|--version|--help|-h) exec "$REAL_FAS" "$@" ;;
esac
level=default
source=
for argument do
  case "$argument" in
    -O0) level=0 ;;
    -O2) level=2 ;;
    *.fas) [ -n "$source" ] || source=$argument ;;
  esac
done
printf '%s|%s\n' "$source" "$level" >>"$FIXTURE_LOG"
exec "$REAL_FAS" --sanitize=undefined "$@"
WRAPPER
chmod +x "$SANITIZE_TMP/fas-wrapper"
export REAL_FAS FIXTURE_LOG

for component in \
  c_import c_records c_array_globals record_literals simd_value_builtins vector_literals \
  literal_aggregates record_attributes opaque_types generic_layout c_interop \
  defer_unwinding lexical_scopes generic_type_slots c_import_types packed_layouts; do
  printf 'sanitize: running %s\n' "$component"
  OCAML_FAS="$SANITIZE_TMP/fas-wrapper" sh "$ROOT/test/$component.sh"
done

for fixture in \
  test/c_import/program.fas test/c_records.fas test/c_import/arrays.fas \
  test/record_literals.fas test/c_interop.fas test/c_import_types.fas; do
  grep -F "$ROOT/$fixture|" "$FIXTURE_LOG" >/dev/null \
    || fail "$fixture was not built with --sanitize=undefined"
done
fixture_count=$(cut -d'|' -f1 "$FIXTURE_LOG" | sort -u | wc -l | tr -d ' ')
[ "$fixture_count" -ge 15 ] || fail "only $fixture_count fixtures were built sanitized"
printf 'sanitize: %s fixtures passed under --sanitize=undefined\n' "$fixture_count"
