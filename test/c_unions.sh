#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

fail() {
  echo "c_unions: $*" >&2
  exit 1
}

"$CC" -Werror -std=c17 "$ROOT/test/c_unions_oracle.c" -o "$TMP/oracle"
"$OCAML_FAS" --emit-llvm "$ROOT/test/c_unions.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output

for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" \
    -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" "$ROOT/test/c_unions.fas" -o "$TMP/fas.O$level"
  if [ "$level" -eq 0 ]; then
    nm "$TMP/fas.O$level" | awk '$2 == "B" && $3 == "screens" { found = 1 } END { exit !found }' \
      || fail "exported zero global is not defined in BSS"
  fi
  timeout 30 "$TMP/fas.O$level" >"$TMP/fas.O$level.out" \
    || fail "Fas O$level execution failed"
  timeout 30 "$TMP/oracle" >"$TMP/oracle.out" \
    || fail "C O$level oracle failed"
  cmp -s "$TMP/fas.O$level.out" "$TMP/oracle.out" \
    || fail "Fas and C oracle outputs differ at O$level"
done

if command -v pkg-config >/dev/null 2>&1 && pkg-config --exists sdl2; then
  SDL_CFLAGS=$(pkg-config --cflags sdl2)
  SDL_LIBS=$(pkg-config --libs sdl2)
  "$CC" -Werror -std=c17 "$ROOT/test/c_unions_sdl_oracle.c" $SDL_CFLAGS \
    $SDL_LIBS -o "$TMP/sdl-oracle"
  "$OCAML_FAS" --emit-llvm $SDL_CFLAGS "$ROOT/test/c_unions_sdl.fas" \
    >"$TMP/sdl.ll"
  "$LLVM_OPT" -passes=verify "$TMP/sdl.ll" -disable-output
  for level in 0 2; do
    "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/sdl.ll" \
      -o "$TMP/sdl.O$level.ll"
    "$LLVM_OPT" -passes=verify "$TMP/sdl.O$level.ll" -disable-output
    "$OCAML_FAS" -O"$level" "$ROOT/test/c_unions_sdl.fas" \
      -o "$TMP/sdl.O$level" $SDL_CFLAGS $SDL_LIBS
    SDL_VIDEODRIVER=dummy timeout 30 "$TMP/sdl-oracle" >"$TMP/sdl.oracle.out" \
      || fail "SDL C oracle failed"
    SDL_VIDEODRIVER=dummy timeout 30 "$TMP/sdl.O$level" \
      >"$TMP/sdl.O$level.out" || fail "SDL Fas O$level execution failed"
    cmp -s "$TMP/sdl.O$level.out" "$TMP/sdl.oracle.out" \
      || fail "SDL Fas and C outputs differ at O$level"
  done
  echo "c_unions: SDL_Event O0/O2 push/poll code 7 matches C oracle"
else
  echo "c_unions: SDL2 unavailable; SDL_Event round trip skipped"
fi

echo "c_unions: O0/O2 verified, union bytes match C oracle: ok"
