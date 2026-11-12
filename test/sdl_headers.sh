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

if ! pkg-config --exists sdl2; then
  echo "sdl headers: skipped (SDL2 not installed)"
  exit 0
fi
SDL_CFLAGS=$(pkg-config --cflags sdl2)
SDL_LIBS=$(pkg-config --libs sdl2)
"$OCAML_FAS" --emit-llvm $SDL_CFLAGS "$ROOT/test/sdl_headers.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
"$CC" -Werror -std=c17 "$ROOT/test/sdl_headers_oracle.c" $SDL_CFLAGS $SDL_LIBS -o "$TMP/oracle"

for level in 0 2; do
  "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/program.ll" -o "$TMP/program.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/program.O$level.ll" -disable-output
  "$OCAML_FAS" -O"$level" -o "$TMP/fas.O$level" "$ROOT/test/sdl_headers.fas" $SDL_CFLAGS $SDL_LIBS
done

worked=0
for driver in offscreen dummy; do
  if ! SDL_VIDEODRIVER=$driver "$TMP/oracle" >"$TMP/oracle.out" 2>/dev/null; then
    continue
  fi
  driver_ok=1
  for level in 0 2; do
    if ! SDL_VIDEODRIVER=$driver timeout 30 "$TMP/fas.O$level" >"$TMP/fas.O$level.out" 2>/dev/null; then
      driver_ok=0
      break
    fi
    cmp -s "$TMP/oracle.out" "$TMP/fas.O$level.out" || {
      echo "sdl headers: output differs at O$level with $driver" >&2
      exit 1
    }
  done
  if [ "$driver_ok" -eq 1 ]; then
    worked=1
    echo "sdl headers: $driver O0/O2 LLVM verified, checksum and events match C: $(cat "$TMP/oracle.out" | tr -d '\n')"
    break
  fi
done

if [ "$worked" -eq 0 ]; then
  echo "sdl headers: SDL2 is installed, but offscreen and dummy video drivers failed" >&2
  exit 1
fi
