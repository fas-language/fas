#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
ulimit -v 4194304
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

for kind in functions structs enums; do
  for count in 1250 5000; do
    CASE="$TMP/$kind-$count"
    mkdir -p "$CASE"
    python3 - "$CASE" "$kind" "$count" <<'PY'
from pathlib import Path
import sys

directory = Path(sys.argv[1])
kind = sys.argv[2]
count = int(sys.argv[3])
if kind == "functions":
    declarations = [f"int f{i}(int);" for i in range(count)]
elif kind == "structs":
    declarations = [f"struct s{i} {{ int a; char b; }};" for i in range(count)]
else:
    declarations = [f"enum e{i} {{ E{i}a, E{i}b }};" for i in range(count)]
(directory / "h.h").write_text("\n".join(declarations) + "\n")
(directory / "main.fas").write_text('use "C" "h.h"\nfn main() i32 { return 0 }\n')
PY
    best=
    run=0
    while [ "$run" -lt 3 ]; do
      start=$(date +%s%N)
      (
        ulimit -v 4194304
        FAS_CC="$CC" FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" OCAML_FAS="$OCAML_FAS" \
          "$OCAML_FAS" --emit-llvm -I "$CASE" "$CASE/main.fas" >/dev/null
      )
      end=$(date +%s%N)
      elapsed=$((end - start))
      if [ -z "$best" ] || [ "$elapsed" -lt "$best" ]; then best=$elapsed; fi
      run=$((run + 1))
    done
    printf '%s\n' "$best" > "$TMP/$kind-$count.time"
  done
  small=$(cat "$TMP/$kind-1250.time")
  large=$(cat "$TMP/$kind-5000.time")
  ratio=$(awk -v small="$small" -v large="$large" 'BEGIN { printf "%.3f", large / small }')
  printf 'C import scaling %s: 1250=%s ns 5000=%s ns ratio=%s\n' "$kind" "$small" "$large" "$ratio"
  if awk -v ratio="$ratio" 'BEGIN { exit !(ratio > 8) }'; then
    printf 'C import scaling exceeded 8 for %s\n' "$kind" >&2
    exit 1
  fi
done

SDL_HEADER=/usr/include/SDL/SDL.h
WOLF3D_ROOT=${HOME:-}/wolf3d
if [ ! -f "$SDL_HEADER" ]; then
  printf 'C import real-header timing: skipped (SDL 1.2 headers absent)\n'
else
  if [ -f "$WOLF3D_ROOT/src/id_pm.fas" ]; then
    cp "$WOLF3D_ROOT/src/id_pm.fas" "$TMP/id_pm.fas"
    if /usr/bin/time -f '%U' -o "$TMP/id-pm.cpu" \
      env FAS_CC="$CC" FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" OCAML_FAS="$OCAML_FAS" \
      "$OCAML_FAS" -O2 -I "$WOLF3D_ROOT/reference/wolf4sdl" \
      -isystem /usr/include/SDL -D_GNU_SOURCE=1 -D_REENTRANT \
      -c "$TMP/id_pm.fas" -o "$TMP/id_pm.o" \
      2>"$TMP/id-pm.compile.log"; then
      id_pm_cpu=$(cat "$TMP/id-pm.cpu")
      printf 'C import id_pm CPU: %ss (limit 2.50s)\n' "$id_pm_cpu"
      if awk -v cpu="$id_pm_cpu" 'BEGIN { exit !(cpu > 2.5) }'; then
        printf 'C import id_pm exceeded 2.50 CPU seconds\n' >&2
        exit 1
      fi
    else
      printf 'C import id_pm timing: skipped (Wolf3D source does not compile)\n'
    fi
  else
    printf 'C import id_pm timing: skipped (Wolf3D source absent)\n'
  fi

  cat >"$TMP/sdl_main.fas" <<'FAS'
use "C" <SDL.h>
fn main() i32 { return 0 }
FAS
  printf '#include <SDL.h>\n' >"$TMP/sdl_syntax.c"
  printf 'fn main() i32 { return 0 }\n' >"$TMP/empty.fas"
  "$OCAML_FAS" --emit-llvm "$TMP/empty.fas" >"$TMP/empty.ll"
  /usr/bin/time -f '%U' -o "$TMP/clang.cpu" \
    "$CC" -fsyntax-only -isystem /usr/include/SDL "$TMP/sdl_syntax.c"
  /usr/bin/time -f '%U' -o "$TMP/opt.cpu" \
    "$LLVM_OPT" -passes='default<O2>' -S "$TMP/empty.ll" -o "$TMP/empty.opt.ll"
  /usr/bin/time -f '%U' -o "$TMP/llc.cpu" \
    "$LLVM_LLC" -O2 "$TMP/empty.opt.ll" -o "$TMP/empty.s"
  /usr/bin/time -f '%U' -o "$TMP/sdl.cpu" \
    env FAS_CC="$CC" FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" OCAML_FAS="$OCAML_FAS" \
    "$OCAML_FAS" -O2 -isystem /usr/include/SDL -c "$TMP/sdl_main.fas" \
    -o "$TMP/sdl.o"
  sdl_cpu=$(cat "$TMP/sdl.cpu")
  baseline=$(awk -v clang="$(cat "$TMP/clang.cpu")" \
    -v opt="$(cat "$TMP/opt.cpu")" -v llc="$(cat "$TMP/llc.cpu")" \
    'BEGIN { printf "%.2f", clang + opt + llc }')
  sdl_limit=$(awk -v baseline="$baseline" \
    'BEGIN { limit = baseline * 8; if (limit < 0.6) limit = 0.6; if (limit > 1.0) limit = 1.0; printf "%.2f", limit }')
  printf 'C import SDL CPU: %ss (Clang + opt + llc %ss; limit %ss)\n' \
    "$sdl_cpu" "$baseline" "$sdl_limit"
  if awk -v cpu="$sdl_cpu" -v limit="$sdl_limit" 'BEGIN { exit !(cpu > limit) }'; then
    printf 'C import SDL timing exceeded %s CPU seconds\n' "$sdl_limit" >&2
    exit 1
  fi
fi
