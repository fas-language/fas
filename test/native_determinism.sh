#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
# The native backend's output must be byte-identical across runs (roadmap:
# deterministic output, the baseline for the rewrite in Fas). Every Fas test
# program and every Fir conformance program is compiled twice:
#   A: from the repository root, the normal way;
#   B: from another directory, into a differently named output, with OCaml's
#      hash tables randomized (OCAMLRUNPARAM=R), so any output that depends
#      on Hashtbl iteration order, the working directory or the output path
#      shows up as a difference.
# Objects (-c) are compared for every program the backend accepts;
# executables (static PIE and -no-pie) for the Fir programs and a few Fas
# ones, and -g builds (DWARF line tables) of some. A program the backend refuses in run A must be refused in run B too.
# Needs no LLVM and no C compiler.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
FAS_FIR_TOOL=${FAS_FIR_TOOL:-$ROOT/_build/default/tools/fas_fir/fas_fir.exe}
DET_TMP=$(mktemp -d)
trap 'rm -rf "$DET_TMP"' EXIT HUP INT TERM
mkdir "$DET_TMP/a" "$DET_TMP/b" "$DET_TMP/elsewhere"

fail=0
compared=0

# same NAME TOOL ARGS... -- compile twice and compare; the output path is
# appended to ARGS.
same() {
  name=$1; shift
  if ! (cd "$ROOT" && "$@" -o "$DET_TMP/a/out") >/dev/null 2>&1; then
    if (cd "$DET_TMP/elsewhere" && OCAMLRUNPARAM=R "$@" -o "$DET_TMP/b/other-name") \
      >/dev/null 2>&1; then
      echo "native determinism: $name refused once, accepted once" >&2; fail=1
    fi
    return 0
  fi
  if ! (cd "$DET_TMP/elsewhere" && OCAMLRUNPARAM=R "$@" -o "$DET_TMP/b/other-name") \
    >/dev/null 2>&1; then
    echo "native determinism: $name accepted once, refused once" >&2; fail=1
    return 0
  fi
  compared=$((compared + 1))
  if ! cmp -s "$DET_TMP/a/out" "$DET_TMP/b/other-name"; then
    echo "native determinism: $name differs between runs" >&2; fail=1
  fi
}

for f in "$ROOT"/test/*.fas; do
  same "$(basename "$f") -c" "$OCAML_FAS" --backend=native -c "$f"
done
for b in native_backend native_vectors native_floats ir_simple; do
  f=$ROOT/test/$b.fas
  same "$b.fas" "$OCAML_FAS" --backend=native "$f"
  same "$b.fas -no-pie" "$OCAML_FAS" --backend=native -Wl,-no-pie "$f"
  # the source path is absolute, so the debug info does not depend on the
  # working directory either (a relative one, like the loc entries of
  # test/fas_fir/lines.fir, is resolved against it, as gcc and clang do)
  same "$b.fas -g" "$OCAML_FAS" --backend=native -g "$f"
done
for f in "$ROOT"/test/fas_fir/*.fir "$ROOT"/test/fas_fir/c/*.fir; do
  same "${f#"$ROOT"/} -c" "$FAS_FIR_TOOL" -c "$f"
done
"$FAS_FIR_TOOL" -c "$ROOT/test/fas_fir/putnum.fir" -o "$DET_TMP/putnum.o"
for f in "$ROOT"/test/fas_fir/*.fir; do
  [ "$(basename "$f")" = putnum.fir ] && continue
  same "${f#"$ROOT"/}" "$FAS_FIR_TOOL" "$f" "$DET_TMP/putnum.o"
  same "${f#"$ROOT"/} --no-pie" "$FAS_FIR_TOOL" --no-pie "$f" "$DET_TMP/putnum.o"
done

[ "$compared" -gt 100 ] || { echo "native determinism: only $compared builds compared" >&2; fail=1; }
[ $fail = 0 ] && echo "native determinism: $compared builds byte-identical"
exit $fail
