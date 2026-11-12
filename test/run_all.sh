#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
START_DIR=$(pwd)
LLVM_OPT=${FAS_OPT:-${LLVM_OPT:-opt-22}}
LLVM_LLC=${FAS_LLC:-${LLVM_LLC:-llc-22}}
CC=${FAS_CC:-${CC:-clang-22}}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}

resolve_tool_path() {
  case "$1" in
    /*) printf '%s\n' "$1" ;;
    */*) printf '%s/%s\n' "$START_DIR" "$1" ;;
    *)
      resolved=$(command -v "$1" 2>/dev/null || true)
      case "$resolved" in
        /*) printf '%s\n' "$resolved" ;;
        */*) printf '%s/%s\n' "$START_DIR" "$resolved" ;;
        *) printf '%s\n' "$1" ;;
      esac
      ;;
  esac
}

LLVM_OPT=$(resolve_tool_path "$LLVM_OPT")
LLVM_LLC=$(resolve_tool_path "$LLVM_LLC")
CC=$(resolve_tool_path "$CC")
OCAML_FAS=$(resolve_tool_path "$OCAML_FAS")
export FAS_OPT="$LLVM_OPT" FAS_LLC="$LLVM_LLC" FAS_CC="$CC"
START_TIME=$(date +%s)
GATE_TMP=$(mktemp -d)
trap 'rm -rf "$GATE_TMP"' EXIT HUP INT TERM

cd "$ROOT"
for tool in "$LLVM_OPT" "$LLVM_LLC" "$CC"; do
  if ! command -v "$tool" >/dev/null 2>&1 && [ ! -x "$tool" ]; then
    echo "validation: required tool is missing: $tool" >&2
    exit 2
  fi
done

if [ "${FAS_OCAML_CONTAINER+x}" = x ]; then
  CONTAINER=$FAS_OCAML_CONTAINER
  DOCKER=${DOCKER:-docker}
  DOCKER=$(resolve_tool_path "$DOCKER")
  if ! command -v "$DOCKER" >/dev/null 2>&1 && [ ! -x "$DOCKER" ]; then
    echo "validation: required tool is missing: $DOCKER" >&2
    exit 2
  fi
  "$DOCKER" inspect "$CONTAINER" >/dev/null 2>&1 || {
    echo "validation: persistent container $CONTAINER is unavailable" >&2
    exit 2
  }
  "$DOCKER" exec "$CONTAINER" sh -lc \
    'cd /work && eval $(opam env) && dune build --display=short @all && dune build --display=short @fmt && dune runtest --force --display=short'
else
  dune build @all
  dune build @fmt
  dune runtest --force
fi

"$OCAML_FAS" --emit-llvm test/ir_simple.fas >"$GATE_TMP/stage3.ll"
"$LLVM_OPT" -passes=verify "$GATE_TMP/stage3.ll" -disable-output
"$LLVM_LLC" "$GATE_TMP/stage3.ll" -o "$GATE_TMP/stage3.s"

COMPONENTS=0
for component in "$ROOT"/test/*.sh; do
  [ -f "$component" ] || continue
  [ "$component" = "$ROOT/test/run_all.sh" ] && continue
  COMPONENTS=$((COMPONENTS + 1))
  echo "validation: running ${component#"$ROOT"/}"
  CC="$CC" LLVM_OPT="$LLVM_OPT" LLVM_LLC="$LLVM_LLC" OCAML_FAS="$OCAML_FAS" \
    sh "$component"
done

END_TIME=$(date +%s)
echo "validation: all $COMPONENTS component scripts passed in $((END_TIME - START_TIME)) seconds"
