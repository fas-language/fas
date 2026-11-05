#!/bin/sh
# Native shadow run: every test/*.sh script, once as written (LLVM) and once
# with the native backend generating the Fas code (test/native_shadow/shim.py
# explains how). A script that passes with LLVM must pass in the shadow run.
#
# Per script it reports the two statuses and how much native code the shadow
# run actually linked:
#   ok      both pass, native objects/executables were used
#   llvm    both pass, but native refused every module (see refusals below)
#   none    both pass, the script never compiles Fas code for execution
#   FAIL    LLVM passes, shadow fails  <- a native bug or an LLVM-only check
#   skip    LLVM itself fails here (environment); not compared
#
# Usage: OCAML_FAS=<main.exe> sh test/native_shadow.sh [script names...]
# Env: SHADOW_JOBS (default 4), SHADOW_OUT (kept results; default a tempdir),
#      SHADOW_TIMEOUT seconds per script (default 900).
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
JOBS=${SHADOW_JOBS:-4}
TIMEOUT=${SHADOW_TIMEOUT:-900}
OUT=${SHADOW_OUT:-$(mktemp -d)}
mkdir -p "$OUT/base" "$OUT/shadow" "$OUT/cache" "$OUT/bin"

abs() { case "$1" in /*) echo "$1" ;; *) command -v "$1" || echo "$PWD/$1" ;; esac; }
REAL_FAS=$(abs "$OCAML_FAS")
REAL_CC=$(abs "$CC")
REAL_LLC=$(abs "$LLVM_LLC")
REAL_OPT=$(abs "$LLVM_OPT")

PYTHON=$(command -v python3)
# Wrappers rather than symlinks: some scripts resolve the compiler path or
# run it under `env -i` with an empty PATH, so the tool name, interpreter and
# shadow settings are baked in (a per-script log from the environment wins).
if [ -z "${SHADOW_ONE:-}" ]; then
  for t in fas cc llc; do
    {
      echo '#!/bin/sh'
      echo ": \"\${FAS_SHADOW_REAL_FAS:=$REAL_FAS}\" \"\${FAS_SHADOW_REAL_CC:=$REAL_CC}\""
      echo ": \"\${FAS_SHADOW_REAL_LLC:=$REAL_LLC}\" \"\${FAS_SHADOW_CACHE:=$OUT/cache}\""
      echo ": \"\${FAS_SHADOW_LOG:=$OUT/shadow/unattributed.events}\""
      echo 'export FAS_SHADOW_REAL_FAS FAS_SHADOW_REAL_CC FAS_SHADOW_REAL_LLC FAS_SHADOW_CACHE FAS_SHADOW_LOG'
      echo "exec $PYTHON \"$ROOT/test/native_shadow/shim.py\" $t \"\$@\""
    } >"$OUT/bin/$t"
    chmod +x "$OUT/bin/$t"
  done
fi

# Scripts whose checks are about the LLVM pipeline itself, so they cannot pass
# with native code generation. A shadow failure there reads "llvm-only".
LLVM_ONLY="
driver: checks that opt and llc ran
variadic_call_types: matches LLVM's movb-to-%al form (native writes %eax, same values)
line_tables: compares -g builds (native emits no DWARF yet)
tool_names: checks which opt/llc/clang the LLVM pipeline picked
"

# Scripts that do not build programs from Fas, drive the whole suite, need
# the network/editors, or measure timing (the shadow would only slow them).
EXCLUDE=" run_all native_shadow fir_fuzz zed_extension tree_sitter_corpus editor_keywords \
 tune_runner tune_session tune_measure profile_tune profile_build perf_command "

if [ $# -gt 0 ]; then
  SCRIPTS=$*
else
  SCRIPTS=$(cd "$ROOT/test" && ls *.sh | sed 's/\.sh$//')
fi

one() {
  name=$1
  set +e
  cd "$ROOT"
  env OCAML_FAS="$REAL_FAS" CC="$REAL_CC" LLVM_LLC="$REAL_LLC" LLVM_OPT="$REAL_OPT" \
    FAS_CC="$REAL_CC" FAS_LLC="$REAL_LLC" FAS_OPT="$REAL_OPT" DEBUGINFOD_URLS= \
    timeout "$TIMEOUT" sh "test/$name.sh" >"$OUT/base/$name.log" 2>&1
  echo $? >"$OUT/base/$name.status"
  env OCAML_FAS="$OUT/bin/fas" CC="$OUT/bin/cc" LLVM_LLC="$OUT/bin/llc" LLVM_OPT="$REAL_OPT" \
    FAS_CC="$OUT/bin/cc" FAS_LLC="$OUT/bin/llc" FAS_OPT="$REAL_OPT" DEBUGINFOD_URLS= \
    FAS_SHADOW_REAL_FAS="$REAL_FAS" FAS_SHADOW_REAL_CC="$REAL_CC" FAS_SHADOW_REAL_LLC="$REAL_LLC" \
    FAS_SHADOW_CACHE="$OUT/cache" FAS_SHADOW_LOG="$OUT/shadow/$name.events" FAS_SHADOW_SCRIPT="$name" \
    timeout "$TIMEOUT" sh "test/$name.sh" >"$OUT/shadow/$name.log" 2>&1
  echo $? >"$OUT/shadow/$name.status"
}

if [ "${SHADOW_ONE:-}" ]; then one "$SHADOW_ONE"; exit 0; fi

SHADOW_OUT=$OUT; export SHADOW_OUT ROOT OUT REAL_FAS REAL_CC REAL_LLC REAL_OPT TIMEOUT
for s in $SCRIPTS; do
  case "$EXCLUDE" in *" $s "*) continue ;; esac
  echo "$s"
done | xargs -P "$JOBS" -I{} env SHADOW_ONE={} sh "$0" || true

fail=0
printf '%-40s %s\n' script result
for s in $SCRIPTS; do
  [ -f "$OUT/base/$s.status" ] || continue
  b=$(cat "$OUT/base/$s.status"); n=$(cat "$OUT/shadow/$s.status")
  ev="$OUT/shadow/$s.events"; touch "$ev"
  used=$(grep -c -E '	(native-build|cc-link|cc-object|llc-object)	' "$ev" || true)
  refused=$(grep -c '	native-refused' "$ev" || true)
  only=$(printf '%s\n' "$LLVM_ONLY" | grep "^$s:" | cut -d: -f2- || true)
  if [ "$b" != 0 ]; then r="skip (llvm exit $b)"
  elif [ "$n" != 0 ] && [ -n "$only" ]; then r="llvm-only ($only )"
  elif [ "$n" != 0 ]; then r="FAIL (shadow exit $n)"; fail=$((fail + 1))
  elif [ "$used" -gt 0 ]; then r="ok ($used native, $refused refused)"
  elif [ "$refused" -gt 0 ]; then r="llvm ($refused refused)"
  else r="none"; fi
  printf '%-40s %s\n' "$s" "$r"
done
echo "results in $OUT"
[ "$fail" = 0 ]
