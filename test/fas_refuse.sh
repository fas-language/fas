#!/bin/sh
# The Fas compiler refuses bad input with diagnostics and never crashes
# (roadmap v0.7 hardening).
# 1. Known cases, each once a crash found by test/fas_refuse.py: a valid
#    program with one edit, and the message the compiler must give for it.
# 2. test/fas_refuse.py mutates every .fas file under test/
#    FAS_REFUSE_COUNT times in all (default 2000) with seed
#    FAS_REFUSE_SEED (default 1), and fails on any uncaught exception,
#    internal error, stack overflow, signal or timeout.
# OCAML_FAS names the compiler; by default the dune build.
set -eu
unset FAS_ORACLE
here=$(cd "$(dirname "$0")" && pwd)
tool=${OCAML_FAS:-$here/../_build/default/bin/main.exe}
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0; n=0
# case FILE SED-EDIT EXPECTED-MESSAGE (checked with `fas check`)
case_() {
  n=$((n + 1))
  sed "$2" "$here/$1" > "$tmp/m.fas"
  if cmp -s "$here/$1" "$tmp/m.fas"; then echo "FAIL $1: the edit $2 changed nothing"; fail=1; return; fi
  set +e; (cd "$tmp" && timeout 60 "$tool" check m.fas) > /dev/null 2> "$tmp/err"; rc=$?; set -e
  if [ $rc != 1 ] || ! grep -qF -- "$3" "$tmp/err" || grep -q 'internal error' "$tmp/err"; then
    echo "FAIL $1 ($2): exit $rc, expected 1 with '$3':"; head -n 3 "$tmp/err"; fail=1
  fi
}
# a value where a generic struct is applied, in a memory access type
case_ copy_compile_1048576.fas 's/copy(dst\[arr\[/copy(dst[src[/' \
  'unknown generic struct `src`'
# 2. the fuzzer
if command -v python3 > /dev/null 2>&1; then
  # crashing inputs would vanish with $tmp: keep them in FAS_REFUSE_KEEP
  # (default ./fas_refuse_crashes)
  export FAS_REFUSE_KEEP="${FAS_REFUSE_KEEP:-$PWD/fas_refuse_crashes}"
  out=$(cd "$tmp" && python3 "$here/fas_refuse.py" "$tool" "${FAS_REFUSE_COUNT:-2000}" "${FAS_REFUSE_SEED:-1}" \
          $(find "$here" -name '*.fas' | sort)) || { echo "$out"; fail=1; out=; }
else
  out="(no python3: fuzzer skipped)"
fi
[ $fail = 0 ] && echo "fas refuse: $n known cases; $out"
exit $fail
