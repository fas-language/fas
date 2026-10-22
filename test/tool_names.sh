#!/bin/sh
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
case "$OCAML_FAS" in /*) ;; *) OCAML_FAS=$(pwd)/$OCAML_FAS ;; esac
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

fail() {
  echo "tool_names: $*" >&2
  exit 1
}

real() {
  path=$(command -v "$1") || fail "$1 not found"
  readlink -f "$path"
}

CLANG=$(real "$CC")
OPT=$(real "$LLVM_OPT")
LLC=$(real "$LLVM_LLC")
LD=$(real ld)

mkdir "$TMP/plain" "$TMP/both" "$TMP/old"
for dir in plain both old; do
  ln -s "$CLANG" "$TMP/$dir/clang"
  ln -s "$LLC" "$TMP/$dir/llc"
  ln -s "$LD" "$TMP/$dir/ld"
done
ln -s "$OPT" "$TMP/plain/opt"
ln -s "$OPT" "$TMP/both/opt-22"
cat >"$TMP/old/opt" <<'SH'
#!/bin/sh
echo "LLVM version 21.1.0"
SH
chmod +x "$TMP/old/opt"
cp "$TMP/old/opt" "$TMP/both/opt"

cat >"$TMP/hello.fas" <<'FAS'
use "C" <stdio.h>
fn main() i32 {
  printf(c"tools ok\n")
  return 0
}
FAS

build() {
  dir=$1
  shift
  (
    cd "$TMP"
    env -u FAS_CC -u FAS_OPT -u FAS_LLC -u FAS_PROFDATA PATH="$TMP/$dir" TMPDIR="$TMP" \
      "$OCAML_FAS" "$@"
  )
}

for level in -O0 -O2; do
  build plain "$level" --keep hello.fas -o hello >"$TMP/plain.out" 2>&1 \
    || { cat "$TMP/plain.out" >&2; fail "unsuffixed LLVM 22 tools were not used at $level"; }
  test "$("$TMP/hello")" = "tools ok" || fail "unsuffixed tools built a wrong program"
  grep -F 'FAS_OPT=opt FAS_LLC=llc FAS_CC=clang ' "$TMP/plain.out" >/dev/null \
    || fail "unsuffixed tools were not reported at $level"
done

build both --keep hello.fas -o hello >"$TMP/both.out" 2>&1 \
  || fail "suffixed opt-22 next to an old opt failed"
grep -F 'FAS_OPT=opt-22 FAS_LLC=llc FAS_CC=clang ' "$TMP/both.out" >/dev/null \
  || fail "suffixed tool did not win over the unsuffixed one"

if build old hello.fas -o hello >"$TMP/old.out" 2>&1; then
  fail "an opt that is not LLVM 22 was accepted"
fi
grep -F 'opt-22' "$TMP/old.out" >/dev/null || fail "missing tool error did not name opt-22"

(
  cd "$TMP"
  PATH="$TMP/old" FAS_OPT="$OPT" "$OCAML_FAS" hello.fas -o hello
) || fail "FAS_OPT did not override the PATH lookup"

echo 'tool_names: unsuffixed LLVM 22 tools, suffixed preference, version check and overrides: ok'
