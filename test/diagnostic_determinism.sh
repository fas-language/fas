#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BIN=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT HUP INT TERM
ulimit -v 4194304

if git -C "$ROOT" rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  git -C "$ROOT" ls-files --cached -- test | awk '/\.fas$/ { print }' >"$WORK/sources"
else
  find "$ROOT/test" -path "$ROOT/test/mistakes" -prune -o -type f -name '*.fas' -print \
    | sed "s|^$ROOT/||" | sort >"$WORK/sources"
fi
count=0
while IFS= read -r relative; do
  source=$ROOT/$relative
  set +e
  "$BIN" --emit-llvm "$source" >"$WORK/first.out" 2>"$WORK/first.err"
  first_status=$?
  set -e
  [ "$first_status" -ne 0 ] || continue
  set +e
  "$BIN" --emit-llvm "$source" >"$WORK/second.out" 2>"$WORK/second.err"
  second_status=$?
  set -e
  [ "$first_status" -eq "$second_status" ] || {
    echo "diagnostic determinism: status changed for $source" >&2
    exit 1
  }
  cmp -s "$WORK/first.err" "$WORK/second.err" || {
    echo "diagnostic determinism: stderr changed for $source" >&2
    diff -u "$WORK/first.err" "$WORK/second.err" >&2 || true
    exit 1
  }
  count=$((count + 1))
done <"$WORK/sources"

cat >"$WORK/failure.h" <<'EOF'
#error fas diagnostic determinism
EOF
cat >"$WORK/forced_import.fas" <<EOF
use "C" "$WORK/failure.h"
fn main() void {}
EOF
set +e
"$BIN" --emit-llvm "$WORK/forced_import.fas" >"$WORK/forced_first.out" 2>"$WORK/forced_first.err"
first_status=$?
"$BIN" --emit-llvm "$WORK/forced_import.fas" >"$WORK/forced_second.out" 2>"$WORK/forced_second.err"
second_status=$?
set -e
[ "$first_status" -ne 0 ] && [ "$first_status" -eq "$second_status" ] || {
  echo "diagnostic determinism: forced import failure status changed" >&2
  exit 1
}
cmp -s "$WORK/forced_first.err" "$WORK/forced_second.err" || {
  echo "diagnostic determinism: forced import failure changed" >&2
  diff -u "$WORK/forced_first.err" "$WORK/forced_second.err" >&2 || true
  exit 1
}

echo "diagnostic determinism: $count rejected inputs matched"
