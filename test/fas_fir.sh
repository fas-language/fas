#!/bin/sh
# Hand-written Fir programs through fas-fir (Fir text -> native executable).
# Each test/fas_fir/NAME.fir is linked with putnum.fir; NAME.out holds the
# expected stdout and an optional "; exit: N" line in the .fir the expected
# exit code (default 0). FAS_FIR_TOOL names the tool; by default the dune build.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
tool=${FAS_FIR_TOOL:-$here/../_build/default/tools/fas_fir/fas_fir.exe}
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
"$tool" -c "$here/fas_fir/putnum.fir" -o "$tmp/putnum.o"
fail=0; n=0
for f in "$here"/fas_fir/*.fir; do
  name=$(basename "$f" .fir)
  [ "$name" = putnum ] && continue
  n=$((n + 1))
  want=$(sed -n 's/^; exit: *\([0-9]*\).*/\1/p' "$f"); want=${want:-0}
  if ! "$tool" "$f" -o "$tmp/$name" "$tmp/putnum.o" 2>"$tmp/err"; then
    echo "FAIL $name: $(cat "$tmp/err")"; fail=1; continue
  fi
  set +e; "$tmp/$name" > "$tmp/$name.stdout"; rc=$?; set -e
  if [ "$rc" != "$want" ]; then echo "FAIL $name: exit $rc, expected $want"; fail=1
  elif ! cmp -s "$tmp/$name.stdout" "$here/fas_fir/$name.out"; then
    echo "FAIL $name: stdout differs"; diff "$here/fas_fir/$name.out" "$tmp/$name.stdout" | head; fail=1
  fi
done
[ $fail = 0 ] && echo "fas-fir: $n programs ok"
exit $fail
