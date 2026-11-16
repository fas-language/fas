#!/bin/sh
# Untyped literals inside `bitcast[T]` take T's size (LANGUAGE.md
# "Conversions"): test/bitcast_literals.fas runs at -O0 and -O2, and a
# literal too wide for the target is an error that names the size.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
BL_TMP=$(mktemp -d)
trap 'rm -rf "$BL_TMP"' EXIT HUP INT TERM

cp "$ROOT/test/bitcast_literals.fas" "$BL_TMP/ok.fas"
for level in 0 2; do
  (cd "$BL_TMP" && "$OCAML_FAS" -O"$level" ok.fas -o ok)
  "$BL_TMP/ok" || { echo "bitcast_literals: -O$level exited $?" >&2; exit 1; }
done

cat >"$BL_TMP/wide.fas" <<'FAS'
fn main() i32 {
    a f32 = bitcast[f32](0x1FFFFFFFF)
    return 0
}
FAS
if (cd "$BL_TMP" && "$OCAML_FAS" wide.fas -o wide) 2>"$BL_TMP/wide.err"; then
  echo "bitcast_literals: wide.fas compiled, want an error" >&2
  exit 1
fi
grep -qF "untyped literal is a u32, so it must fit in 32 bits" "$BL_TMP/wide.err" || {
  echo "bitcast_literals: wide.fas: unexpected error:" >&2
  cat "$BL_TMP/wide.err" >&2
  exit 1
}
echo "bitcast_literals: ok"
