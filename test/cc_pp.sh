#!/usr/bin/env bash
set -euo pipefail
cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.."
ulimit -f 524288
if [[ -n ${FCC:-} ]]; then
    binary=$(realpath -- "$FCC")
else
    build=$(mktemp -d -- "${TMPDIR:-/tmp}/fcc-test.XXXXXXXX")
    trap 'rm -rf -- "$build"' EXIT
    binary=$build/fcc
    timeout 600 _build/default/bin/main.exe -o "$binary" cc/main.fas
fi
python3 test/cc_pp/compare.py "$binary" "$@"
