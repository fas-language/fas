#!/usr/bin/env bash
set -euo pipefail
cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.."
ulimit -f 524288
if [[ -n ${FCC:-} ]]; then
    fcc=$(realpath -- "$FCC")
else
    build=$(mktemp -d -- "${TMPDIR:-/tmp}/fcc-parse-test.XXXXXXXX")
    trap 'rm -rf -- "$build"' EXIT
    fcc=$build/fcc
    timeout 600 _build/default/bin/main.exe -o "$fcc" cc/main.fas
fi
python3 test/cc_parse/compare.py "$fcc" "$@"
