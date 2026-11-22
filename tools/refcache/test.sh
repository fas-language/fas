#!/usr/bin/env bash
set -euo pipefail
here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
base=${FAS_REFCACHE_TEST_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/fas-refcache-tests}
mkdir -p "$base"
work=$(mktemp -d "$base/test.XXXXXXXX")
trap 'rm -rf "$work"' EXIT
export FAS_REFCACHE=$work/cache
cd "$work"
cache=$here/refcache
cat > oracle <<'PY'
#!/usr/bin/env python3
import os
from pathlib import Path
import sys

if sys.argv[1:] == ["--version"]:
    print("fixed version")
    sys.exit(0)
with open("calls", "ab") as f:
    f.write(b"x")
Path(sys.argv[1]).write_bytes(Path("input").read_bytes())
Path(sys.argv[1]).chmod(0o751)
os.write(1, b"out\x00\xff\r\n")
os.write(2, b"err\x00\xfe\n")
sys.exit(23)
PY
chmod +x oracle
printf 'first\000\377' > input
run() {
    local rc=0
    "$cache" run --ns oracle --dep input --out "$1" -- ./oracle "$1" >stdout 2>stderr || rc=$?
    [[ $rc == 23 ]]
}
rc=0
./oracle expected >stdout.expected 2>stderr.expected || rc=$?
[[ $rc == 23 ]]
rm calls
run output
rm output
run output
[[ $(wc -c <calls) == 1 ]]
cmp stdout stdout.expected
cmp stderr stderr.expected
cmp output expected
[[ $(stat -c %a output) == 751 ]]
"$cache" verify --sample 1 | grep 'verified=1 requested=1 mismatches=0'
printf x >calls
printf second >input
run output
cmp output input
[[ $(wc -c <calls) == 2 ]]
printf '\n# same version, different binary bytes\n' >>oracle
run output
[[ $(wc -c <calls) == 3 ]]
find "$FAS_REFCACHE/blobs" -type f -exec sh -c 'printf corrupt >"$1"' sh {} \;
run output
cmp stdout stdout.expected
cmp stderr stderr.expected
[[ $(wc -c <calls) == 4 ]]
# All workers publish identical content and use private destinations.
cat > parallel <<'PY'
#!/usr/bin/env python3
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
import sys
from refcache import Cache

def run(i):
    scratch = Path(f"worker{i}").absolute()
    scratch.mkdir()
    out = scratch / "output"
    result = Cache().run(["./oracle", out], ns="parallel", deps=["input"], outs=[out], scratch=scratch)
    assert result.returncode == 23 and out.read_bytes() == b"second"

if __name__ == "__main__":
    with ProcessPoolExecutor(8) as workers:
        list(workers.map(run, range(16)))
PY
PYTHONPATH=$here python3 parallel
while IFS= read -r index; do printf broken >"$index"; done < <(find "$FAS_REFCACHE/index" -type f)
run output
cmp output input
"$cache" stats | grep -E '^entries=1 '
"$cache" gc --max-size 0 | grep 'retained bytes=0'
"$cache" stats | grep -E '^entries=0 blobs=0 '
cat > cc <<'SH_CC'
#!/bin/sh
case "${1:-}" in --version|-print-*) exec "${FAS_CC:-clang-22}" "$@";; esac
printf x >>cc-calls
exec "${FAS_CC:-clang-22}" "$@"
SH_CC
chmod +x cc
printf '#include "value.h"\nVALUE\n' > probe.c
printf '#define VALUE 101\n' > value.h
"$cache" run --ns header --c --dep probe.c -- ./cc -E -P probe.c >a
"$cache" run --ns header --c --dep probe.c -- ./cc -E -P probe.c >b
cmp a b
[[ $(wc -c <cc-calls) == 1 ]]
printf '#define VALUE 202\n' > value.h
"$cache" run --ns header --c --dep probe.c -- ./cc -E -P probe.c >c
grep -qx 202 c
[[ $(wc -c <cc-calls) == 2 ]]
"$cache" run --refresh --ns header --c --dep probe.c -- ./cc -E -P probe.c >d
FAS_REFCACHE=off "$cache" run --ns header --c --dep probe.c -- ./cc -E -P probe.c >e
cmp c d
cmp c e
[[ $(wc -c <cc-calls) == 4 ]]
printf 'stdin\000\377' | "$cache" run --stdin --ns stdin -- cat >actual
printf 'stdin\000\377' | cmp - actual
"$cache" verify --sample 2 | grep 'verified=2 requested=2 mismatches=0'
"$here/bench.py" self-test
echo 'refcache: all tests passed'
