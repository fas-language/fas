#!/bin/sh
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
case $OCAML_FAS in
  /*) ;;
  *) OCAML_FAS=$(pwd)/$OCAML_FAS ;;
esac
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

fail() {
  echo "c_header_clash: $*" >&2
  exit 1
}

# C namespaces (Fas v2 decisions): a C name is always `alias.NAME`, so a
# Fas top-level name never clashes with a C declaration of the same
# spelling. Each case below checks, and the function case also links and
# runs with both the Fas `remove` and C's `c.remove` in one program.
expect_coexists() {
  name=$1
  source=$2
  "$OCAML_FAS" check "$source" >"$TMP/$name.out" 2>&1 \
    || fail "$name: Fas and C \`$name\` should coexist: $(cat "$TMP/$name.out")"
}

cd "$TMP"

cat >clash-function.fas <<'FAS'
use "C" <stdio.h> as c

fn remove(x u64) u64 {
    return x + 1
}

fn main(argc i32, argv addr) i32 {
    // C remove of a missing file fails with -1; the Fas remove adds one
    if c.remove(c"/nonexistent/fas-c-header-clash") != -1 { return 1 }
    if remove(41) != 42 { return 2 }
    if c.EOF != -1 { return 3 }
    return 0
}
FAS
cat >clash-global.fas <<'FAS'
use "C" <stdio.h> as c

var stdin i32 = 1

fn main(argc i32, argv addr) i32 {
    if c.stdin == null { return 9 }
    return stdin - 1
}
FAS
cat >clash-struct-tag.fas <<'FAS'
use "C" <stdio.h> as c

struct FILE {
    x i32
}

fn main(argc i32, argv addr) i32 {
    f FILE = {}
    return f.x
}
FAS
cat >clash-macro-const.fas <<'FAS'
use "C" <stdio.h> as c

const EOF i32 = 1

fn main(argc i32, argv addr) i32 {
    return EOF + c.EOF
}
FAS

expect_coexists remove "$ROOT/test/c_header_clash.fas"
expect_coexists remove clash-function.fas
expect_coexists stdin clash-global.fas
expect_coexists FILE clash-struct-tag.fas
expect_coexists EOF clash-macro-const.fas

for case in clash-function clash-global clash-struct-tag clash-macro-const; do
  "$OCAML_FAS" "$case.fas" -o "$case" >"$case.build" 2>&1 \
    || fail "$case did not build: $(cat "$case.build")"
  status=0
  "./$case" || status=$?
  [ "$status" -eq 0 ] || fail "$case exited $status"
done

cat >clash-block.fas <<'FAS'
use "C" as c <<C
int remove_it(int x) { return x + 1; }
C

fn remove_it(x i32) i32 { return x }
fn main() i32 { return c.remove_it(1) - remove_it(2) }
FAS
expect_coexists remove_it clash-block.fas
"$OCAML_FAS" clash-block.fas -o clash-block >clash-block.build 2>&1 \
  || fail "clash-block did not build: $(cat clash-block.build)"
./clash-block || fail "clash-block called the wrong remove_it"

cat >same-file.fas <<'FAS'
fn twice(x i32) i32 { return x }
fn twice(x i32) i32 { return x }
fn main() i32 { return 0 }
FAS
if "$OCAML_FAS" check same-file.fas >same-file.out 2>&1; then
  fail "same-file duplicate was accepted"
fi
grep -F 'duplicate function `twice`' same-file.out >/dev/null \
  || fail "same-file duplicate lost its message"
grep -F 'also declared by' same-file.out >/dev/null \
  && fail "same-file duplicate gained a C header note"

echo 'c_header_clash: Fas names and C names of the same spelling coexist: ok'
