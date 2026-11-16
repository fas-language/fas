#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

fail() {
  echo "c_macro_names: $*" >&2
  exit 1
}

cat >"$TMP/macros.h" <<'C'
#define SAME_I 7
#define SAME_F 2.5
C

# C namespaces: a macro is `c.NAME`, so a Fas name of the same spelling
# (in the importing file or in a file that imports it) is a different
# entity, and both keep their values
cat >"$TMP/same-int.fas" <<'FAS'
use "C" "macros.h" as c
const SAME_I i32 = 1
fn main() i32 { return SAME_I + c.SAME_I - 8 }
FAS
cat >"$TMP/same-float.fas" <<'FAS'
use "C" "macros.h" as c
fn SAME_F() f64 { return 1.0 }
fn main() i32 { return cvt[i32](SAME_F() + c.SAME_F) - 3 }
FAS
cat >"$TMP/lib.fas" <<'FAS'
use "C" "macros.h" as c
fn lib_same_i() i32 { return c.SAME_I }
FAS
cat >"$TMP/cross-int.fas" <<'FAS'
use "lib.fas"
const SAME_I i32 = 1
fn main() i32 { return SAME_I + lib_same_i() - 8 }
FAS
cat >"$TMP/cross-float.fas" <<'FAS'
use "lib.fas"
fn SAME_F() f64 { return 1.0 }
fn main() i32 { return cvt[i32](SAME_F()) + lib_same_i() - 8 }
FAS

for source in same-int same-float cross-int cross-float; do
  "$OCAML_FAS" -O0 "$TMP/$source.fas" -o "$TMP/$source" >"$TMP/$source.out" 2>&1 \
    || fail "$source: Fas name and C macro should coexist: $(cat "$TMP/$source.out")"
  timeout 30 "$TMP/$source" || fail "$source: a Fas name or a C macro changed value"
done

# a C variable shadowed by an empty macro of the same name: the importer's
# type probe for the variable expanded the macro and failed (fas_refuse
# seed 11 input 9778, an internal error)
cat >"$TMP/shadowed.fas" <<'FAS'
use "C" as c <<C
int counter = 37;
#define counter
C
fn main() i32 {
  if c.counter != 37 { return 1 }
  return 0
}
FAS
"$OCAML_FAS" -O0 "$TMP/shadowed.fas" -o "$TMP/shadowed" >"$TMP/shadowed.out" 2>&1 \
  || fail "shadowed: a variable behind a same-named macro: $(cat "$TMP/shadowed.out")"
timeout 30 "$TMP/shadowed" || fail "shadowed: the variable lost its value"

cat >"$TMP/accepted.fas" <<'FAS'
use "C" "macros.h" as c
fn main() i32 {
  integer i32 = c.SAME_I
  floating f64 = c.SAME_F
  return integer - 7
}
FAS
"$OCAML_FAS" -O0 "$TMP/accepted.fas" -o "$TMP/accepted"
timeout 30 "$TMP/accepted" || fail "accepted integer and float macros changed values"

cat >"$TMP/fields.h" <<'C'
struct vt { int SetPort; long Other; };
typedef struct { short Other; } td;
#define SetPort SetPortA
#define Other 3
C
cat >"$TMP/fields.fas" <<'FAS'
use "C" <stdio.h> as c
use "C" "fields.h" as c
fn main() i32 {
  v c.vt = {}
  t c.td = {}
  v.SetPort = 5
  v.Other = 6
  t.Other = -2
  c.printf(c"%d %ld %d %d %zu %zu\n", v.SetPort, v.Other, t.Other, c.Other, sizeof[c.vt], sizeof[c.td])
  return 0
}
FAS
cat >"$TMP/fields.c" <<'C'
#include <stdio.h>
struct vt { int SetPort; long Other; };
typedef struct { short Other; } td;
int main(void) {
  struct vt v = {0};
  td t = {0};
  v.SetPort = 5;
  v.Other = 6;
  t.Other = -2;
  printf("%d %ld %d %d %zu %zu\n", v.SetPort, v.Other, t.Other, 3, sizeof(struct vt), sizeof(td));
  return 0;
}
C
"$CC" "$TMP/fields.c" -o "$TMP/fields-c"
"$TMP/fields-c" >"$TMP/fields.expected"
for level in O0 O2; do
  "$OCAML_FAS" "-$level" --emit-llvm "$TMP/fields.fas" -o "$TMP/fields-$level.ll"
  "${LLVM_OPT:-opt-22}" -passes=verify -disable-output "$TMP/fields-$level.ll" \
    || fail "field named like a macro produced invalid LLVM at $level"
  "$OCAML_FAS" "-$level" "$TMP/fields.fas" -o "$TMP/fields-$level"
  timeout 30 "$TMP/fields-$level" >"$TMP/fields-$level.out"
  cmp -s "$TMP/fields.expected" "$TMP/fields-$level.out" \
    || fail "field named like a macro changed values at $level"
done

cat >"$TMP/broken.h" <<'C'
struct many { int f0, f1, f2, f3, f4, f5, f6, f7, f8, f9; };
#define f3 f3x
#define GOOD 9
#define BAD undeclared_name
C
cat >"$TMP/broken.fas" <<'FAS'
use "C" "broken.h" as c
fn main() i32 {
  m c.many = {}
  m.f3 = c.GOOD
  return m.f3 - c.BAD
}
FAS
if "$OCAML_FAS" --emit-llvm "$TMP/broken.fas" -o "$TMP/broken.ll" \
  >"$TMP/broken.out" 2>&1; then
  fail "unusable macro next to fields named like macros was accepted"
fi
grep -F 'unknown name `c.BAD`' "$TMP/broken.out" >/dev/null \
  || fail "unusable macro next to fields named like macros was not reported as unknown"

cat >"$TMP/no-header.fas" <<'FAS'
fn main() i32 { return 0 }
FAS
cat >"$TMP/cc-wrap" <<'SH'
#!/bin/sh
printf '%s\n' "$*" >>"$TOOL_LOG"
exec "$REAL_CC" "$@"
SH
chmod +x "$TMP/cc-wrap"
: >"$TMP/cc.log"
TOOL_LOG="$TMP/cc.log" REAL_CC="$CC" CC="$TMP/cc-wrap" \
  "$OCAML_FAS" --emit-llvm "$TMP/no-header.fas" >"$TMP/no-header.ll"
test ! -s "$TMP/cc.log" || fail "no-header program ran extra C preprocessing"

echo 'c_macro_names: Fas names next to C macros, accepted macros, fields named like macros, and no-header imports: ok'
