#!/bin/sh
# `--emit-ir` prints the lowered module as Fir text (src/fir/SPEC.md; roadmap
# v0.6 replaced the old custom IR dump and its JSON form). For every test
# program it compiles, the dump must parse, verify and print back to the same
# text (fas-fir --check), and be the same on a second run and through -o.
# FAS_FIR_TOOL names fas-fir; by default the dune build.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
FAS_FIR_TOOL=${FAS_FIR_TOOL:-$ROOT/_build/default/tools/fas_fir/fas_fir.exe}
EMIT_IR_TMP=$(mktemp -d)
trap 'rm -rf "$EMIT_IR_TMP"' EXIT HUP INT TERM

emit_one() {
  source=$1
  shift
  case "$source" in
    "$ROOT/test/assembly_linkage.fas") set -- -DASSEMBLY_TABLE_VALUE=42 "$@" ;;
    "$ROOT/test/export_nested_headers.fas") set -- -I "$ROOT/test" "$@" ;;
  esac
  "$OCAML_FAS" "$@" --emit-ir "$source"
}

mkdir "$EMIT_IR_TMP/collisions"
cat >"$EMIT_IR_TMP/collisions/duplicate_adapters.fas" <<'EOF'
use "C" as c <<A
static inline int oracle(int x) { return x + 1; }
A
use "C" as c <<B
static inline int oracle(int x) { return x + 2; }
B
fn main() i32 { return c.oracle(1) }
EOF
cat >"$EMIT_IR_TMP/collisions/function_name.fas" <<'EOF'
use "C" as c <<C
static inline int oracle(int x) { return x + 1; }
C
fn oracle(x i32) i32 { return x }
fn main() i32 { return c.oracle(1) + oracle(1) - 3 }
EOF
cat >"$EMIT_IR_TMP/collisions/global_name.fas" <<'EOF'
use "C" as c <<C
static inline int oracle(int x) { return x + 1; }
C
var oracle i32 = 3
fn main() i32 { return c.oracle(1) + oracle - 5 }
EOF
cat >"$EMIT_IR_TMP/collisions/extern_name.fas" <<'EOF'
use "C" as c <<C
static inline int oracle(int x) { return x + 1; }
C
extern "C" { fn oracle(x i32) i32 { return x } }
fn main() i32 { return oracle(1) }
EOF
mkdir "$EMIT_IR_TMP/collisions/dependencies"
cat >"$EMIT_IR_TMP/collisions/dependencies/a.fas" <<'EOF'
use "C" as c <<C
static inline int oracle(int x) { return x + 1; }
C
fn a_oracle(x i32) i32 { return c.oracle(x) }
EOF
cat >"$EMIT_IR_TMP/collisions/dependencies/b.fas" <<'EOF'
use "C" as c <<C
static inline int oracle(int x) { return x + 2; }
C
fn b_oracle(x i32) i32 { return c.oracle(x) }
EOF
cat >"$EMIT_IR_TMP/collisions/dependencies/root.fas" <<'EOF'
use "a.fas"
use "b.fas"
fn main() i32 { return a_oracle(1) + b_oracle(1) - 5 }
EOF

expect_rejection() {
  source=$1
  expected=$2
  if "$OCAML_FAS" --emit-ir "$source" >"$EMIT_IR_TMP/rejected.fir" \
    2>"$EMIT_IR_TMP/stderr"; then
    echo "emit-ir: collision unexpectedly compiled: $source" >&2
    exit 1
  fi
  grep -F "$expected" "$EMIT_IR_TMP/stderr" >/dev/null || {
    cat "$EMIT_IR_TMP/stderr" >&2
    echo "emit-ir: collision diagnostic did not contain: $expected" >&2
    exit 1
  }
}

expect_rejection "$EMIT_IR_TMP/collisions/duplicate_adapters.fas" \
  "C compilation failed: redefinition of 'oracle'"
expect_rejection "$EMIT_IR_TMP/collisions/extern_name.fas" \
  "C compilation failed: static declaration of 'oracle' follows non-static declaration"

# C namespaces: a Fas name and a C name of the same spelling coexist; each
# program verifies as Fir and runs to exit 0
expect_coexistence() {
  source=$1
  "$OCAML_FAS" --emit-ir "$source" >"$EMIT_IR_TMP/coexist.fir" \
    2>"$EMIT_IR_TMP/stderr" || {
    cat "$EMIT_IR_TMP/stderr" >&2
    echo "emit-ir: same-spelled names were rejected: $source" >&2
    exit 1
  }
  "$FAS_FIR_TOOL" --check "$EMIT_IR_TMP/coexist.fir" >/dev/null || {
    echo "emit-ir: the dump of $source does not parse or verify" >&2
    exit 1
  }
  "$OCAML_FAS" "$source" -o "$EMIT_IR_TMP/coexist" || exit 1
  "$EMIT_IR_TMP/coexist" || {
    echo "emit-ir: $source called the wrong \`oracle\`" >&2
    exit 1
  }
}
expect_coexistence "$EMIT_IR_TMP/collisions/function_name.fas"
expect_coexistence "$EMIT_IR_TMP/collisions/global_name.fas"
# imported C names are program-wide (one C entity, one name), so two files
# whose C defines one name differently still conflict
expect_rejection "$EMIT_IR_TMP/collisions/dependencies/root.fas" \
  'C declaration `c.oracle` is not supported: conflicting C declarations'

count=0
skipped=0
for source in "$ROOT"/test/*.fas; do
  dump="$EMIT_IR_TMP/current.fir"
  if emit_one "$source" >"$dump" 2>"$EMIT_IR_TMP/stderr"; then
    [ ! -s "$EMIT_IR_TMP/stderr" ] || {
      cat "$EMIT_IR_TMP/stderr" >&2
      echo "emit-ir: diagnostics on success: $source" >&2
      exit 1
    }
    if ! "$FAS_FIR_TOOL" --check "$dump" >"$EMIT_IR_TMP/reprinted.fir" 2>"$EMIT_IR_TMP/stderr"; then
      cat "$EMIT_IR_TMP/stderr" >&2
      echo "emit-ir: the dump of $source does not parse or verify" >&2
      exit 1
    fi
    cmp -s "$dump" "$EMIT_IR_TMP/reprinted.fir" || {
      diff "$dump" "$EMIT_IR_TMP/reprinted.fir" | head -20 >&2
      echo "emit-ir: the dump of $source does not print back the same" >&2
      exit 1
    }
    count=$((count + 1))
  else
    skipped=$((skipped + 1))
  fi
done

emit_one "$ROOT/test/ir_json.fas" >"$EMIT_IR_TMP/out.fir"
emit_one "$ROOT/test/ir_json.fas" >"$EMIT_IR_TMP/repeat.fir"
cmp "$EMIT_IR_TMP/out.fir" "$EMIT_IR_TMP/repeat.fir"
"$OCAML_FAS" --emit-ir "$ROOT/test/ir_json.fas" -o "$EMIT_IR_TMP/file.fir"
cmp "$EMIT_IR_TMP/out.fir" "$EMIT_IR_TMP/file.fir"

echo "emit-ir: $count compilable source files gave valid Fir; $skipped were rejected"
