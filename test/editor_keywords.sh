#!/bin/sh
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
GRAMMAR_DIR="$ROOT/editors/tree-sitter-fas"
LEXER="$ROOT/src/fas/lexer.ml"
NAMES="$ROOT/src/fas/names.ml"
GRAMMAR="$GRAMMAR_DIR/grammar.js"
HIGHLIGHTS="$GRAMMAR_DIR/queries/highlights.scm"
GATE_TMP=$(mktemp -d)
trap 'rm -rf "$GATE_TMP"' EXIT HUP INT TERM

fail() {
  echo "editor keywords: $*" >&2
  exit 1
}

nonempty() {
  [ -s "$1" ] || fail "$2 is empty"
}

same_set() {
  sort -u "$1" > "$1.sorted"
  sort -u "$2" > "$2.sorted"
  if ! diff -u "$1.sorted" "$2.sorted" > "$GATE_TMP/diff.txt"; then
    cat "$GATE_TMP/diff.txt" >&2
    fail "$3 differs"
  fi
}

ml_block() {
  awk -v s="$2" 'index($0, s) == 1 { on = 1 } on { print } on && /\]/ { exit }' "$1" |
    grep -o '"[^"]*"' | tr -d '"'
}

js_block() {
  awk -v s="$2" 'index($0, s) == 1 { on = 1 } on { print } on && /\];/ { exit }' "$1" |
    grep -o "'[^']*'" | tr -d "'"
}

highlight_block() {
  awk '
    /^\[$/ { buf = ""; inb = 1; next }
    inb && /^\] @keyword$/ { printf "%s", buf; inb = 0; next }
    inb && /^\]/ { inb = 0; next }
    inb { buf = buf $0 "\n" }
  ' "$1" | grep -o '"[^"]*"' | tr -d '"'
}

sed -n 's/^[[:space:]]*| "\([a-z]*\)" -> \(Token\.\)*Kw_.*$/\1/p' "$LEXER" > "$GATE_TMP/lexer_keywords"
nonempty "$GATE_TMP/lexer_keywords" "lexer keyword table"

printf '%s\n' view > "$GATE_TMP/contextual_view"

ml_block "$NAMES" "let primitive_type_names" > "$GATE_TMP/names_contextual"
ml_block "$NAMES" "let scalar_type_names" > "$GATE_TMP/names_scalar"
ml_block "$NAMES" "let reserved_float_type_names" > "$GATE_TMP/names_float"
ml_block "$NAMES" "let literal_names" > "$GATE_TMP/names_literal"
ml_block "$NAMES" "let reserved_float_names" > "$GATE_TMP/names_float_math"
ml_block "$NAMES" "let operations =" > "$GATE_TMP/names_operations"
for name in names_contextual names_scalar names_float names_literal names_float_math names_operations; do
  nonempty "$GATE_TMP/$name" "$name"
done

highlight_block "$HIGHLIGHTS" > "$GATE_TMP/highlight_keywords"
nonempty "$GATE_TMP/highlight_keywords" "highlight keyword block"

sed -n '/#any-of? @function.builtin/,/))$/p' "$HIGHLIGHTS" |
  grep -o '"[^"]*"' | tr -d '"' > "$GATE_TMP/highlight_builtins"
nonempty "$GATE_TMP/highlight_builtins" "highlight builtin list"

js_block "$GRAMMAR" "const keywords" > "$GATE_TMP/grammar_keywords"
js_block "$GRAMMAR" "const primitive_types" > "$GATE_TMP/grammar_primitives"
nonempty "$GATE_TMP/grammar_keywords" "grammar keywords"
nonempty "$GATE_TMP/grammar_primitives" "grammar primitive types"

cat "$GATE_TMP/lexer_keywords" "$GATE_TMP/contextual_view" > "$GATE_TMP/expected_highlight_keywords"
same_set "$GATE_TMP/highlight_keywords" "$GATE_TMP/expected_highlight_keywords" "highlight keyword block"

cat "$GATE_TMP/grammar_keywords" "$GATE_TMP/grammar_primitives" > "$GATE_TMP/grammar_all_keywords"
cat "$GATE_TMP/lexer_keywords" "$GATE_TMP/contextual_view" "$GATE_TMP/names_contextual" \
  "$GATE_TMP/names_scalar" "$GATE_TMP/names_float" "$GATE_TMP/names_literal" > "$GATE_TMP/expected_grammar_keywords"
same_set "$GATE_TMP/grammar_all_keywords" "$GATE_TMP/expected_grammar_keywords" "grammar keyword set"

cat "$GATE_TMP/names_scalar" "$GATE_TMP/names_float" > "$GATE_TMP/expected_primitives"
same_set "$GATE_TMP/grammar_primitives" "$GATE_TMP/expected_primitives" "grammar primitive types"

cat "$GATE_TMP/names_operations" "$GATE_TMP/names_float_math" > "$GATE_TMP/expected_builtins"
same_set "$GATE_TMP/highlight_builtins" "$GATE_TMP/expected_builtins" "highlight builtin list"

grep -q '^(boolean) @' "$HIGHLIGHTS" || fail "highlights lack a (boolean) capture"
grep -q '^(null) @' "$HIGHLIGHTS" || fail "highlights lack a (null) capture"
grep -q '^(pkg_declaration "pkg" @keyword)$' "$HIGHLIGHTS" || fail "highlights lack the pkg declaration keyword"

echo "editor keywords: $(wc -l < "$GATE_TMP/lexer_keywords" | tr -d ' ') lexer keywords, $(wc -l < "$GATE_TMP/expected_builtins.sorted" | tr -d ' ') builtins agree with the compiler"
