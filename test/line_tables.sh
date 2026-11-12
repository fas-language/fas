#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_OBJCOPY=${LLVM_OBJCOPY:-llvm-objcopy-22}
LLVM_READELF=${LLVM_READELF:-llvm-readelf-22}
LLVM_DWARFDUMP=${LLVM_DWARFDUMP:-llvm-dwarfdump-22}
LLVM_OBJDUMP=${LLVM_OBJDUMP:-llvm-objdump-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
LT_TMP=$(mktemp -d)
trap 'rm -rf "$LT_TMP"' EXIT HUP INT TERM

fail() {
  echo "line tables: $*" >&2
  exit 1
}

allocated_sections() {
  "$LLVM_READELF" -S -W "$1" | sed -E 's/^ *\[ *[0-9]+\]//' | awk '
    $1 ~ /^\./ && $1 !~ /^\.(debug_|rela\.debug_|symtab$|strtab$|shstrtab$|comment$|note)/ {
      flags = ""
      if ($(NF - 3) ~ /^[A-Za-z]+$/) flags = $(NF - 3)
      if (flags ~ /A/) print ($1 == ".eh_frame" ? "eh" : "core"), $1, $2, $5
    }'
}

allocated_relocations() {
  "$LLVM_READELF" -r -W "$1" | awk '
    /^Relocation section/ {
      skip = ($0 ~ /debug/)
      kind = ($0 ~ /eh_frame/) ? "eh" : "core"
      next
    }
    !skip && length($1) == 16 && $1 ~ /^[0-9a-f]+$/ { print kind, $1, $3, $5, $6, $7 }'
}

SRC="$LT_TMP/src"
mkdir "$SRC"
cat >"$SRC/prog.fas" <<'EOF'
fn scale(x i64) i64 {
    y i64 = x * 3
    return y
}

fn divide(a i64, b i64) i64 {
    q i64 = a / b
    return q
}

fn main(argc i32, argv addr) i32 {
    n i64 = sext[i64](argc)
    total i64 = 0
    for i i64 = 0; i < n + 4; i += 1 {
        total = total ^ scale(i)
    }
    q i64 = divide(total, n - 1)
    if q == 0 {
        return 0
    }
    return 1
}
EOF
SCALE_LINE=$(grep -n 'y i64 = x \* 3' "$SRC/prog.fas" | cut -d: -f1)
BODY_LINE=$(grep -n 'total = total ^ scale(i)' "$SRC/prog.fas" | cut -d: -f1)
DIV_LINE=$(grep -n 'q i64 = a / b' "$SRC/prog.fas" | cut -d: -f1)

compared=0
zero_compared=0
skipped=0
eh_only=0
sections_compared=0
relocation_rows=0
for file in "$ROOT"/test/*.fas; do
  name=$(basename "$file" .fas)
  for level in 0 1 2 3; do
    plain="$LT_TMP/$name-O$level.o"
    debug="$LT_TMP/$name-O$level-g.o"
    if ! "$OCAML_FAS" -O"$level" -c "$file" -o "$plain" >"$LT_TMP/plain.log" 2>&1; then
      if "$OCAML_FAS" -g -O"$level" -c "$file" -o "$debug" >/dev/null 2>&1; then
        fail "$name -O$level: -g builds where the plain build fails"
      fi
      skipped=$((skipped + 1))
      continue
    fi
    "$OCAML_FAS" -g -O"$level" -c "$file" -o "$debug" ||
      fail "$name -O$level: -g build failed"
    allocated_sections "$plain" >"$LT_TMP/plain.sec"
    allocated_sections "$debug" >"$LT_TMP/debug.sec"
    allocated_relocations "$plain" >"$LT_TMP/plain.rel"
    allocated_relocations "$debug" >"$LT_TMP/debug.rel"
    eh_differs=0
    for part in sec rel; do
      awk '$1 == "core"' "$LT_TMP/plain.$part" >"$LT_TMP/plain.$part.core"
      awk '$1 == "core"' "$LT_TMP/debug.$part" >"$LT_TMP/debug.$part.core"
      if ! cmp -s "$LT_TMP/plain.$part.core" "$LT_TMP/debug.$part.core"; then
        diff "$LT_TMP/plain.$part.core" "$LT_TMP/debug.$part.core" >&2 || true
        fail "$name -O$level: allocated $part outside .eh_frame differ with -g"
      fi
      awk '$1 == "eh"' "$LT_TMP/plain.$part" >"$LT_TMP/plain.$part.eh"
      awk '$1 == "eh"' "$LT_TMP/debug.$part" >"$LT_TMP/debug.$part.eh"
      if ! cmp -s "$LT_TMP/plain.$part.eh" "$LT_TMP/debug.$part.eh"; then
        if [ "$level" -eq 0 ]; then
          diff "$LT_TMP/plain.$part.eh" "$LT_TMP/debug.$part.eh" >&2 || true
          fail "$name -O0: .eh_frame $part differ with -g"
        fi
        eh_differs=1
      fi
    done
    relocation_rows=$((relocation_rows + $(awk '$1 == "core"' "$LT_TMP/plain.rel" | wc -l)))
    while read -r kind section type size; do
      sections_compared=$((sections_compared + 1))
      if [ "$type" != NOBITS ]; then
        "$LLVM_OBJCOPY" --dump-section "$section=$LT_TMP/plain.bin" "$plain"
        "$LLVM_OBJCOPY" --dump-section "$section=$LT_TMP/debug.bin" "$debug"
        if ! cmp -s "$LT_TMP/plain.bin" "$LT_TMP/debug.bin"; then
          if [ "$kind" = eh ] && [ "$level" -ne 0 ]; then
            eh_differs=1
          else
            fail "$name -O$level: section $section bytes differ with -g"
          fi
        fi
      fi
    done <"$LT_TMP/plain.sec"
    if [ "$eh_differs" -eq 1 ]; then
      eh_only=$((eh_only + 1))
    fi
    compared=$((compared + 1))
    if [ "$level" -eq 0 ]; then
      zero_compared=$((zero_compared + 1))
    fi
  done
done
if [ "$compared" -eq 0 ]; then
  fail "no test programs were compared"
fi
echo "line tables: $compared file-level pairs compared: $zero_compared at -O0 fully identical (including .eh_frame), $eh_only differed only in .eh_frame at -O1 to -O3, $skipped skipped (plain build fails); $sections_compared allocated sections and $relocation_rows relocation rows outside .eh_frame compared"

"$OCAML_FAS" --emit-llvm "$SRC/prog.fas" >"$LT_TMP/plain.ll"
if grep -q 'llvm\.dbg\|!DI' "$LT_TMP/plain.ll"; then
  fail "plain --emit-llvm contains debug metadata"
fi
for source in "$SRC/prog.fas" "$ROOT/test/copy.fas" "$ROOT/test/defer_unwinding.fas" \
  "$ROOT/test/division_trap_order.fas"; do
  for flag in --emit-llvm --emit-llvm=optimized; do
    "$OCAML_FAS" -g "$flag" "$source" >"$LT_TMP/ir.ll"
    "$LLVM_OPT" -passes=verify "$LT_TMP/ir.ll" -disable-output
    if ! grep -q '!DILocation' "$LT_TMP/ir.ll"; then
      fail "$flag with -g has no DILocation: $source"
    fi
  done
done
echo "line tables: debug IR verified for 4 programs at --emit-llvm and --emit-llvm=optimized"

"$OCAML_FAS" -g -O2 -c "$SRC/prog.fas" -o "$LT_TMP/prog-O2.o"
"$LLVM_DWARFDUMP" --debug-line "$LT_TMP/prog-O2.o" >"$LT_TMP/lines.txt"
grep -q 'name: "prog.fas"' "$LT_TMP/lines.txt" || fail "debug line table lacks prog.fas"
grep -Eq "^0x[0-9a-f]+ +$BODY_LINE +" "$LT_TMP/lines.txt" ||
  fail "debug line table lacks loop body line $BODY_LINE"
echo "line tables: -O2 debug line table lists prog.fas and loop body line $BODY_LINE"

cat >"$SRC/loops.fas" <<'EOF'
fn sum_while(n i64) i64 {
    total i64 = 0
    i i64 = 0
    while i < n {
        total = total + i
        i = i + 1
    }
    return total
}

fn sum_for(n i64) i64 {
    total i64 = 0
    i i64 = 0
    for ; i < n; {
        total = total + i
        i = i + 1
    }
    return total
}

fn main(argc i32, argv addr) i32 {
    a i64 = sum_while(sext[i64](argc))
    b i64 = sum_for(sext[i64](argc))
    if a == b {
        return 0
    }
    return 1
}
EOF
"$OCAML_FAS" -g -O0 -c "$SRC/loops.fas" -o "$LT_TMP/loops.o"
WHILE_LINE=$(grep -n 'while i < n {' "$SRC/loops.fas" | cut -d: -f1)
FOR_LINE=$(grep -n 'for ; i < n; {' "$SRC/loops.fas" | cut -d: -f1)
for loop in sum_while:$WHILE_LINE sum_for:$FOR_LINE; do
  name=${loop%%:*}
  want=${loop#*:}
  got=$("$LLVM_OBJDUMP" -d -l --no-show-raw-insn --disassemble-symbols="$name" "$LT_TMP/loops.o" | awk -v name="$name" '
    /^[0-9a-f]+ <.*>:$/ { cur = $2; gsub(/[<>:]/, "", cur) }
    cur == name && /\.fas:[0-9]+$/ { line = $NF; sub(/.*\.fas:/, "", line) }
    cur == name && /[[:space:]]jmp[[:space:]]/ { last = line }
    END { print last }')
  [ "$got" = "$want" ] || fail "$name backedge is on line ${got:-none}, expected loop line $want"
done
echo "line tables: -O0 loop backedges carry the while and for loop lines"

if ! command -v gdb >/dev/null 2>&1; then
  echo "line tables: gdb not found, debugger checks skipped" >&2
  exit 0
fi

"$OCAML_FAS" -g -O0 "$SRC/prog.fas" -o "$LT_TMP/prog-O0"
"$OCAML_FAS" -g -O2 "$SRC/prog.fas" -o "$LT_TMP/prog-O2"
GDB="gdb -batch -nx"

(cd "$LT_TMP" && $GDB -ex "break prog.fas:$SCALE_LINE" -ex run -ex bt ./prog-O0 \
  >"$LT_TMP/break.out" 2>&1) || true
grep -Eq "^#0 .* scale \(\) at prog\.fas:$SCALE_LINE\$" "$LT_TMP/break.out" ||
  { cat "$LT_TMP/break.out" >&2; fail "gdb breakpoint did not stop in scale at line $SCALE_LINE"; }
grep -Eq "^#1 .* main \(\) at prog\.fas:$BODY_LINE\$" "$LT_TMP/break.out" ||
  { cat "$LT_TMP/break.out" >&2; fail "gdb caller frame is not main at line $BODY_LINE"; }

(cd "$LT_TMP" && $GDB -ex run -ex bt ./prog-O0 >"$LT_TMP/trap-O0.out" 2>&1) || true
grep -Eq "^#0 .* in divide \(\) at prog\.fas:$DIV_LINE\$" "$LT_TMP/trap-O0.out" ||
  { cat "$LT_TMP/trap-O0.out" >&2; fail "-O0 trap frame is not divide at line $DIV_LINE"; }

(cd "$LT_TMP" && $GDB -ex run -ex 'info line *$pc' ./prog-O2 >"$LT_TMP/trap-O2.out" 2>&1) ||
  true
grep -Eq "^Line $DIV_LINE of \"prog\.fas\" starts at address" "$LT_TMP/trap-O2.out" ||
  { cat "$LT_TMP/trap-O2.out" >&2; fail "-O2 trap pc is not mapped to line $DIV_LINE"; }
echo "line tables: gdb breakpoint, -O0 trap frame and -O2 trap line checked"
