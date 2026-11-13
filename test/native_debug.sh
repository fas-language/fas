#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
# Native -g (DWARF line tables, src/backend/mir/mir_dwarf.ml):
#   1. -g never changes the program: every test/*.fas object built with -g
#      has the same allocated sections (bytes and relocations) and symbols
#      as without, and executables are byte-identical once both are stripped
#      of debug sections (objcopy --strip-debug);
#   2. the line tables say what the source says: test/fas_fir/lines.fir
#      (two files, a location only on a fall-through jump) and a Fas program
#      decode (readelf --debug-dump=decodedline) to the expected file:line
#      rows, and the compile unit is named after the main file;
#   3. llvm-dwarfdump --verify accepts them, gdb stops at file:line and
#      prints the variables of test/fas_fir/vars.fir (in frame slots, in
#      registers, spilled, behind a pointer, shadowed in a lexical block),
#      when those tools are installed (optional).
# Needs binutils (readelf, objdump, objcopy, nm); no LLVM, no C compiler.
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
FAS_FIR_TOOL=${FAS_FIR_TOOL:-$ROOT/_build/default/tools/fas_fir/fas_fir.exe}
DBG_TMP=$(mktemp -d)
trap 'rm -rf "$DBG_TMP"' EXIT HUP INT TERM
fail=0
bad() { echo "native debug: $*" >&2; fail=1; }

# allocated sections' bytes and relocations, and the symbols outside the
# debug sections (relocations by symbol name: two more local symbols shift
# the indices)
alloc_view() {
  for s in $(objdump -h "$1" | awk '/^ *[0-9]+ / { name = $2; next } name != "" && /ALLOC/ { print name } { name = "" }'); do
    echo "== $s"
    objdump -s -j "$s" "$1" | tail -n +5
    objdump -r -j "$s" "$1" | tail -n +5
  done
  nm "$1" | grep -v ' \.Lfas\.debug_'
}

# 1. objects
objects=0
for f in "$ROOT"/test/*.fas; do
  b=$(basename "$f" .fas)
  if ! "$OCAML_FAS" --backend=native -c "$f" -o "$DBG_TMP/$b.o" >/dev/null 2>&1; then
    if "$OCAML_FAS" --backend=native -g -c "$f" -o "$DBG_TMP/$b.g.o" >/dev/null 2>&1; then
      bad "$b.fas: refused without -g, accepted with"
    fi
    continue
  fi
  if ! "$OCAML_FAS" --backend=native -g -c "$f" -o "$DBG_TMP/$b.g.o" >/dev/null 2>"$DBG_TMP/err"; then
    bad "$b.fas: refused with -g: $(head -3 "$DBG_TMP/err")"; continue
  fi
  objects=$((objects + 1))
  alloc_view "$DBG_TMP/$b.o" >"$DBG_TMP/a"
  alloc_view "$DBG_TMP/$b.g.o" >"$DBG_TMP/b"
  cmp -s "$DBG_TMP/a" "$DBG_TMP/b" || bad "$b.fas: -g changes the object's code or data"
  if command -v llvm-dwarfdump >/dev/null 2>&1; then
    llvm-dwarfdump --verify "$DBG_TMP/$b.g.o" >/dev/null 2>&1 || bad "$b.fas: llvm-dwarfdump --verify fails"
  fi
done
[ "$objects" -gt 100 ] || bad "only $objects objects compared"

# executables: same output, identical once stripped
same_exe() {
  name=$1 plain=$2 debug=$3
  "$plain" >"$DBG_TMP/out1" 2>&1 && rc1=0 || rc1=$?
  "$debug" >"$DBG_TMP/out2" 2>&1 && rc2=0 || rc2=$?
  [ "$rc1" = "$rc2" ] && cmp -s "$DBG_TMP/out1" "$DBG_TMP/out2" || bad "$name: -g changes the output"
  # both through objcopy, which rewrites the layout even when it removes
  # nothing
  objcopy --strip-debug "$plain" "$DBG_TMP/stripped1"
  objcopy --strip-debug "$debug" "$DBG_TMP/stripped2"
  cmp -s "$DBG_TMP/stripped1" "$DBG_TMP/stripped2" || bad "$name: -g executable differs once stripped"
  readelf -S "$debug" | grep -q '\.debug_line' || bad "$name: no .debug_line with -g"
  if readelf -S "$plain" | grep -q '\.debug_'; then bad "$name: debug sections without -g"; fi
}
"$FAS_FIR_TOOL" -c "$ROOT/test/fas_fir/putnum.fir" -o "$DBG_TMP/putnum.o"
for pie in "" --no-pie; do
  "$FAS_FIR_TOOL" $pie "$ROOT/test/fas_fir/lines.fir" "$DBG_TMP/putnum.o" -o "$DBG_TMP/lines"
  "$FAS_FIR_TOOL" -g $pie "$ROOT/test/fas_fir/lines.fir" "$DBG_TMP/putnum.o" -o "$DBG_TMP/lines.g"
  same_exe "lines.fir${pie:+ $pie}" "$DBG_TMP/lines" "$DBG_TMP/lines.g"
  "$FAS_FIR_TOOL" $pie "$ROOT/test/fas_fir/vars.fir" "$DBG_TMP/putnum.o" -o "$DBG_TMP/vars"
  "$FAS_FIR_TOOL" -g $pie "$ROOT/test/fas_fir/vars.fir" "$DBG_TMP/putnum.o" -o "$DBG_TMP/vars.g"
  same_exe "vars.fir${pie:+ $pie}" "$DBG_TMP/vars" "$DBG_TMP/vars.g"
done
for pie in "" -Wl,-no-pie; do
  "$OCAML_FAS" --backend=native $pie "$ROOT/test/native_backend.fas" -o "$DBG_TMP/nb"
  "$OCAML_FAS" --backend=native -g $pie "$ROOT/test/native_backend.fas" -o "$DBG_TMP/nb.g"
  same_exe "native_backend.fas${pie:+ $pie}" "$DBG_TMP/nb" "$DBG_TMP/nb.g"
done

# 2. the rows of lines.fir: "file line" in address order, one per row
# (the end of each sequence has no line); line 9 is a jump that falls
# through, so it has no code and no row
readelf --debug-dump=decodedline "$DBG_TMP/lines.g" \
  | awk '$2 ~ /^[0-9]+$/ && $3 ~ /^0x/ { print $1, $2 }' >"$DBG_TMP/rows"
printf '%s\n' "helper.h 7" "helper.h 7" "lines.c 3" "lines.c 3" "lines.c 4" "lines.c 10" "lines.c 11" \
  >"$DBG_TMP/want"
cmp -s "$DBG_TMP/rows" "$DBG_TMP/want" || { bad "lines.fir: line rows differ"; diff "$DBG_TMP/want" "$DBG_TMP/rows" >&2 || true; }
readelf --debug-dump=info "$DBG_TMP/lines.g" | grep -A3 'DW_TAG_compile_unit' | grep -q 'lines\.c' \
  || bad "lines.fir: the compile unit is not named lines.c"
for fn in twice main; do
  readelf --debug-dump=info "$DBG_TMP/lines.g" | grep -q "DW_AT_name.*: $fn\$" || bad "lines.fir: no subprogram $fn"
done
readelf --debug-dump=decodedline "$DBG_TMP/nb.g" | grep -q '^native_backend\.fas  *[0-9]' \
  || bad "native_backend.fas: no rows for the source file"

# 3. optional tools
if command -v llvm-dwarfdump >/dev/null 2>&1; then
  for x in lines.g vars.g nb.g; do
    llvm-dwarfdump --verify "$DBG_TMP/$x" >/dev/null 2>&1 || bad "$x: llvm-dwarfdump --verify fails"
  done
fi
if command -v gdb >/dev/null 2>&1; then
  out=$(cd "$DBG_TMP" && gdb -nx -q -batch -ex 'break lines.c:10' -ex run -ex bt ./lines.g 2>&1 || true)
  echo "$out" | grep -q '^#0  main () at .*lines\.c:10' || bad "gdb: no stop at lines.c:10 in main: $out"
  # variables (vars.fir): the second stop at vars.c:7, inner block first;
  # also with every promoted local spilled (FAS_MIR_REGS)
  for regs in "" 3; do
    FAS_MIR_REGS=$regs "$FAS_FIR_TOOL" -g "$ROOT/test/fas_fir/vars.fir" "$DBG_TMP/putnum.o" -o "$DBG_TMP/vars.r"
    out=$(cd "$DBG_TMP" && gdb -nx -q -batch -ex 'break vars.c:7' -ex run -ex continue -ex 'info locals' \
      -ex 'info args' -ex 'print *ppt' ./vars.r 2>&1 || true)
    got=$(echo "$out" | grep -E '^(x|i|sum|pt|w|n|\$1) = ' | grep -v '^ppt' | tr '\n' ';')
    want='x = 30;i = 2;sum = 3;x = 100;pt = {x = 3, y = 4};w = 7;n = 3;$1 = {x = 3, y = 4};'
    [ "$got" = "$want" ] || bad "gdb variables${regs:+ (FAS_MIR_REGS=$regs)}: got $got, expected $want"
  done
fi

[ $fail = 0 ] && echo "native debug: $objects objects and 6 executables unchanged by -g, line tables ok"
exit $fail
