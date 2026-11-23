#!/usr/bin/env bash
set -euo pipefail
ulimit -f 524288
root=$(cd "$(dirname "$0")/.." && pwd)
export FCC="${FCC:-$root/_build/fcc}"
export FCC_FIR="${FCC_FIR:-$root/_build/default/tools/fas_fir/fas_fir.exe}"
export FCC_LD="${FCC_LD:-$root/_build/fas-ld}"
export FCC_INCLUDE="${FCC_INCLUDE:-$root/cc/include}"
dwarfdump="${LLVM_DWARFDUMP:-llvm-dwarfdump-22}"
gdb="${GDB:-gdb}"

work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
probe="$root/test/cc_debug/probe.c"
extra="$root/test/cc_debug/extra.c"
"$FCC" -g -c "$probe" -o "$work/probe.o"
"$FCC" -g -c "$extra" -o "$work/extra.o"
"$FCC" "$work/probe.o" "$work/extra.o" -o "$work/probe"
"$work/probe"

for object in "$work/probe.o" "$work/extra.o" "$work/probe"; do
    "$dwarfdump" --verify "$object"
    readelf -SW "$object" > "$work/sections"
    for section in debug_info debug_line debug_abbrev debug_str debug_line_str eh_frame; do
        grep -Eq "[.]$section[[:space:]]" "$work/sections"
    done
done

"$dwarfdump" --debug-info "$work/probe.o" > "$work/types"
for tag in typedef enumeration_type union_type structure_type array_type subroutine_type; do
    grep -q "DW_TAG_$tag" "$work/types"
done
grep -q 'DW_AT_name.*("Counter")' "$work/types"

"$FCC" -g -g0 -c "$probe" -o "$work/no-debug.o"
readelf -SW "$work/no-debug.o" > "$work/sections"
if grep -q '[.]debug_' "$work/sections"; then
    echo 'unexpected debug sections with -g0' >&2
    exit 1
fi

inner=$(grep -n 'inner-break' "$probe" | cut -d: -f1)
outer=$(grep -n 'outer-break' "$probe" | cut -d: -f1)
"$gdb" -q -nx -batch "$work/probe" \
    -ex 'set debuginfod enabled off' \
    -ex "break probe.c:$inner" \
    -ex "break probe.c:$outer" \
    -ex run \
    -ex 'printf "inner=%ld\n", total' \
    -ex continue \
    -ex 'printf "seed=%ld total=%ld value=%ld state=%d count=%ld offset=%d flag=%u\n", seed, total, rec.value, rec.state, rec.payload.count, rec.offsets[0], rec.flag' \
    -ex 'ptype total' -ex 'ptype adjust' -ex 'print adjust' -ex bt -ex continue > "$work/gdb"
cat "$work/gdb"
grep -qx 'inner=99' "$work/gdb"
grep -qx 'seed=40 total=49 value=42 state=1 count=13 offset=99 flag=5' "$work/gdb"
grep -Eq 'type = int \(\*\)\(int\)' "$work/gdb"
grep -Eq '\$[0-9]+ = .*<extra>' "$work/gdb"
grep -Eq '^#0 .*inspect ' "$work/gdb"
grep -Eq '^#1 .*main ' "$work/gdb"
echo 'PASS: FCC DWARF 5 types, lexical scopes, locations and backtrace'
