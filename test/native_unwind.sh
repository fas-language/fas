#!/bin/sh
# Native unwind info (.eh_frame, src/backend/mir/mir_cfi.ml), roadmap v0.8:
#   1. coverage: every function of every test object (test/*.fas,
#      test/fas_fir/*.fir) has an FDE covering exactly its symbol's range;
#   2. every instruction of a run unwinds: test/native_unwind.py
#      single-steps test/fas_fir/unwind.fir (every frame shape: dynamic
#      stack, two returns, runtime helpers, inline asm moving rsp) and a Fas
#      program from main to exit, and at each instruction gdb, with its
#      prologue analyzers disabled (CFI only), must unwind back to main.
#      The same walk over a copy without .eh_frame must fail, so the check
#      cannot pass on heuristics;
#   3. a trap (ud2) unwinds to its caller with CFI only.
set -eu
export DEBUGINFOD_URLS=

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
FAS_FIR_TOOL=${FAS_FIR_TOOL:-$ROOT/_build/default/tools/fas_fir/fas_fir.exe}
T=$(mktemp -d)
trap 'rm -rf "$T"' EXIT HUP INT TERM
fail=0
bad() { echo "native unwind: $*" >&2; fail=1; }

# fde_check FILE: functions (sized FUNC symbols in executable sections)
# without an FDE for exactly [value, value+size)
fde_check() {
  readelf -SW "$1" | sed -n 's/^ *\[ *\([0-9]*\)\] [^ ]* .* AX .*/\1/p' >"$T/xsec"
  readelf --debug-dump=frames "$1" 2>/dev/null \
    | sed -n 's/.* FDE .*pc=\([0-9a-f]*\)\.\.\([0-9a-f]*\).*/\1 \2/p' \
    | while read lo hi; do printf '%x %x\n' "0x$lo" "0x$hi"; done >"$T/fde"
  readelf -sW "$1" | awk '$4 == "FUNC" && $3 > 0 { print $7, $2, $3, $8 }' \
    | while read sec v sz name; do
        grep -qx "$sec" "$T/xsec" || continue
        r=$(printf '%x %x' "0x$v" $((0x$v + sz)))
        grep -qx "$r" "$T/fde" || echo "$name [$r)"
      done
}

# 1. coverage
objects=0
for f in "$ROOT"/test/*.fas; do
  b=$(basename "$f" .fas)
  # hand-written asm without .cfi directives: its author's job, as in C
  [ "$b" = pic_asm ] && continue
  "$OCAML_FAS" -c "$f" -o "$T/o.o" >/dev/null 2>&1 || continue
  objects=$((objects + 1))
  missing=$(fde_check "$T/o.o")
  [ -z "$missing" ] || bad "$b.fas: no FDE for $(echo $missing)"
done
for f in "$ROOT"/test/fas_fir/*.fir "$ROOT"/test/fir_fuzz/*.fir; do
  b=$(basename "$f" .fir)
  "$FAS_FIR_TOOL" -c "$f" -o "$T/o.o" >/dev/null 2>&1 || continue
  objects=$((objects + 1))
  missing=$(fde_check "$T/o.o")
  [ -z "$missing" ] || bad "$b.fir: no FDE for $(echo $missing)"
done
[ "$objects" -gt 150 ] || bad "only $objects objects checked"

"$FAS_FIR_TOOL" -c "$ROOT/test/fas_fir/putnum.fir" -o "$T/putnum.o"
cat >"$T/shapes.fas" <<'FAS'
fn leaf(x i64) i64 {
    return x * 3 + 1
}

fn pressure(a i64, b i64, c i64, d i64) i64 {
    x1 i64 = a * b
    x2 i64 = b * c
    x3 i64 = c * d
    x4 i64 = d * a
    x5 i64 = x1 ^ x3
    x6 i64 = x2 ^ x4
    r i64 = leaf(x5) + leaf(x6) + x1 + x2 + x3 + x4 + x5 + x6
    if r == 7 {
        return 0
    }
    return r
}

fn main(argc i32, argv addr) i32 {
    v i64 = pressure(sext[i64](argc), 2, 3, 4)
    return trunc[i32](v & 63)
}
FAS
for pie in "" --no-pie; do
  "$FAS_FIR_TOOL" -g $pie "$ROOT/test/fas_fir/unwind.fir" "$T/putnum.o" -o "$T/uw"
  out=$("$T/uw" | tr '\n' ' ')
  [ "$out" = "13 35 7 7 10 " ] || bad "unwind.fir${pie:+ $pie}: printed $out"
  missing=$(fde_check "$T/uw")
  [ -z "$missing" ] || bad "unwind.fir${pie:+ $pie} executable: no FDE for $(echo $missing)"
done
"$OCAML_FAS" -g "$T/shapes.fas" -o "$T/shapes"
missing=$(fde_check "$T/shapes")
[ -z "$missing" ] || bad "shapes.fas executable: no FDE for $(echo $missing)"

if ! command -v gdb >/dev/null 2>&1 || ! gdb -nx -q -batch -ex 'python print(1)' >/dev/null 2>&1; then
  echo "native unwind: gdb with Python not found, the step and trap checks are skipped" >&2
  [ $fail = 0 ] && echo "native unwind: $objects objects covered"
  exit $fail
fi

# 2. every instruction
walk() { (cd "$T" && gdb -nx -q -batch -x "$ROOT/test/native_unwind.py" "$@" 2>&1 || true); }
for pie in "" --no-pie; do
  "$FAS_FIR_TOOL" -g $pie "$ROOT/test/fas_fir/unwind.fir" "$T/putnum.o" -o "$T/uw"
  out=$(walk ./uw)
  echo "$out" | grep -q '^steps [0-9]* bad 0$' \
    || bad "unwind.fir${pie:+ $pie}: $(echo "$out" | grep -E '^(BAD|steps)' | head -5)"
  seen=$(echo "$out" | sed -n 's/^seen //p')
  for fn in leaf dyn wide fused pushy boom putnum __fir_udivmod __fas_rt_fma_core; do
    case " $seen " in *" $fn "*) ;; *) bad "unwind.fir${pie:+ $pie}: the walk never reached $fn" ;; esac
  done
done
out=$(walk ./shapes)
echo "$out" | grep -q '^steps [0-9]* bad 0$' || bad "shapes.fas: $(echo "$out" | grep -E '^(BAD|steps)' | head -5)"
objcopy --remove-section .eh_frame --remove-section .eh_frame_hdr "$T/uw" "$T/uw.nocfi"
out=$(walk ./uw.nocfi)
echo "$out" | grep -q '^steps [0-9]* bad [1-9]' || bad "the walk passes without .eh_frame: $(echo "$out" | tail -2)"

# 3. a trap
out=$(cd "$T" && gdb -nx -q -batch -ex run -ex 'maint frame-unwinder disable -class ARCH' -ex bt \
  --args ./uw trap 2>&1 || true)
echo "$out" | grep -q 'SIGILL' || bad "unwind.fir trap: no SIGILL: $out"
echo "$out" | grep -Eq '^#0 .* boom ' && echo "$out" | grep -Eq '^#1 .* main ' \
  || bad "unwind.fir trap: backtrace is not boom <- main: $(echo "$out" | grep '^#')"

[ $fail = 0 ] && echo "native unwind: $objects objects covered; every instruction of 3 runs and a trap unwind to main"
exit $fail
