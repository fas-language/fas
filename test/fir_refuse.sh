#!/bin/sh
# fas-fir refuses bad input with a message and never crashes (roadmap v0.8).
# 1. Known cases, each once a crash found by test/fir_refuse.py: a valid
#    program with one edit, and the message fas-fir must give for it.
# 2. test/fir_refuse.py mutates every .fir file under test/ FIR_REFUSE_COUNT
#    times in all (default 3000) with seed FIR_REFUSE_SEED (default 1), and
#    fails on any uncaught exception, internal error, signal or timeout.
# FAS_FIR_TOOL names the tool; by default the dune build.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
tool=${FAS_FIR_TOOL:-$here/../_build/default/tools/fas_fir/fas_fir.exe}
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
fail=0; n=0
# case FILE SED-EDIT FLAGS EXPECTED-MESSAGE
case_() {
  n=$((n + 1))
  sed "$2" "$here/fas_fir/$1" > "$tmp/m.fir"
  if cmp -s "$here/fas_fir/$1" "$tmp/m.fir"; then echo "FAIL $1: the edit $2 changed nothing"; fail=1; return; fi
  set +e; (cd "$tmp" && timeout 30 "$tool" $3 m.fir -o out) > /dev/null 2> "$tmp/err"; rc=$?; set -e
  if [ $rc != 1 ] || ! grep -qF -- "$4" "$tmp/err" || grep -q 'internal error' "$tmp/err"; then
    echo "FAIL $1 ($2): exit $rc, expected 1 with '$4':"; head -n 3 "$tmp/err"; fail=1
  fi
}
case_ asm_link.fir 's/in("1" i64 %x)/in("99999999999999999999999" i64 %x)/' -c \
  'asm matching constraint "99999999999999999999999" does not name an output'
case_ c/vastruct.fir 's/agg(16, 8, \[0: f64, 8: f64\]) ptr @g_dd2/agg(4294967296, 8, [0: f64, 8: f64]) ptr @g_dd2/' -c \
  'a struct of 4294967296 bytes passed or returned by value'
case_ c/vastruct.fir 's/va_arg agg(4, 4, \[0: i32\])/va_arg agg(4, 4294967296, [0: i32])/' -c \
  'a struct aligned to 4294967296'
case_ c/vastruct.fir 's/va_arg agg(4, 4, \[0: i32\])/va_arg agg(4, 3, [0: i32])/' -c \
  'agg alignment 3 is not a power of two'
case_ c/vastruct.fir 's/va_arg agg(4, 4, \[0: i32\])/va_arg agg(4, 4, [2: i32])/' -c \
  'agg field i32 at offset 2 is outside its 4 bytes'
case_ vectors.fir 's/store <4 x i32> \[i32 1, i32 2, i32 3, i32 4\]/store <4294967296 x i32> [i32 1, i32 2, i32 3, i32 4]/' -c \
  'vector constant <4294967296 x i32>: 4 lanes given'
case_ vectors.fir 's/store <4 x i32> \[i32 1, i32 2, i32 3, i32 4\]/store <0 x i32> [i32 1, i32 2, i32 3, i32 4]/' -c \
  'a vector of 0 lanes'
case_ asm_link.fir 's/global @counter size 8 align 8/global @counter size 4294967296 align 8/' -c \
  '@counter: a global of 4294967296 bytes'
case_ c/symbols.fir 's/global @wkeep size 8 align 8/global @wkeep size 8 align 4294967296/' -c \
  '@wkeep: aligned to 4294967296'
case_ asm_goto.fir 's/i64/i24/g' -c \
  'asm_goto on a wide or odd-width integer'
case_ vars.fir 's/loc 1 "vars.c"/loc 1 "va\\00rs.c"/' -c \
  'loc 1: file name "va\000rs.c" has a NUL byte'
case_ vars.fir 's/dtype 1 base "long"/dtype 1 base "lo\\00ng"/' '-c -g' \
  'dtype 1: name "lo\000ng" has a NUL byte'
case_ vars.fir 's/var "sum"/var "s\\00um"/' '-c -g' \
  '@work: var "s\000um" has a NUL byte'
for c in $(sed -n 's/^global @\([a-z_]*\) size 8 align 8 .*= zero$/\1/p' "$here/fas_fir/sum_loop.fir" | head -n 1); do
  # a 1 GiB zero global is fine (.bss), and fast: its bytes are never built
  sed "s/^global @$c size 8 /global @$c size 1073741824 /" "$here/fas_fir/sum_loop.fir" > "$tmp/big.fir"
  n=$((n + 1))
  if ! (cd "$tmp" && timeout 10 "$tool" -c big.fir -o big.o 2> "$tmp/err"); then echo "FAIL 1 GiB zero global: $(cat "$tmp/err")"; fail=1; fi
done
# 2. the fuzzer
if command -v python3 > /dev/null 2>&1; then
  out=$(cd "$tmp" && python3 "$here/fir_refuse.py" "$tool" "${FIR_REFUSE_COUNT:-3000}" "${FIR_REFUSE_SEED:-1}" \
          $(find "$here" -name '*.fir' | sort)) || { echo "$out"; fail=1; out=; }
else
  out="(no python3: fuzzer skipped)"
fi
[ $fail = 0 ] && echo "fir refuse: $n known cases; $out"
exit $fail
