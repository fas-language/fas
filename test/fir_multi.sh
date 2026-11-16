#!/bin/sh
# Several results (SPEC §5, Fir_abi.multi_results) across the two backends:
# test/fir_multi/lib.fir (the callees) and main.fir (the caller) are each
# built natively and through the LLVM path (--llvm), and every pairing is
# linked and run against main.out, so the two paths must agree on which
# results go in rax/rdx and xmm0/xmm1 and where the rest sit in the block.
# FAS_FIR_TOOL names the tool; by default the dune build.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
tool=${FAS_FIR_TOOL:-$here/../_build/default/tools/fas_fir/fas_fir.exe}
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
"$tool" -c "$here/fas_fir/putnum.fir" -o "$tmp/putnum.o"
fail=0
for b in native llvm; do
  fl=; [ $b = llvm ] && fl=--llvm
  "$tool" $fl -c "$here/fir_multi/lib.fir" -o "$tmp/lib_$b.o"
done
for m in native llvm; do
  fl=; [ $m = llvm ] && fl=--llvm
  for l in native llvm; do
    x="$tmp/main_${m}_$l"
    if ! "$tool" $fl "$here/fir_multi/main.fir" "$tmp/lib_$l.o" "$tmp/putnum.o" -o "$x" 2>"$tmp/err"; then
      echo "FAIL caller $m, callee $l: $(cat "$tmp/err")"; fail=1; continue
    fi
    if ! "$x" > "$x.stdout"; then echo "FAIL caller $m, callee $l: exit $?"; fail=1
    elif ! cmp -s "$x.stdout" "$here/fir_multi/main.out"; then
      echo "FAIL caller $m, callee $l: stdout differs"; diff "$here/fir_multi/main.out" "$x.stdout" | head; fail=1
    fi
  done
done
[ $fail = 0 ] && echo "fir-multi: 4 pairings ok"
exit $fail
