#!/bin/sh
# Hand-written Fir programs through fas-fir (Fir text -> native executable).
# Each test/fas_fir/NAME.fir is linked with putnum.fir; NAME.out holds the
# expected stdout and an optional "; exit: N" line in the .fir the expected
# exit code (default 0). FAS_FIR_TOOL names the tool; by default the dune build.
# test/fas_fir/c/NAME.fir is compiled with -c and linked with NAME.c by a C
# compiler (FAS_CC, else cc), whose main exits 0 when the two agree; skipped
# when no C compiler is found. A c/NAME.fir with a "; shared: yes" line is
# built with -c -fPIC into a shared library instead, and NAME.c gets its
# path as argv[1] (to dlopen it). A "; libs: -lx ..." line adds libraries
# to the link. FAS_FIR_FLAGS are passed to every tool run.
# Each executable program is built twice: as a static PIE (the default, whose
# _start relocates itself) and with --no-pie (a fixed-address executable).
# FAS_FIR_FLAGS=--llvm runs them all through the LLVM path instead (Fir_llvm,
# then FAS_CC); a program with a "; llvm: skip REASON" line is skipped then.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
tool=${FAS_FIR_TOOL:-$here/../_build/default/tools/fas_fir/fas_fir.exe}
flags=${FAS_FIR_FLAGS:-}
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
"$tool" $flags -c "$here/fas_fir/putnum.fir" -o "$tmp/putnum.o"
fail=0; n=0; skipped=0
llvm_skip() {
  case " $flags " in *" --llvm "*) grep -q '^; llvm: skip' "$1" ;; *) return 1 ;; esac
}
for f in "$here"/fas_fir/*.fir; do
 for pie in "" --no-pie; do
  name=$(basename "$f" .fir)
  [ "$name" = putnum ] && continue
  if llvm_skip "$f"; then skipped=$((skipped + 1)); continue; fi
  n=$((n + 1))
  want=$(sed -n 's/^; exit: *\([0-9]*\).*/\1/p' "$f"); want=${want:-0}
  if ! "$tool" $flags $pie "$f" -o "$tmp/$name" "$tmp/putnum.o" 2>"$tmp/err"; then
    echo "FAIL $name${pie:+ ($pie)}: $(cat "$tmp/err")"; fail=1; continue
  fi
  set +e; "$tmp/$name" > "$tmp/$name.stdout"; rc=$?; set -e
  if [ "$rc" != "$want" ]; then echo "FAIL $name${pie:+ ($pie)}: exit $rc, expected $want"; fail=1
  elif ! cmp -s "$tmp/$name.stdout" "$here/fas_fir/$name.out"; then
    echo "FAIL $name${pie:+ ($pie)}: stdout differs"; diff "$here/fas_fir/$name.out" "$tmp/$name.stdout" | head; fail=1
  fi
 done
done
cc=${FAS_CC:-cc}
if command -v "$cc" >/dev/null 2>&1; then
  for f in "$here"/fas_fir/c/*.fir; do
    name=$(basename "$f" .fir)
    if llvm_skip "$f"; then skipped=$((skipped + 1)); continue; fi
    n=$((n + 1))
    if grep -q '^; shared: yes' "$f"; then
      # a shared library (-fPIC) that NAME.c loads with dlopen (argv[1])
      if ! "$tool" $flags -c -fPIC "$f" -o "$tmp/c_$name.o" 2>"$tmp/err" \
        || ! "$cc" -shared "$tmp/c_$name.o" -o "$tmp/lib_$name.so" 2>"$tmp/err" \
        || ! "$cc" -rdynamic "$here/fas_fir/c/$name.c" -o "$tmp/c_$name" -ldl -lpthread 2>"$tmp/err"; then
        echo "FAIL c/$name: $(cat "$tmp/err")"; fail=1; continue
      fi
      if ! "$tmp/c_$name" "$tmp/lib_$name.so" > "$tmp/c_$name.stdout"; then
        echo "FAIL c/$name: $(cat "$tmp/c_$name.stdout")"; fail=1
      fi
      continue
    fi
    n=$((n + 1))
    libs=$(sed -n 's/^; libs: *//p' "$f")
    if ! "$tool" $flags -c "$f" -o "$tmp/c_$name.o" 2>"$tmp/err" \
      || ! "$cc" "$here/fas_fir/c/$name.c" "$tmp/c_$name.o" -o "$tmp/c_$name" -lm $libs 2>"$tmp/err"; then
      echo "FAIL c/$name: $(cat "$tmp/err")"; fail=1; continue
    fi
    if ! "$tmp/c_$name" > "$tmp/c_$name.stdout"; then
      echo "FAIL c/$name: $(cat "$tmp/c_$name.stdout")"; fail=1
    fi
  done
else
  echo "fas-fir: no C compiler ($cc), C interop programs skipped"
fi
note=; [ $skipped = 0 ] || note=", $skipped skipped"
[ $fail = 0 ] && echo "fas-fir: $n programs ok$note"
exit $fail
