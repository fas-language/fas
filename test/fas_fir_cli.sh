#!/bin/sh
# The fas-fir command line (SPEC §13, stable from v0.8): every option, the
# output files and streams, and the exit status (0 success, 1 input refused,
# 2 usage error, 3 internal error). FAS_FIR_TOOL names the tool; by default
# the dune build. FAS_CC (else cc) is used for --llvm when it is found.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
tool=${FAS_FIR_TOOL:-$here/../_build/default/tools/fas_fir/fas_fir.exe}
case $tool in /*) ;; *) tool=$(pwd)/$tool ;; esac
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
cd "$tmp"
cp "$here/fas_fir/sum_loop.fir" "$here/fas_fir/putnum.fir" "$here/fas_fir/tls.fir" "$here/fas_fir/lines.fir" .
fail=0; n=0
bad() { echo "FAIL $*"; fail=1; }
# run WANT-STATUS ARGS...: runs the tool, stdout to out, stderr to err
run() {
  w=$1; shift; n=$((n + 1))
  set +e; "$tool" "$@" > out 2> err; rc=$?; set -e
  if [ $rc != "$w" ]; then bad "fas-fir $*: exit $rc, expected $w: $(head -n 3 err)"; return 1; fi
  case $w in
    1 | 2) if ! head -n 1 err | grep -q '^fas-fir: '; then bad "fas-fir $*: no 'fas-fir:' message: $(head -n 2 err)"; return 1; fi ;;
  esac
  return 0
}
has() { grep -q -- "$2" "$1" || bad "$3: '$2' not in $1: $(head -n 3 "$1")"; }

# help and version: stdout, exit 0
run 0 --help && { has out 'usage: fas-fir' --help; has out 'exit status' --help; [ -s err ] && bad "--help wrote to stderr"; }
run 0 -h && has out 'usage: fas-fir' -h
run 0 --version && { grep -Eq '^fas-fir [0-9]+\.[0-9]+\.[0-9]+ \(' out || bad "--version: $(cat out)"; }

# usage errors: exit 2
run 2 && has err 'no input file' "no arguments"
run 2 --frobnicate sum_loop.fir && has err 'unknown option --frobnicate' "unknown option"
run 2 sum_loop.fir -o && has err '-o needs a file name' "-o at the end"
run 2 sum_loop.fir -o a -o b && has err '-o given twice' "-o twice"
run 2 -c --check sum_loop.fir && has err 'do not go together' "-c --check"
run 2 --legalize --emit-llvm sum_loop.fir && has err 'do not go together' "--legalize --emit-llvm"
run 2 sum_loop.fir putnum.fir && has err 'one .fir input' "two inputs"
run 2 -fPIC sum_loop.fir && has err '-fPIC is for objects' "-fPIC without -c"
run 2 -c --no-pie sum_loop.fir && has err 'for executables' "-c --no-pie"
run 2 -c sum_loop.fir putnum.o && has err 'takes no object inputs' "-c with objects"
run 2 --llvm --check sum_loop.fir && has err 'does not go with --check' "--llvm --check"

# refused input: exit 1 with a located message
run 1 missing.fir && has err 'missing.fir' "missing file"
printf 'func @main() -> (i32) {\n^0:\n  ret i32\n}\n' > parse.fir
run 1 --check parse.fir && has err 'parse.fir:4:' "parse error line"
printf 'func @main() -> (i32) {\n^0:\n  ret i64 0\n}\n' > verify.fir
run 1 --check verify.fir && has err 'verifier' "verifier error"
printf 'func @f() -> (i32) {\n^0:\n  ret i32 0\n}\n' > nomain.fir
run 1 nomain.fir && has err 'no @main' "executable without @main"
run 0 -c nomain.fir -o nomain.o

# --check: canonical text on stdout or to -o, a fixed point; stdin as -
run 0 --check sum_loop.fir && cp out canon.fir
run 0 --check canon.fir && { cmp -s out canon.fir || bad "--check is not a fixed point"; }
run 0 --check sum_loop.fir -o checked.fir && { cmp -s checked.fir canon.fir || bad "--check -o differs from stdout"; [ -s out ] && bad "--check -o also wrote stdout"; }
n=$((n + 1)); "$tool" --check - < sum_loop.fir > stdin.fir 2> err || bad "--check - (stdin): $(cat err)"
cmp -s stdin.fir canon.fir || bad "--check - differs from the file"
# --legalize and --emit-llvm, stdout and -o
run 0 --legalize sum_loop.fir && has out 'func @main' --legalize
run 0 --legalize sum_loop.fir -o leg.fir && { cmp -s leg.fir out || true; [ -s leg.fir ] || bad "--legalize -o wrote nothing"; }
run 0 --emit-llvm sum_loop.fir && has out 'define' --emit-llvm
run 0 --emit-llvm sum_loop.fir -o s.ll && has s.ll 'define' "--emit-llvm -o"

# objects: -c (default name INPUT.o), -o, -fPIC, -g
elftype() { readelf -h "$1" | sed -n 's/^ *Type: *\([A-Z]*\).*/\1/p'; }
run 0 -c putnum.fir && { [ -f putnum.o ] || bad "-c did not write putnum.o"; [ "$(elftype putnum.o)" = REL ] || bad "putnum.o is not REL"; }
run 0 -c sum_loop.fir -o s.o && [ "$(elftype s.o)" = REL ] || bad "-c -o: not REL"
run 0 -c tls.fir -o tls_ie.o && run 0 -c -fPIC tls.fir -o tls_gd.o && {
  readelf -r tls_gd.o | grep -q TLSGD || bad "-fPIC: no general-dynamic TLS relocation"
  readelf -r tls_ie.o | grep -q TLSGD && bad "-c without -fPIC used general-dynamic TLS"
}
# -g needs a loc table: lines.fir has one, sum_loop.fir none
run 0 -c -g lines.fir -o lg.o && { readelf -S lg.o | grep -q debug_line || bad "-c -g: no .debug_line"; }
readelf -S s.o | grep -q debug_ && bad "-c without -g has debug sections"

# executables: default a.out (static PIE), -o, --no-pie, --pie, -g, objects, --mir
want=$(cat "$here/fas_fir/sum_loop.out")
check_exe() { [ "$(./"$1")" = "$want" ] || bad "$2: wrong output"; [ "$(elftype "$1")" = "$3" ] || bad "$2: ELF type $(elftype "$1"), expected $3"; }
run 0 sum_loop.fir putnum.o && check_exe a.out "default a.out" DYN
run 0 sum_loop.fir putnum.o -o pie && check_exe pie "-o" DYN
run 0 --no-pie sum_loop.fir putnum.o -o nopie && check_exe nopie "--no-pie" EXEC
run 0 --pie sum_loop.fir putnum.o -o pie2 && check_exe pie2 "--pie" DYN
run 0 --mir sum_loop.fir putnum.o -o mir && check_exe mir "--mir" DYN
run 0 -g lines.fir putnum.o -o dbg && { [ "$(./dbg)" = "$(cat "$here/fas_fir/lines.out")" ] || bad "-g: wrong output"; readelf -S dbg | grep -q debug_info || bad "-g: no .debug_info"; }
run 1 sum_loop.fir && has err 'undefined symbol putnum' "a missing object"

# --llvm, when a C compiler is found
cc=${FAS_CC:-cc}
if command -v "$cc" > /dev/null 2>&1; then
  export FAS_CC="$cc"
  run 0 --llvm sum_loop.fir putnum.o -o viallvm && [ "$(./viallvm)" = "$want" ] || bad "--llvm: wrong output"
  run 0 --llvm -c putnum.fir -o pl.o && [ "$(elftype pl.o)" = REL ] || bad "--llvm -c: not REL"
fi

[ $fail = 0 ] && echo "fas-fir cli: $n runs ok"
exit $fail
