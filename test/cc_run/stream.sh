#!/usr/bin/env bash
set -euo pipefail
ulimit -f 524288

work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
cd "$work"

cat > main.c <<'C'
int answer(void);
int main(void) { return answer() != 42; }
C
echo 'int answer(void) { return 42; }' > answer.c

# Verify the actual transport as well as object and executable bytes.
cat > check-fir <<'SH'
#!/bin/sh
for arg do
    if [ "$arg" = - ]; then
        exec "$REAL_FIR" "$@"
    fi
done
echo 'test: expected Fir from stdin' >&2
exit 1
SH
chmod +x check-fir
REAL_FIR="$FCC_FIR" FCC_FIR="$work/check-fir" "$FCC" -g -c main.c -o main.o
cp main.o streamed.o
"$FCC" -g -save-temps -c main.c -o main.o
cmp streamed.o main.o
test -s main.o.fir
"$FCC" -g -c main.o.fir -o reassembled.o
cmp streamed.o reassembled.o

"$FCC" -g main.c answer.c -o executable
cp executable streamed
"$FCC" -g -save-temps main.c answer.c -o executable
cmp streamed executable
test -s executable.0.fir
test -s executable.1.fir
./executable

ln answer.c protected.o.fir
cp answer.c original.c
status=0
"$FCC" -save-temps -c answer.c -o protected.o 2> clobber.err || status=$?
test "$status" = 1
grep -qF 'fcc: saved Fir would overwrite' clobber.err
cmp answer.c original.c
test ! -e protected.o

# C can also come from the driver's stdin while the child reads the pipe.
"$FCC" -x c -c -o stdin.o - < answer.c
test -s stdin.o
( exec 0<&-; "$FCC" -c answer.c -o closed-stdin.o )
test -s closed-stdin.o
printf '#warning test diagnostic with stderr closed\nint warned(void) { return 0; }\n' > warn.c
( exec 0<&- 2>&-; "$FCC" -c warn.c -o warned.o )
test -s warned.o

# Losing the backend should produce a regular driver error, even beyond
# pipe capacity. A late C diagnostic must leave the output untouched.
printf '#!/bin/sh\nexit 42\n' > failing-fir
chmod +x failing-fir
for i in {1..400}; do
    printf 'int f%d(int n) { return n + %d; }\n' "$i" "$i"
done > many.c
status=0
FCC_FIR="$work/failing-fir" timeout 20 "$FCC" -c many.c -o missing.o 2> backend.err || status=$?
test "$status" = 1
grep -qF 'fcc: fas-fir failed' backend.err
test ! -e missing.o

# A subprocess that reports success without consuming its input is still
# an error when the front end cannot flush the Fir stream.
printf '#!/bin/sh\nexit 0\n' > premature-fir
chmod +x premature-fir
status=0
FCC_FIR="$work/premature-fir" timeout 20 "$FCC" -c many.c -o missing.o 2> pipe.err || status=$?
test "$status" = 1
grep -qF 'fcc: could not write Fir output' pipe.err
test ! -e missing.o

printf '\n#error frontend failed late\n' >> many.c
status=0
timeout 20 "$FCC" -c many.c -o main.o 2> frontend.err || status=$?
test "$status" = 1
grep -qF 'frontend failed late' frontend.err
cmp main.o streamed.o

# Check compile-and-link cleanup on both frontend and linker errors. The
# linker receives live object descriptors without creating directory entries.
mkdir scratch
export TMPDIR="$work/scratch"
status=0
"$FCC" answer.c many.c -o missing 2> frontend-link.err || status=$?
test "$status" = 1
grep -qF 'frontend failed late' frontend-link.err
test ! -e missing
test -z "$(ls -A scratch)"

cat > failing-ld <<'SH'
#!/bin/sh
set -eu
for arg do
    case "$arg" in
        /proc/self/fd/*)
            test -s "$arg"
            test -z "$(ls -A "$TMPDIR")"
            echo 'test linker: received anonymous object' >&2
            exit 42
            ;;
    esac
done
exit 99
SH
chmod +x failing-ld
status=0
FCC_LD="$work/failing-ld" "$FCC" answer.c -o missing 2> linker.err || status=$?
test "$status" = 1
grep -qF 'test linker: received anonymous object' linker.err
test ! -e missing
test -z "$(ls -A scratch)"

echo 'PASS: streamed Fir, saved temps, stdin and failure handling'
