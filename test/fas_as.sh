#!/usr/bin/env bash
set -euo pipefail

# End-to-end verification suite for fas-as (x86-64 AT&T assembler in Fas).
#
# Env overrides:
#   FAS_AS_CASES   number of differential fuzz cases (default: 1000)
#   FAS_AS_SEED    fuzzer RNG seed (default: 1)
#   FAS_AS_CORPUS  corpus mode: quick | libs | full (default: libs)
#   FAS_AS_JOBS    parallel corpus jobs (default: available CPUs)

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
OCAML_FAS="$ROOT/_build/default/bin/main.exe"
UPSTREAM="${FAS_AS_UPSTREAM:-$ROOT/../cc-tests/upstream}"
FAS_AS_CASES="${FAS_AS_CASES:-1000}"
FAS_AS_SEED="${FAS_AS_SEED:-1}"
FAS_AS_CORPUS="${FAS_AS_CORPUS:-libs}"

if [ ! -x "$OCAML_FAS" ]; then
    (cd "$ROOT" && dune build bin/main.exe)
fi

WORK=$(mktemp -d "${TMPDIR:-/tmp}/fas-as-test.XXXXXX")
trap 'rm -rf "$WORK"' EXIT

reference_cc() {
    python3 "$ROOT/tools/refcache/compiler.py" fas-as/fixture "$WORK" clang-22 "$@"
}

# Build fas-as, as/test/apitest, and fas-ld
"$OCAML_FAS" -o "$WORK/fas-as" "$ROOT/as/main.fas"
"$OCAML_FAS" -o "$WORK/as-apitest" "$ROOT/as/test/apitest.fas"
"$OCAML_FAS" -o "$WORK/fas-ld" "$ROOT/ld/main.fas"

# 1. Library API + template-assembly + diagnostics unit test
"$WORK/as-apitest"

# 2. CLI diagnostics checks (file:line: error: ..., exit 1)
check_diag() {
    local label="$1" src="$2"
    shift 2
    printf '%b\n' "$src" >"$WORK/bad.s"
    if "$WORK/fas-as" "$@" "$WORK/bad.s" -o "$WORK/bad.o" 2>"$WORK/bad.err"; then
        echo "FAIL diag ($label): expected exit 1" >&2
        exit 1
    fi
    if ! grep -q ': error: ' "$WORK/bad.err"; then
        echo "FAIL diag ($label): missing 'file:line: error:' in: $(cat "$WORK/bad.err")" >&2
        exit 1
    fi
}
check_diag "unknown-mnemonic" ".text\n  not_an_instruction %rax"
check_diag "bad-operand" ".text\n  movq %al, %rax"
check_diag "undef-local-label" ".text\n  jmp 1f"
check_diag "intel-syntax" ".intel_syntax noprefix\n  nop"
check_diag "rex-hi8" ".text\n  movb %ah, %r8b"
check_diag "backwards-org" ".text\n  .long 1\n  .org 2"
check_diag "code32-rex" ".code32\n  movb %dil, (%eax)"
check_diag "fatal-warning" '.warning "this warning is fatal"' --fatal-warnings
echo "diag: ok (8/8 checks)"

for fixture in "$ROOT"/test/fas_as/fixtures/*.asm; do
    llvm-mc-22 -triple=x86_64-unknown-linux-gnu -filetype=obj "$fixture" -o "$WORK/ref.o"
    "$WORK/fas-as" "$fixture" -o "$WORK/fas.o"
    python3 "$ROOT/test/fas_as/corpus_check.py" --compare "$WORK/ref.o" "$WORK/fas.o"
    echo "fixture: ${fixture##*/} ok"
done

# LLVM exposes this option through the Clang assembler driver.
printf '%s\n' '.text' 'movq external@GOTPCREL(%rip), %rax' 'movl external@GOTPCREL(%rip), %eax' > "$WORK/got.s"
reference_cc -c -Wa,-mrelax-relocations=no "$WORK/got.s" -o "$WORK/ref.o"
"$WORK/fas-as" -mrelax-relocations=no "$WORK/got.s" -o "$WORK/fas.o"
python3 "$ROOT/test/fas_as/corpus_check.py" --compare "$WORK/ref.o" "$WORK/fas.o"
echo "fixture: unrelaxed GOT relocations ok"

# 3. Differential fuzzer against llvm-mc-22
python3 "$ROOT/test/fas_as/fuzz_as.py" "$WORK/fas-as" "$FAS_AS_SEED" "$FAS_AS_CASES"

# 4. Corpus differential check against llvm-mc-22
if [ -d "$UPSTREAM" ]; then
    python3 "$ROOT/test/fas_as/corpus_check.py" "$WORK/fas-as" "$UPSTREAM" "$FAS_AS_CORPUS"

    # Also verify -g and -fno-asynchronous-unwind-tables on a sample of c-testsuite
    g_pass=0
    for f in "$UPSTREAM"/c-testsuite/tests/single-exec/0000[1-9].c "$UPSTREAM"/c-testsuite/tests/single-exec/0001[0-5].c; do
        [ -f "$f" ] || continue
        for fl in "-O2 -g" "-O2 -fno-asynchronous-unwind-tables"; do
            reference_cc -S $fl "$f" -o "$WORK/g.s"
            "$WORK/fas-as" -g "$WORK/g.s" -o "$WORK/g.o"
            g_pass=$((g_pass + 1))
        done
    done
    echo "flags (-g / -fno-asynchronous-unwind-tables): $g_pass/$g_pass ok"

    # 5. End-to-end: assemble with fas-as, link with fas-ld, run & compare with clang-22
    run_pass=0
    # Find glibc CRT files for hosted linking with fas-ld
    CRT1=$(clang-22 -print-file-name=Scrt1.o)
    CRTI=$(clang-22 -print-file-name=crti.o)
    CRTN=$(clang-22 -print-file-name=crtn.o)
    LIBC_DIR=$(dirname "$CRT1")
    for n in 00001 00003 00005 00010 00015 00020 00025 00030 00040 00050; do
        cf="$UPSTREAM/c-testsuite/tests/single-exec/${n}.c"
        [ -f "$cf" ] || continue
        reference_cc -O2 -fPIE "$cf" -o "$WORK/ref_${n}"
        reference_cc -S -O2 -fPIE "$cf" -o "$WORK/f_${n}.s"
        "$WORK/fas-as" "$WORK/f_${n}.s" -o "$WORK/f_${n}.o"
        "$WORK/fas-ld" -pie --dynamic-linker /lib64/ld-linux-x86-64.so.2 \
            -o "$WORK/fas_${n}" "$CRT1" "$CRTI" "$WORK/f_${n}.o" -L"$LIBC_DIR" -lc "$CRTN"
        set +e
        "$WORK/ref_${n}" >"$WORK/ref.out" 2>&1; ref_rc=$?
        "$WORK/fas_${n}" >"$WORK/fas.out" 2>&1; fas_rc=$?
        set -e
        if [ "$ref_rc" != "$fas_rc" ] || ! cmp -s "$WORK/ref.out" "$WORK/fas.out"; then
            echo "FAIL run $n: ref_rc=$ref_rc fas_rc=$fas_rc" >&2
            exit 1
        fi
        run_pass=$((run_pass + 1))
    done
    echo "link+run (fas-as + fas-ld vs clang-22): $run_pass/$run_pass ok"
fi

echo "fas-as: all checks ok"
