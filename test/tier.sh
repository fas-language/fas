#!/usr/bin/env bash
# Quick checks the compiler and the differential manifest. Push also runs
# the shorter project builds. Milestone adds the heavyweight projects,
# the native compiler checks, kernel boot, and fuzzers.
# Run one tier at a time. Build products are private to each invocation.
set -uo pipefail
ulimit -f 524288
ulimit -c 0
cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." || exit 2
root=$PWD

tier=${1:-}
if [[ $# != 1 || ! $tier =~ ^(quick|push|milestone)$ ]]; then
    echo "usage: $0 quick|push|milestone" >&2
    exit 2
fi
jobs=${TIER_JOBS:-3}
case "$jobs" in
    [1-6]) ;;
    0*|*[!0-9]*) echo 'TIER_JOBS must be a positive integer' >&2; exit 2 ;;
    *) jobs=6 ;;
esac
# Do not let a parent's jobserver, compiler wrappers or oracle setting change
# the tier. Every worker launches at most one linker at a time.
unset FAS_ORACLE FAS_FIR MAKEFLAGS MFLAGS GNUMAKEFLAGS
export FAS_CC=clang-22 FAS_OPT=opt-22 FAS_LLC=llc-22
export CC=clang-22 LLVM_OPT=opt-22 LLVM_LLC=llc-22
export FAS_LD_THREADS=1

state=$root/_build/tiers
mkdir -p -- "$state" || exit 2
exec 9>"$state/lock"
if ! flock -n 9; then
    echo 'Another tier run is active in this worktree' >&2
    exit 2
fi
run=$(mktemp -d -- "$state/$tier.XXXXXXXX") || exit 2
export TMPDIR=$run/tmp
mkdir -p -- "$TMPDIR" || exit 2
log=$run/tier.log
started=$SECONDS
active=
status=1
finish() {
    local code=$?
    if (( code != 0 )); then status=$code; fi
    rm -rf -- "$TMPDIR" "$run/bin"
    local result=PASS
    if (( status != 0 )); then result=FAIL; fi
    printf '%s %s (%ss total) %s\n' "$result" "$tier" "$((SECONDS - started))" "$log" | tee -a "$log"
    exit "$status"
}
interrupt() {
    local code=$1
    if [[ -n $active ]]; then
        # Every part has its own process group. The project runner also
        # shuts down its private build groups before removing their trees.
        kill -TERM -- "-$active" 2>/dev/null || kill -TERM "$active" 2>/dev/null || :
        wait "$active" || :
    fi
    exit "$code"
}
trap finish EXIT
trap 'interrupt 130' INT
trap 'interrupt 143' TERM
trap 'interrupt 129' HUP

printf 'Tier %s with %s jobs. Full log %s\n' "$tier" "$jobs" "$log" | tee "$log" || exit 1
part() {
    local name=$1 start=$SECONDS rc=0 result=PASS
    shift
    printf '\n=== %s ===\n' "$name" >> "$log"
    printf ' %q' "$@" >> "$log"
    printf '\n' >> "$log"
    setsid --wait "$@" >> "$log" 2>&1 9>&- &
    active=$!
    wait "$active" || rc=$?
    active=
    if (( rc != 0 )); then result=FAIL; status=1; fi
    printf '%s %s (%ss, exit %s)\n' "$result" "$name" "$((SECONDS - start))" "$rc" | tee -a "$log"
    return "$rc"
}

export OCAML_FAS=$root/_build/default/bin/main.exe
export FCC_FIR=$root/_build/default/tools/fas_fir/fas_fir.exe
export FCC_INCLUDE=$root/cc/include

build_fcc() {
    local backend=$1
    mkdir -p -- "$run/bin/$backend" || return
    export FCC=$run/bin/$backend/fcc FCC_LD=$run/bin/$backend/fas-ld
    if [[ $backend == llvm ]]; then
        part 'LLVM -O2 fcc build' env FAS_ORACLE=llvm timeout 600 "$OCAML_FAS" --backend=llvm -O2 cc/main.fas -o "$FCC" || return
    else
        part 'Native fcc build' timeout 600 "$OCAML_FAS" cc/main.fas -o "$FCC" || return
    fi
    part "$backend tier fas-ld build" timeout 600 "$OCAML_FAS" ld/main.fas -o "$FCC_LD"
}

cc_checks() {
    local backend=$1
    part "$backend manifest" bash test/cc_run.sh --manifest --strict --show 10000 --jobs "$jobs" || :
    part "$backend regressions" bash test/cc_run.sh --regressions || :
    part "$backend flags" bash test/cc_run.sh --flags || :
    part "$backend stream" bash test/cc_run.sh --stream || :
    part "$backend random C" bash test/cc_run.sh --random --count 100 --jobs "$jobs" || :
    part "$backend cc_types" bash test/cc_types.sh || :
}

project_checks() {
    local selection=()
    if [[ $tier == push ]]; then
        selection=(--exclude postgresql --exclude cpython --exclude openssl --exclude perl --exclude git)
    fi
    part "$1 projects" bash test/cc_run.sh --projects --jobs "$jobs" --log-dir "$run/$1-projects" "${selection[@]}" || :
}

status=0
part 'Dune build' dune build -j "$jobs" || exit 1
part 'Dune test' dune test -j "$jobs" || :
build_fcc llvm || exit 1
cc_checks llvm
if [[ $tier != quick ]]; then
    project_checks llvm
fi
if [[ $tier == milestone ]]; then
    build_fcc native || exit 1
    cc_checks native
    project_checks native
    part 'Native Microsoft ABI' bash test/cc_run.sh --abi || :
    part 'Native preprocessor' bash test/cc_pp.sh --jobs "$jobs" || :
    part 'Kernel boot' env KERNEL_JOBS="$jobs" bash test/cc_kernel.sh || :
    part 'Csmith' bash test/cc_run.sh --csmith --jobs "$jobs" --failures "$run/csmith-failures" || :
    part 'Fir fuzzer' env FIR_FUZZ_KEEP="$run/fir-failures" bash test/fir_fuzz.sh || :
    part 'ABI fuzzer' env ABI_FUZZ_KEEP="$run/abi-failures" bash test/abi_fuzz.sh || :
    part 'Linker API, shadow and fuzzer' env FAS_LD_TEST_MAX_THREADS="$jobs" bash test/fas_ld.sh || :
fi
exit "$status"
