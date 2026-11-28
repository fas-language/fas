#!/usr/bin/env bash
# Front-end speed and memory report (ARCHITECTURE.md §1 and §2).
#
# Builds fcc with LLVM -O2 from FCC_MAIN (default cc/main.fas), then times
# it against gcc and clang on the reference amalgamations. Every run is
# pinned to one core. The report shows the median wall time of RUNS runs
# and the peak RSS. It is a report, not a pass/fail gate.
#
#   bash test/cc_speed.sh                    # -fsyntax-only, current fcc
#   FCC_MAIN=cc2/main.fas bash test/cc_speed.sh
#   SPEED_BASELINE=1 FCC_MAIN=cc2/main.fas bash test/cc_speed.sh
#                                            # also time the old fcc (cc/main.fas)
#   SPEED_MODE=-E bash test/cc_speed.sh      # preprocess only
#   SPEED_PERF=1 bash test/cc_speed.sh       # add perf's top symbols per input
#   FCC=/path/to/fcc bash test/cc_speed.sh   # use a prebuilt fcc, skip the build
#
# Other knobs: RUNS (default 3), SPEED_CPU (default 2), SPEED_INPUTS
# (space-separated C files; default sqlite3.c and duktape.c).
set -euo pipefail
ulimit -f 524288
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd -- "$root"
tests=$root/../cc-tests/upstream

fcc_main=${FCC_MAIN:-cc/main.fas}
mode=${SPEED_MODE:--fsyntax-only}
runs=${RUNS:-3}
cpu=${SPEED_CPU:-2}
stamp=$(date +%Y%m%d-%H%M%S)
out=$root/_build/speed/$stamp
mkdir -p -- "$out"

export FAS_CC=${FAS_CC:-clang-22} FAS_OPT=${FAS_OPT:-opt-22} FAS_LLC=${FAS_LLC:-llc-22}
export FCC_INCLUDE=${FCC_INCLUDE:-$root/cc/include}
fas=$root/_build/default/bin/main.exe

if [[ -n ${SPEED_INPUTS:-} ]]; then
    read -r -a inputs <<< "$SPEED_INPUTS"
else
    inputs=("$tests/sqlite/sqlite3.c" "$tests/duktape/src-noline/duktape.c")
fi
for f in "${inputs[@]}"; do
    [[ -f $f ]] || { echo "missing input $f (run cc-tests/fetch.sh)" >&2; exit 2; }
done

build_llvm() { # main.fas output
    echo "building $2 from $1 with LLVM -O2" >&2
    FAS_ORACLE=llvm timeout 900 "$fas" --backend=llvm -O2 "$1" -o "$2"
}

dune build -j 3 2>&1 | tail -20
declare -A tool
if [[ -n ${FCC:-} ]]; then
    tool[fcc]=$(realpath -- "$FCC")
else
    build_llvm "$fcc_main" "$out/fcc"
    tool[fcc]=$out/fcc
fi
order=(fcc)
if [[ -n ${SPEED_BASELINE:-} ]]; then
    build_llvm cc/main.fas "$out/fcc-base"
    tool[fcc-base]=$out/fcc-base
    order+=(fcc-base)
fi
tool[gcc]=gcc
tool[clang]=clang-22
order+=(gcc clang)

# One timed run. Prints "seconds peak_kib". A nonzero exit is reported, not hidden.
one_run() { # tool input
    local t=$1 f=$2 rc=0
    /usr/bin/time -f '%e %M' -o "$out/time.txt" \
        taskset -c "$cpu" "${tool[$t]}" -std=gnu23 -O0 "$mode" "$f" \
        > /dev/null 2> "$out/stderr.txt" || rc=$?
    if (( rc != 0 )); then
        echo "  $t failed on $(basename "$f") (exit $rc); last stderr lines:" >&2
        tail -5 "$out/stderr.txt" >&2
        echo "FAIL FAIL"
        return
    fi
    tail -1 "$out/time.txt"
}

median() { sort -g | awk '{a[NR]=$1} END {print a[int((NR+1)/2)]}'; }

json=$out/report.json
printf '{"stamp":"%s","mode":"%s","fcc_main":"%s","runs":%s,"results":[' \
    "$stamp" "$mode" "$fcc_main" "$runs" > "$json"
first=1
printf '\n%-14s %-10s %10s %12s %10s\n' input tool seconds peak_MB vs_best_cc
for f in "${inputs[@]}"; do
    name=$(basename "$f")
    declare -A sec=() mem=()
    for t in "${order[@]}"; do
        times=() mems=()
        for ((i = 0; i < runs; i++)); do
            read -r s m < <(one_run "$t" "$f")
            times+=("$s"); mems+=("$m")
        done
        if [[ ${times[0]} == FAIL ]]; then sec[$t]=FAIL; mem[$t]=FAIL; continue; fi
        sec[$t]=$(printf '%s\n' "${times[@]}" | median)
        mem[$t]=$(printf '%s\n' "${mems[@]}" | sort -g | tail -1)
    done
    best=$(printf '%s\n' "${sec[gcc]}" "${sec[clang]}" | grep -v FAIL | sort -g | head -1)
    for t in "${order[@]}"; do
        if [[ ${sec[$t]} == FAIL ]]; then
            printf '%-14s %-10s %10s %12s %10s\n' "$name" "$t" FAIL - -
            ratio=null
        else
            ratio=$(awk -v a="${sec[$t]}" -v b="$best" 'BEGIN { if (a > 0) printf "%.2f", b / a; else print "inf" }')
            printf '%-14s %-10s %10s %12.1f %9sx\n' "$name" "$t" "${sec[$t]}" \
                "$(awk -v k="${mem[$t]}" 'BEGIN { print k / 1024 }')" "$ratio"
        fi
        (( first )) || printf ',' >> "$json"
        first=0
        printf '{"input":"%s","tool":"%s","seconds":"%s","peak_kib":"%s","speedup_vs_best_cc":"%s"}' \
            "$name" "$t" "${sec[$t]}" "${mem[$t]}" "$ratio" >> "$json"
    done
    unset sec mem
    if [[ -n ${SPEED_PERF:-} && -x ${tool[fcc]} ]]; then
        perf record -q -o "$out/perf-$name.data" -- \
            taskset -c "$cpu" "${tool[fcc]}" -std=gnu23 -O0 "$mode" "$f" > /dev/null 2>&1 || :
        perf report -q -i "$out/perf-$name.data" --stdio --no-children --percent-limit 1 \
            2> /dev/null | grep -v '^$' | head -25 > "$out/perf-$name.txt" || :
        echo "  perf top symbols for fcc on $name: $out/perf-$name.txt"
    fi
done
printf ']}\n' >> "$json"
echo
echo "vs_best_cc = (faster of gcc and clang) / tool; the front-end budget is >= 5x."
echo "Report: $json"
