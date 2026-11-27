#!/usr/bin/env bash
set -euo pipefail

clang=${CLANG:-clang-22}
original=("$@")
cpp_args=()
ref_args=()
as_args=(--64)
source=
out=
mode=-m64
language=none
preprocess_only=false
deps=false
dep_file=false
dep_target=false
while (( $# )); do
    case "$1" in
        -m16|-m32|-mx32|-m64) mode=$1; ref_args+=("$1") ;;
        --target=x86_64-linux-gnu|-fintegrated-as) ref_args+=("$1") ;;
        -o) out=$2; shift ;;
        -x) language=$2; ref_args+=("$1" "$2"); shift ;;
        -E) preprocess_only=true ;;
        -c) ;;
        # Linker scripts can omit C comments.
        -C) ;;
        -I) cpp_args+=("$1" "$2"); as_args+=("$1" "$2"); ref_args+=("$1" "$2"); shift ;;
        -I*) cpp_args+=("$1"); as_args+=("$1"); ref_args+=("$1") ;;
        -include|-imacros|-iquote|-isystem|-idirafter|-D|-U)
            cpp_args+=("$1" "$2"); ref_args+=("$1" "$2"); shift ;;
        -Wa,-mrelax-relocations=yes|-Wa,-mrelax-relocations=no)
            as_args+=("${1#-Wa,}"); ref_args+=("$1") ;;
        -Wp,-MD,*|-Wp,-MMD,*) deps=true; dep_file=true; cpp_args+=("$1") ;;
        -MD|-MMD) deps=true; cpp_args+=("$1") ;;
        -MP) cpp_args+=("$1") ;;
        -MF) dep_file=true; cpp_args+=("$1" "$2"); shift ;;
        -MT) dep_target=true; cpp_args+=("$1" "$2"); shift ;;
        -MF*) dep_file=true; cpp_args+=("$1") ;;
        -MT*) dep_target=true; cpp_args+=("$1") ;;
        -|*.s|*.S) source=$1; ref_args+=("$1") ;;
        -*) cpp_args+=("$1"); ref_args+=("$1") ;;
        *) source=$1; ref_args+=("$1") ;;
    esac
    shift
done

if [[ $mode != -m64 ]]; then
    if ! $preprocess_only && [[ -n ${KERNEL_CLANG_AS_LOG:-} ]]; then
        printf '%s\t%s\n' "$mode" "${source#*/linux/}" >> "$KERNEL_CLANG_AS_LOG"
    fi
    exec "$clang" "${original[@]}"
fi

if $preprocess_only; then
    if [[ -n $out ]]; then cpp_args+=(-o "$out"); fi
    exec "$FCC" -E -x assembler-with-cpp "${cpp_args[@]}" "$source"
fi

if [[ -z $source || -z $out ]]; then
    echo 'fcc Kbuild assembler requires an input and -o output' >&2
    exit 1
fi

scratch=$(mktemp -d)
trap 'rm -rf -- "$scratch"' EXIT
asm_input=$source
if [[ $language == assembler-with-cpp || ( $language == none && $source == *.S ) ]]; then
    asm_input=$scratch/input.s
    if $deps; then
        if ! $dep_file; then cpp_args+=(-MF "${out%.*}.d"); fi
        if ! $dep_target; then cpp_args+=(-MT "$out"); fi
    fi
    "$FCC" -E -P -x assembler-with-cpp "${cpp_args[@]}" "$source" -o "$asm_input"
fi
"$FCC_AS" "${as_args[@]}" "$asm_input" -o "$out"

if [[ -n ${KERNEL_AS_DIFF_DIR:-} ]]; then
    compare=$KERNEL_AS_DIFF_DIR/${out#/}.diff
    mkdir -p -- "$(dirname -- "$compare")"
    root=$(cd -- "$(dirname -- "$0")/../.." && pwd)
    {
        printf '%s\n' "$source"
        if "$clang" -c "${ref_args[@]}" -o "$scratch/clang.o" > "$scratch/clang.log" 2>&1; then
            if python3 -B "$root/test/fas_as/corpus_check.py" --compare "$scratch/clang.o" "$out" > "$scratch/diff" 2>&1; then
                echo MATCH
            else
                echo DIFF
                cat "$scratch/diff"
            fi
        else
            echo 'CLANG ERROR'
            cat "$scratch/clang.log"
        fi
    } > "$compare"
fi
