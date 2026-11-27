#!/usr/bin/env bash
set -euo pipefail

clang=${CLANG:-clang-22}
here=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
if [[ $# == 1 && $1 == --version ]]; then
    echo "fcc (Kbuild clang configuration)"
    exit 0
fi
if [[ $# == 1 && $1 == -print-file-name=plugin ]]; then
    echo plugin
    exit 0
fi

args=()
output=false
out_path=
offsets=false
target=
setup_c=
asm_index=-1
for arg; do
    if $output; then
        out_path=$arg
        args+=("$arg")
        output=false
        continue
    fi
    case "$arg" in
        -o) output=true ;;
        -m16|-m32|-m64) target=$arg ;;
        -S) asm_index=${#args[@]} ;;
        --target=x86_64-linux-gnu) continue ;; # fcc targets this ABI by default.
        -mpreferred-stack-boundary=*|-fmin-function-alignment=*|-falign-jumps=*|-fzero-init-padding-bits=*)
            # fcc warns and continues on these options. Fail Kbuild's
            # capability probes so it does not add them to the shared
            # flags and pass them to Clang on real-mode and compat inputs.
            echo "fcc Kbuild adapter: unsupported option $arg" >&2
            exit 1
            ;;
        */arch/x86/kernel/asm-offsets.c|*/kernel/bounds.c|*/scripts/mod/devicetable-offsets.c) offsets=true ;;
        arch/x86/boot/*.c|*/arch/x86/boot/*.c) setup_c=$arg ;;
        -*) ;;
        *.S|*.s|assembler|assembler-with-cpp) exec "$clang" "$@" ;;
    esac
    args+=("$arg")
done

# Keep the largest setup objects small enough for the kernel's 32 KiB limit.
if [[ $target == -m16 && -n $setup_c && ${KERNEL_FCC_SETUP:-0} != 1 ]]; then
    case "${setup_c##*/}" in
        cmdline.c|printf.c|string.c|video.c)
            if [[ -n ${KERNEL_CLANG_C_LOG:-} ]]; then
                printf '%s\tarch/x86/%s\n' "$target" "${setup_c#*arch/x86/}" >> "$KERNEL_CLANG_C_LOG"
            fi
            exec "$clang" "$@" -Wno-unknown-warning-option
            ;;
    esac
fi

# The native i386 object emitter has no DWARF writer yet. Keep the assembly
# invocation intact (the dispatch above has already handled it).
if [[ $target == -m16 || $target == -m32 ]]; then
    for index in "${!args[@]}"; do
        if [[ ${args[index]} == -g ]]; then unset 'args[index]'; fi
    done
fi

# Kbuild's offset generators put constant layout values in .ascii directives.
# The native fcc backend can compile those directives directly. Recover their
# values from its object for Kbuild's sed rule, which expects GNU assembly text.
if $offsets && (( asm_index >= 0 )); then
    args[asm_index]=-c
    "$FCC" "${args[@]}"
    scratch=$(mktemp -d)
    trap 'rm -rf -- "$scratch"' EXIT
    "${KERNEL_OBJCOPY:-llvm-objcopy-22}" --dump-section=".text=$scratch/text" "$out_path"
    python3 - "$scratch/text" "$out_path" <<'PY'
from pathlib import Path
import re
import sys

text = Path(sys.argv[1]).read_bytes()
rows = re.findall(rb'->([A-Za-z_][A-Za-z_0-9]*) [$#]?(-?(?:0x[0-9A-Fa-f]+|[0-9]+)) ', text)
if not rows:
    sys.exit("fcc Kbuild adapter: missing offset markers in .text")
Path(sys.argv[2]).write_bytes(b"".join(b'\t.ascii "->%s %s "\n' % row for row in rows))
PY
    exit
fi

# Kbuild only recognizes GCC and Clang. Select its Clang configuration
# for the version probe while letting fcc see its own macros in kernel C.
if [[ ${*: -5} == '-E -P -x c -' ]]; then
    source_text=$(cat)
    if [[ $source_text == *'Clang'*'__clang_major__'*'__GNUC_PATCHLEVEL__'* ]]; then
        exec "$clang" "$@" <<< "$source_text"
    fi
    exec "$FCC" "${args[@]}" <<< "$source_text"
fi

# Kernel fallbacks for optimizations fcc does not have yet.
# TODO: fallbacks.h lists what removes each entry.
exec "$FCC" -include "$here/fallbacks.h" "${args[@]}"
