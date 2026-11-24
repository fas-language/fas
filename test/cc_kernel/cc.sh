#!/usr/bin/env bash
set -euo pipefail

clang=${CLANG:-clang-22}
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
        -S) asm_index=${#args[@]} ;;
        --target=x86_64-linux-gnu) continue ;; # fcc targets this ABI by default.
        -mpreferred-stack-boundary=*)
            # Reject Kbuild's GCC-only probe. Otherwise fcc's warning lets
            # this GCC flag reach the Clang driver for real-mode .S files.
            echo "fcc Kbuild adapter: unsupported GCC option $arg" >&2
            exit 1
            ;;
        */arch/x86/kernel/asm-offsets.c|*/kernel/bounds.c|*/scripts/mod/devicetable-offsets.c) offsets=true ;;
        -*) ;;
        *.S|*.s|assembler|assembler-with-cpp) exec "$clang" "$@" ;;
    esac
    args+=("$arg")
done

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

exec "$FCC" "${args[@]}"
