#!/usr/bin/env bash
# Send relocatable links and the final scripted x86-64 vmlinux link to
# fas-ld. Kbuild's version probe, kallsyms passes and auxiliary images use
# ld.lld. Each fas-ld invocation is logged to $KERNEL_LD_LOG.
set -euo pipefail

lld=${KERNEL_LLD:-ld.lld-22}
fas_ld=${FCC_LD:?FCC_LD must name the fas-ld binary}
relocatable=false
output=
script=
next=
for arg; do
    if [[ $next == output ]]; then
        output=$arg
        next=
        continue
    elif [[ $next == script ]]; then
        script=$arg
        next=
        continue
    fi
    case "$arg" in
        -r|--relocatable) relocatable=true ;;
        -o|--output) next=output ;;
        -T|--script) next=script ;;
        --script=*) script=${arg#*=} ;;
        -T?*) script=${arg#-T} ;;
    esac
done
if $relocatable || [[ ${output##*/} == vmlinux && /${script#./} == */arch/x86/kernel/vmlinux.lds ]]; then
    [[ -n ${KERNEL_LD_LOG:-} ]] && printf 'fas-ld\t%s\n' "$*" >> "$KERNEL_LD_LOG"
    exec "$fas_ld" "$@"
fi
exec "$lld" "$@"
