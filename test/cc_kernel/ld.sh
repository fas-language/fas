#!/usr/bin/env bash
# Send relocatable links and supported kernel linker scripts to fas-ld.
# Each fas-ld invocation is logged to $KERNEL_LD_LOG.
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
scripted=false
case "${output##*/}:/${script#./}" in
    vmlinux:*/arch/x86/kernel/vmlinux.lds|.tmp_vmlinux[123]:*/arch/x86/kernel/vmlinux.lds|\
    vmlinux:*/arch/x86/boot/compressed/vmlinux.lds|\
    setup.elf:*/arch/x86/boot/setup.ld|\
    realmode.elf:*/arch/x86/realmode/rm/realmode.lds|\
    vdso32.so.dbg:*/arch/x86/entry/vdso/vdso32/vdso32.lds|\
    vdso64.so.dbg:*/arch/x86/entry/vdso/vdso.lds) scripted=true ;;
esac
if $relocatable || $scripted; then
    [[ -n ${KERNEL_LD_LOG:-} ]] && printf 'fas-ld\t%s\n' "$*" >> "$KERNEL_LD_LOG"
    exec "$fas_ld" "$@"
fi
exec "$lld" "$@"
