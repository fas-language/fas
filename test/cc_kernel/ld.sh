#!/usr/bin/env bash
# Kbuild LD adapter: relocatable (-r) links, which build vmlinux.o, go to
# fas-ld; every other link (and Kbuild's version probe) goes to ld.lld until
# fas-ld handles the kernel's linker scripts. Each routed link is logged to
# $KERNEL_LD_LOG.
set -euo pipefail

lld=${KERNEL_LLD:-ld.lld-22}
fas_ld=${FCC_LD:?FCC_LD must name the fas-ld binary}
relocatable=false
for arg; do
    [[ $arg == -r || $arg == --relocatable ]] && relocatable=true
done
if $relocatable; then
    [[ -n ${KERNEL_LD_LOG:-} ]] && printf 'fas-ld\t%s\n' "$*" >> "$KERNEL_LD_LOG"
    exec "$fas_ld" "$@"
fi
exec "$lld" "$@"
