#!/usr/bin/env bash
# Build the pinned Linux kernel and boot an initramfs in QEMU.
set -euo pipefail
ulimit -f 524288
ulimit -c 0

root=$(cd -- "$(dirname -- "$0")/.." && pwd)
tests=$(realpath -- "${CC_TESTS:-$root/../cc-tests}")
export FCC=${FCC:-$root/_build/fcc}
export CLANG=${CLANG:-clang-22}
export FCC_FIR=${FCC_FIR:-$root/_build/default/tools/fas_fir/fas_fir.exe}
export FCC_LD=${FCC_LD:-$root/_build/fas-ld}
export FCC_INCLUDE=${FCC_INCLUDE:-$root/cc/include}
usage() {
    echo "usage: $0 [fcc|clang] [tinyconfig|defconfig]" >&2
    exit 2
}
(( $# <= 2 )) || usage
mode=${1:-fcc}
profile=${2:-tinyconfig}
case "$mode" in
    fcc) compiler=$root/test/cc_kernel/cc.sh ;;
    clang) compiler=$CLANG ;;
    *) usage ;;
esac
case "$profile" in
    tinyconfig) build_name=tiny; projects=(linux) ;;
    defconfig) build_name=defconfig; projects=(linux busybox) ;;
    *) usage ;;
esac
jobs=${KERNEL_JOBS:-3}
case "$jobs" in
    [1-6]) ;;
    0*|*[!0-9]*) echo 'KERNEL_JOBS must be a positive integer' >&2; exit 2 ;;
    *) jobs=6 ;;
esac

build=$(realpath -m -- "${KERNEL_BUILD:-$tests/kernel-build/$build_name-$mode}")
mkdir -p "$build/tmp"
export TMPDIR=$build/tmp
if [[ $mode == fcc ]]; then
    export KERNEL_CLANG_C_LOG=$build/clang-c.tsv
    : > "$KERNEL_CLANG_C_LOG"
    report_clang_c() {
        local -a sources=()
        mapfile -t sources < <(LC_ALL=C sort -u "$KERNEL_CLANG_C_LOG")
        printf 'Clang target C fallback in this run (%d files)\n' "${#sources[@]}"
        if (( ${#sources[@]} )); then printf '  %s\n' "${sources[@]}"; fi
    }
    trap report_clang_c EXIT
fi
sh "$tests/fetch.sh" --projects "${projects[@]}"
src=$tests/upstream/linux
# KERNEL_LD=fas links vmlinux.o and the supported kernel scripts with fas-ld.
linker=()
case "${KERNEL_LD:-lld}" in
    lld) ;;
    fas)
        linker=(LD="$root/test/cc_kernel/ld.sh" HOSTLD="$root/test/cc_kernel/ld.sh")
        export KERNEL_LD_LOG=$build/fas-ld.tsv
        : > "$KERNEL_LD_LOG"
        ;;
    *) echo 'KERNEL_LD must be lld or fas' >&2; exit 2 ;;
esac
kmake() {
    make -C "$src" O="$build" ARCH=x86_64 LLVM=-22 HOSTCC="${KERNEL_HOSTCC:-gcc}" CC="$compiler" \
        "${linker[@]}" "$@"
}

if [[ ! -e $build/.config ]]; then
    kmake "$profile"
    if [[ $profile == tinyconfig ]]; then
        "$src/scripts/config" --file "$build/.config" \
            -e PRINTK -e TTY -e SERIAL_8250 -e SERIAL_8250_CONSOLE \
            -e BINFMT_ELF -e BLK_DEV_INITRD -e ACPI -e PCI
        kmake olddefconfig
    fi
fi
# fcc can emit local inline helpers out of line, so let modpost report
# their calls into init sections without aborting the boot experiment.
if [[ $mode == fcc && $profile == tinyconfig ]] && ! grep -qx 'CONFIG_SECTION_MISMATCH_WARN_ONLY=y' "$build/.config"; then
    "$src/scripts/config" --file "$build/.config" -e SECTION_MISMATCH_WARN_ONLY
    kmake olddefconfig
fi
# gen_init_cpio can add /dev/console without root privileges.
"$CLANG" -O2 "$src/usr/gen_init_cpio.c" -o "$build/gen_init_cpio"
printf '%s\n' 'dir /dev 0755 0 0' 'nod /dev/console 0600 0 0 c 5 1' > "$build/initramfs.list"
if [[ $profile == tinyconfig ]]; then
    "$CLANG" -std=gnu23 -ffreestanding -fno-stack-protector -nostdlib -static \
        "$root/test/cc_kernel/init.c" -o "$build/kernel-test-init"
    printf '%s\n' "file /init $build/kernel-test-init 0755 0 0" >> "$build/initramfs.list"
    marker=FCC_KERNEL_INIT_OK
    init_args='rdinit=/init'
    memory=256
else
    busybox=$build/busybox
    mkdir -p "$busybox"
    bmake() {
        make -C "$tests/upstream/busybox" O="$busybox" CC="$CLANG" "$@"
    }
    if [[ ! -e $busybox/.config ]]; then
        # Provide only the shell and the poweroff applet in the initramfs.
        bmake allnoconfig
        "$src/scripts/config" --file "$busybox/.config" \
            -e STATIC -e BUSYBOX -e SH_IS_ASH -d SH_IS_NONE -e ASH_PRINTF -e POWEROFF
        printf '\n%.0s' {1..256} | bmake oldconfig
    fi
    bmake -j"$jobs" busybox
    printf '%s\n' 'dir /bin 0755 0 0' "file /bin/busybox $busybox/busybox 0755 0 0" \
        'slink /bin/sh busybox 0777 0 0' "file /boot-check $root/test/cc_kernel/init.sh 0755 0 0" >> "$build/initramfs.list"
    marker=FCC_KERNEL_SHELL_OK
    init_args='rdinit=/bin/sh -- /boot-check'
    memory=512
fi
"$build/gen_init_cpio" "$build/initramfs.list" > "$build/initramfs.cpio"

kmake -j"$jobs" bzImage
timeout "${KERNEL_BOOT_TIMEOUT:-120}" qemu-system-x86_64 -m "$memory" -nographic -no-reboot \
    -kernel "$build/arch/x86/boot/bzImage" -initrd "$build/initramfs.cpio" \
    -append "console=ttyS0 panic=-1 $init_args" | tee "$build/boot.log"
grep -qFx "$marker" < <(tr -d '\r' < "$build/boot.log")
grep -qF 'reboot: Power down' "$build/boot.log"
