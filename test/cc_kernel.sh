#!/usr/bin/env bash
# Build the pinned Linux tinyconfig and boot a static init in QEMU.
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
compiler=$root/test/cc_kernel/cc.sh
case "${1:-fcc}" in
    fcc) ;;
    clang) compiler=$CLANG ;;
    *) echo "usage: $0 [fcc|clang]" >&2; exit 2 ;;
esac

build=$(realpath -m -- "${KERNEL_BUILD:-$tests/kernel-build/tiny-${1:-fcc}}")
mkdir -p "$build/tmp"
export TMPDIR=$build/tmp
sh "$tests/fetch.sh" --projects linux
src=$tests/upstream/linux
kmake() {
    make -C "$src" O="$build" ARCH=x86_64 LLVM=-22 HOSTCC="${KERNEL_HOSTCC:-gcc}" CC="$compiler" "$@"
}

if [[ ! -e $build/.config ]]; then
    kmake tinyconfig
    "$src/scripts/config" --file "$build/.config" \
        -e PRINTK -e TTY -e SERIAL_8250 -e SERIAL_8250_CONSOLE \
        -e BINFMT_ELF -e BLK_DEV_INITRD -e ACPI -e PCI
    kmake olddefconfig
fi
kmake -j"${KERNEL_JOBS:-6}" bzImage

# gen_init_cpio can add /dev/console without root privileges.
"$CLANG" -std=gnu23 -ffreestanding -fno-stack-protector -nostdlib -static \
    "$root/test/cc_kernel/init.c" -o "$build/kernel-test-init"
"$CLANG" -O2 "$src/usr/gen_init_cpio.c" -o "$build/gen_init_cpio"
printf '%s\n' 'dir /dev 0755 0 0' 'nod /dev/console 0600 0 0 c 5 1' \
    "file /init $build/kernel-test-init 0755 0 0" > "$build/initramfs.list"
"$build/gen_init_cpio" "$build/initramfs.list" > "$build/initramfs.cpio"

timeout "${KERNEL_BOOT_TIMEOUT:-120}" qemu-system-x86_64 -m 256 -nographic -no-reboot \
    -kernel "$build/arch/x86/boot/bzImage" -initrd "$build/initramfs.cpio" \
    -append 'console=ttyS0 rdinit=/init panic=-1' | tee "$build/boot.log"
grep -qFx 'FCC_KERNEL_INIT_OK' < <(tr -d '\r' < "$build/boot.log")
grep -qF 'reboot: Power down' "$build/boot.log"
