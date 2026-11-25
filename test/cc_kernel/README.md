# Linux x86-64 boot test

Run `bash test/cc_kernel.sh` from the Fas checkout. Set `FCC`, `FCC_FIR`
and `FCC_LD` to absolute paths to the built tools when using a nondefault
build directory. The script fetches the SHA-256-pinned Linux 6.12.119 source
through the adjacent cc-tests checkout. It builds out of tree under
`cc-tests/kernel-build/tiny-fcc` by default. Use `KERNEL_BUILD` for a clean
build or `KERNEL_JOBS` to change the parallelism.

The host needs Clang/LLVM 22, GCC (or `KERNEL_HOSTCC`), GNU make, bison,
flex, bc, Python 3, libelf development files for objtool, and
`qemu-system-x86_64`. Objtool finds libelf through `pkg-config`. When the
headers are unpacked into a private prefix rather than installed, point
`PKG_CONFIG_PATH` at its `usr/lib/x86_64-linux-gnu/pkgconfig` and set
`PKG_CONFIG_SYSROOT_DIR` to the prefix. The runtime `libelf.so.1` must still
come from the host. Use `bash test/cc_kernel.sh clang` for a control build
with the same configuration in `cc-tests/kernel-build/tiny-clang`.

The configuration starts from upstream `tinyconfig`. It enables printk,
the 8250 serial console, ELF userspace and initrd support. ACPI provides the
machine power-off operation in QEMU, and PCI lets ACPI initialize the QEMU
firmware's PCI configuration regions. The test succeeds when the static init
writes `FCC_KERNEL_INIT_OK` to ttyS0 and the kernel powers off. The full
console transcript is saved in `boot.log`.

The CC adapter selects Kbuild's Clang configuration, runs Clang on assembly
inputs, on `-m16` C in `arch/x86/boot` and `arch/x86/realmode`, and on
`-m32` C in `arch/x86/entry/vdso/vdso32`. All other target C goes to fcc,
including the x86-64 decompressor. The harness reports the count and paths
of Clang-routed C inputs, retaining the list in `clang-c.tsv` for incremental
builds. TODO: 32/16-bit x86 codegen in fcc removes this. Three
Kbuild offset generators expect GNU assembly from `-S`. For those inputs
the adapter extracts their `.ascii` layout markers from fcc's object
files. LLVM 22 supplies the link and object utilities for this stage.
With `KERNEL_LD=fas`, the LD adapter `test/cc_kernel/ld.sh` sends relocatable
(`-r`) links, which build `vmlinux.o`, to fas-ld (`FCC_LD`). All other links
still go to ld.lld until fas-ld supports the kernel's linker scripts. Each
link routed to fas-ld is listed in `fas-ld.tsv` in the build directory.

The fcc tinyconfig sets `CONFIG_SECTION_MISMATCH_WARN_ONLY` so modpost
reports section references through out-of-line inline helpers as warnings.
This Kbuild diagnostic setting does not change which code is built. Both the
Clang control and the fcc tinyconfig image reach init and power off in QEMU.

## Defconfig and the BusyBox boot check

Run `bash test/cc_kernel.sh fcc defconfig` to try upstream
`x86_64_defconfig` under `cc-tests/kernel-build/defconfig-fcc`. The script
builds a static BusyBox from the pinned source with Clang and packages its
ash shell and poweroff applet. The initramfs boots into the shell, runs a
command that prints `FCC_KERNEL_SHELL_OK`, and powers off.

Upstream defconfig enables `CONFIG_IA32_EMULATION`, which builds the compat
vDSO's C files with `-m32` through the adapter. The harness uses upstream
`x86_64_defconfig` without changing its options.
