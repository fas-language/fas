# Linux x86-64 boot test

Run `bash test/cc_kernel.sh` from the Fas checkout. Set `FCC`, `FCC_FIR`
and `FCC_LD` to absolute paths to the built tools when using a nondefault
build directory. The script fetches the SHA-256-pinned Linux 6.12.119 source
through the adjacent cc-tests checkout. It builds out of tree under
`cc-tests/kernel-build/tiny-fcc` by default. Use `KERNEL_BUILD` for a clean
build or `KERNEL_JOBS` to change the parallelism.

The host needs Clang/LLVM 22, GCC (or `KERNEL_HOSTCC`), GNU make, bison,
flex, bc, Python 3, libelf development files for objtool, and
`qemu-system-x86_64`. Use `bash test/cc_kernel.sh clang` for a control build
with the same configuration in `cc-tests/kernel-build/tiny-clang`.

The configuration starts from upstream `tinyconfig`. It enables printk,
the 8250 serial console, ELF userspace and initrd support. ACPI provides the
machine power-off operation in QEMU, and PCI lets ACPI initialize the QEMU
firmware's PCI configuration regions. The test succeeds when the static init
writes `FCC_KERNEL_INIT_OK` to ttyS0 and the kernel powers off. The full
console transcript is saved in `boot.log`.

The CC adapter selects Kbuild's Clang configuration, runs Clang on assembly
inputs and on `-m16` C in `arch/x86/boot` and `arch/x86/realmode`. All
other target C goes to fcc, including the x86-64 decompressor. Three
Kbuild offset generators expect GNU assembly from `-S`. For those inputs
the adapter extracts their `.ascii` layout markers from fcc's object
files. LLVM 22 supplies the link and object utilities for this stage.

The Clang control reaches init and powers off. The fcc build reaches the
`vmlinux.o` link. Kbuild's `modpost` currently reports section mismatches
because functions marked for sections such as `.init.text` are emitted in
`.text`.
