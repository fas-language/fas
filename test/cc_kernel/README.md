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
inputs, and sends target C to fcc. Three Kbuild offset generators expect
GNU assembly from `-S`. For those inputs the adapter extracts their
`.ascii` layout markers from fcc's object files. LLVM 22 supplies the link
and object utilities for this stage.

The Clang control reaches init and powers off. The fcc build has several
remaining blockers:

- `arch/x86/boot/a20.c` and the other real-mode setup files use `-m16`.
  The `bzImage` setup link needs ELF32 i386 objects. fcc targets LP64.
- `arch/x86/include/asm/cpufeature.h` uses `__always_inline` to bring a
  caller's constant into an `asm goto` immediate operand.
- The same helper passes a static object's address plus an offset as an
  `"i"` immediate. fcc currently lowers the address to an SSA value, which
  Fir rejects for this constraint.
- `arch/x86/include/asm/asm.h` binds a C register variable to `rsp` and
  uses it as an inline asm operand. Fir currently rejects that operand.
- Inline asm in the real-mode and vDSO code contributes exception and
  alternative-instruction records to `__ex_table` and `.altinstructions`.
  The native backend currently rejects those extra sections.

The driver also reports `-mcmodel=kernel` and `-mno-red-zone` as unsupported.
