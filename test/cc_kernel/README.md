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

The CC adapter selects Kbuild's Clang configuration and runs Clang on
assembly inputs. Linux's 32 KiB BIOS setup size limit still requires Clang
for `cmdline.c`, `printf.c`, `string.c`, and `video.c` under
`arch/x86/boot/`. fcc compiles the other 16 setup files,
the six `-m16` trampoline files, the two `-m32` compat vDSO files, and the
other target C, including the x86-64 decompressor. The mixed defconfig setup
replay ends at `0x7910`. Set `KERNEL_FCC_SETUP=1` to try compiling all setup
C with fcc. In a size measurement the all-native setup ended at `0x98b0`.
The harness records which Clang C fallbacks it invoked in `clang-c.tsv`.
Rebuild the setup objects when measuring fallback use or changing
`KERNEL_FCC_SETUP`. The log covers the current invocation and Kbuild reuses
existing objects across runs. Three Kbuild offset generators expect GNU
assembly from `-S`. For those inputs the adapter extracts their `.ascii`
layout markers from fcc's object files.
With `KERNEL_LD=fas`, the LD adapter `test/cc_kernel/ld.sh` sends relocatable
(`-r`) links, including the three objtool intermediate objects, to fas-ld
(`FCC_LD`). It also handles the preliminary kallsyms links and the scripts
for `vmlinux`, the loaderless decompressor PIE, `setup.elf`, `realmode.elf`,
and both shared vDSOs. This covers every target link recorded in the
defconfig audit. The build directory's `fas-ld.tsv` lists each fas-ld
invocation.

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
With `KERNEL_LD=fas`, the defconfig image reached that marker and powered
off under both SeaBIOS and OVMF in QEMU.

Upstream defconfig enables `CONFIG_IA32_EMULATION`, which builds the compat
vDSO's C files with `-m32` through the adapter. The harness uses upstream
`x86_64_defconfig` without changing its options.
`bash test/cc_i386.sh` exercises the ILP32 data model, cdecl, `-mregparm=3`,
and real-mode code generation against Clang.

### Defconfig compiler and linker baseline

The Linux 6.12.119 baseline used `KERNEL_LD=fas`. The counts below come
from the defconfig boot transcript, the build directory's `clang-c.tsv`,
and Kbuild's saved `.cmd` commands. C counts are distinct source paths.
Link counts cover build outputs and exclude tool-version and feature probes.

| Tool path | Count | Inputs or outputs |
| --- | ---: | --- |
| Clang `-m16` C | 20 | `arch/x86/boot/*.c` |
| Clang `-m16` C | 6 | `arch/x86/realmode/rm/*.c` |
| Clang `-m32` C | 2 | Compat vDSO `vclock_gettime.c` and `vgetcpu.c` |
| Clang assembly | 56 | 39 x86-64, 11 real-mode, 3 compat i386, and 3 generated x86-64 kallsyms objects |
| Clang linker-script preprocessing | 5 | Main kernel, decompressor, real mode, and the two vDSOs (`.lds.S` sources) |
| `ld.lld` i386 | 3 | `arch/x86/boot/setup.elf`, `arch/x86/realmode/rm/realmode.elf`, `arch/x86/entry/vdso/vdso32.so.dbg` |
| `ld.lld` x86-64 | 4 | Two preliminary kallsyms links, `arch/x86/boot/compressed/vmlinux` (scripted PIE), and `arch/x86/entry/vdso/vdso64.so.dbg` (scripted shared link) |
| `fas-ld` x86-64 | 2 | Relocatable `vmlinux.o` and the final scripted `vmlinux` |
| `ld.lld` via Kbuild `HOSTLD` | 3 | Objtool, its x86 backend, and libsubcmd intermediate `-r` links |

The CC version probe uses Clang's predefined macros. The LD version and
capability probes also use `ld.lld`. The three C offset generators go through
fcc. `arch/x86/tools/relocs` is a GCC-built host executable linked from
three objects. Kbuild runs it three times, once for `realmode.elf` and twice
for `vmlinux` (extraction and absolute-relocation checks). `llvm-objcopy-22`
extracts `setup.bin`, `realmode.bin`, and the kernel payload, and strips the
vDSOs. Kbuild's host `arch/x86/boot/tools/build` combines setup and the
compressed payload into `bzImage`.

The boot harness also uses Clang for one host executable (`gen_init_cpio`)
and for the BusyBox initramfs (149 saved compiler commands and the
`busybox_unstripped` driver link). Those builds run outside the kernel CC
and LD adapters.
