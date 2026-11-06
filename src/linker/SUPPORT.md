# Linux x86-64 compatibility contract candidate

This describes the intended first production scope. The executable still reports
`0.7-dev`; the final freeze, hosted qualification and publication have not
happened. [Recorded acceptance](corpus/results/) identifies exact source/binary
snapshots. A feature description is not evidence that every compiler, libc,
kernel or build system using it has been qualified.

| Boundary | Declared behavior |
| --- | --- |
| Target | Linux x86-64, ELF64 little-endian, System V/Linux ABI version zero; other targets fail explicitly |
| Producers | Native Fas objects and ordinary GCC/Clang C objects within the [supported ELF subset](README.md#supported-contract) |
| Libraries | GNU/SysV regular/thin archives and the documented Linux shared-object/script subset |
| Outputs | Static/dynamic ET_EXEC, loader-backed PIE, documented CRT-relocated static PIE, ET_DYN shared libraries and partial ET_REL |
| Default invocation | `a.out`, `_start`, static ET_EXEC; no implicit library search directories |
| Dynamic policy | Eager binding, RELRO enabled, non-executable stack, no writable executable load segment or text relocations |
| Dynamic metadata defaults | SysV hash, new dtags, no build ID; explicit options select the documented alternatives |
| Library discovery | Explicit `-L`, sysroot, runpath/rpath and strict-dependency options; not the entire GNU implicit host search policy |
| Partial links | No implicit `_start` archive extraction; valid inputs with no selected objects emit a reusable empty ET_REL; no input files remains an error |
| Forced roots in ET_REL | Missing `-u NAME` roots survive as strong undefined declarations, including across stages, following GNU's policy; LLD 22's partial-link policy differs |
| Driver integration | C compiler drivers must provide suitable CRT/libc inputs and supported linker flags; Fas's hosted driver discovers these through its configured C compiler |
| Invocation lifetime | Independent symbol/selection/options state; repeated and concurrent library invocations do not share mutable link state |
| Installation | Native `fas-ld` CLI, staged installation through `DESTDIR`; no installed OCaml package/API |

The [README](README.md) defines supported relocation/TLS/version/script forms,
archive ordering, symbol precedence, section GC, output alias handling and
resource bounds. Unlisted GNU flags or arbitrary scripts do not become part of
the contract merely because a driver could emit them. Unknown options and
unsupported semantics fail explicitly. `--help` describes the user-facing CLI;
`--version` prints the actual embedded development identity.

Expected link and I/O failures use exit status 1 with `fas-ld:` diagnostics;
successful CLI actions use status 0. Diagnostic prose and internal OCaml
exceptions are not a machine-readable diagnostic schema. `--stats` is optional
measurement output and is not a promise of a fixed public statistics schema.
Identical inputs/options have deterministic output within the documented scope;
input path strings in metadata/maps remain inputs to that promise.

Output is built completely before publication through an individual-file rename.
Reported failures preserve the prior artifact; binary/map publication comprises
two separate renames. A killed writer can leave a private incomplete temporary.
Crash durability through fsync and a binary/map transaction are not promised.
The declared input/output limits bound accepted work, not maximum process RSS.
See the [operational regression evidence](corpus/results/2026-11-06-release/README.md).

## Qualified environment versus output format

The clean reference userspace is Ubuntu 24.04.5 x86-64, OCaml 5.4.1/Dune 3.24.2,
GCC 13.3/Clang 18.1.3, binutils 2.42, glibc 2.39 and an isolated musl 1.2.5
sysroot. Workstation checks add Ubuntu 26.04.1 and newer GCC/Clang producers.
The containers share the workstation kernel; another-kernel and hosted-runner
qualification remain open. Exact tool versions and tested selections live in
individual reports, rather than being inferred from these product names.

The distributed linker's runtime environment is separate from the libc used by
programs it links. The native OCaml executable embeds its runtime and needs no
OCaml installation, Python or external linker at runtime. Each binary archive
records its actual ELF interpreter, shared dependencies and required GLIBC
symbol versions. System shared libraries are not bundled. Target musl support
does not mean the distributed fas-ld executable itself is a musl binary.

Static PIE support is the documented glibc/musl CRT subset, not general static
TLS relaxation. Existing C++ exception/RTTI/lifecycle coverage remains usable
regression coverage; broader C++ implementation is not a release requirement.
LLVM/GCC LTO is deliberately unsupported with no implementation planned. Native
objects compiled ahead of time by LLVM can participate in ordinary native links.
Arbitrary kernel/bare-metal layouts, PE/COFF, Windows and other CPUs are later
target projects. The implementation remains OCaml/Unix until the Fas port.

## Freeze and acceptance gates

The supported CLI and compiler integration must be frozen against passing
regression, generated semantic and real-program selections before a production
version is published. Internal OCaml modules currently have inferred interfaces
and are source-version-coupled with the Fas compiler. Their current visibility
does not promise a separately versioned stable public library API. Any future
public package needs explicit interfaces and an independent compatibility gate.

The current [CI recipe](ci/README.md) requires the complete declared case count,
no reported regression skips, all corpus/scale/semantic reports and exact
binary/source identity. Generated semantic qualification covers valid programs
through independent final consumers and mixed partial-link pipelines; malformed
input fuzzing checks separate failure properties. Both are required evidence.
The weekly fuzz selection budgets ten minutes per target; this is a bounded
campaign, not continuous fuzzing or proof of correctness. Publish actual
elapsed time, coverage metrics and every finding with its source identity.

Source/binary archives must pass integrity checks, source rebuilding and a linked
program smoke test. Checksums/inventories are not signatures. Hosted CI execution,
long-running qualification, final contract review and durable publication remain
release gates. Performance work remains deferred until correctness qualification;
a faster-than-LLD result is not a production-readiness gate or current claim.
