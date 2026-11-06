# fas-ld roadmap: v0.1 to v1.0

These are linker versions, independent of Fas language versions. Releases are
defined by working acceptance cases, not dates. Initial production scope is
Linux x86-64 ELF. OCaml is the implementation language until the later Fas port.

The [2026-11-05 release audit](AUDIT.md) identified synthetic export and permission
bugs plus C registry and legacy lifecycle gaps. Fixes now give dynamic synthetic
exports image-relative semantics, preserve creation permissions, implement the
declared registry subset and reject unsupported legacy lifecycle lists. Release
qualification remains separate from passing these regression gates.

Current priority (user decision, 2026-11-05): finish the declared Linux Fas/C
functionality and production qualification before the main performance pass.
Keep the existing timing evidence as a baseline; do not schedule optimization
campaigns while compatibility and release gates remain the active work.

Fas and native C are the release priorities. Existing C++ tests remain useful
regression coverage; keep that usable interoperability level without expanding
the C++ scope. Compiler-specific LTO is explicitly out of scope, including future
roadmap work, per the user's scope decision.

| Version | Deliverable | Acceptance gate |
| --- | --- | --- |
| **0.1 — implemented** | Static objects, strong/local symbols, six relocation types, checked input, atomic output | Cross-object code/data/BSS works; current tests and baseline pass |
| **0.2 — implemented** | Weak/common symbols, visibility, GNU/SysV archives and lazy member extraction; `-L`/`-l`, groups, whole-archive, response files; phase and allocation/GC measurements | Archive chains/cycles and symbol precedence agree with the declared semantics; errors name the member and reference |
| **0.3 — implemented freestanding subset** | Native ET_REL emission and compiler-driver integration, basic map files and linker-defined symbols | Actual integer/float/vector/argc Fas fixtures build without external tools; separate objects/archives and declared scalar external ABI pass. The current compiler is OCaml; Fas self-hosting remains a language milestone |
| **0.4 — implemented freestanding subset** | COMDAT, validated mergeable strings/data with conservative whole-section deduplication, section GC, GOT-based static PIC, GNU properties/LLVM addrsig, DWARF 4/5 and basic CFI unwind/index handling | GCC/Clang fixtures work without metadata-disabling flags; debugger/backtrace (including omitted frame pointers) and GC-root tests pass. Individual string/tail merging remains outside the original v0.4 subset; personality/LSDA pointers were added during v0.7 |
| **0.5 — implemented dynamic-executable subset** | Dynamically linked executables and PIE: shared-library inputs, eager GOT/PLT, dynamic relocations, required import versions, four traditional TLS code models, init/fini and explicit startup integration; minimal INPUT/GROUP/AS_NEEDED scripts | Hosted GCC/Clang C and Fas objects run against the installed glibc CRT/libc, including startup, shutdown, imported data and per-thread TLS isolation. Broader sysroot compatibility, TLS relaxation/TLSDESC and general scripts remain later gates |
| **0.6 — implemented declared shared-library subset** | Shared-library output, visibility/preemption, complete declared TLS models and symbol-version behavior, IFUNC where required; RELRO/binding options | Producer/consumer, preemption, GD/LD/IE/TLSDESC, late dlopen, lifecycle, named version aliases, imported/defined IFUNC and loader-enforced RELRO pass. Deliberate limits remain: no TLS relaxation or base-version aliases; alias hiding follows the tested LLD policy. Static IFUNC was added during v0.7 |
| **0.7 — in progress** | Broader Linux compatibility: declared glibc/musl sysroots, static libc requirements, needed script features, compiler-driver flag compatibility, larger object/section limits | A published real-program corpus works with independent compilers and loaders; unsupported features remain explicit |
| **0.8 — qualification in progress** | Real-library and build-system acceptance across the declared compiler/libc matrix; native Fas consumers, static/shared libraries and larger programs | Reproducible upstream tests and independent-linker runtime comparisons; successful hosted CI on each published baseline |
| **0.9 — release candidate** | Compatibility freeze, sustained coverage-guided fuzzing, generated/differential tests, failure injection, large-input CI and packaging | No open release-blocking correctness failures; publish fuzz duration/coverage and corpus results, reproducibility and performance reports |
| **1.0 — production Linux release** | Stable documented CLI/library and supported feature matrix, installable releases, diagnostics and operational reliability | Fas/C applications in that matrix build and run reliably; all release gates pass, with known limitations published |

The [2026-11-06 local qualification](corpus/results/2026-11-06/README.md) passes
seven real C projects with exact-input GNU comparisons, 4,974 official GCC
torture executions per linker and four libraries in both shared-library
directions. A fresh SQLite replay, four large-input groups and seven bounded
fuzz campaigns also pass. Pinned CI jobs now cover the expanded corpus and
require the full selected GCC execution count. Hosted execution, broader
compiler/libc configurations, sustained fuzzing and release artifacts remain
open gates; these local results do not complete v0.7 or v0.8.

The [fresh Ubuntu checkpoint](corpus/results/2026-11-06-clean/README.md) also
passes the shared correctness recipe on GCC 13/Clang 18 and glibc 2.39, with
GCC/Clang musl zlib PIE/static/static-PIE consumers. It repairs legacy unwind
termination and older debug-reader layout compatibility. The current host root
suite passes 3,792 compiler checks and 250 linker cases; the clean image passes
all 250 linker cases, scale, zlib, Chibicc, SQLite and installation gates.
This qualifies another userspace baseline while sharing the host kernel;
hosted execution, sustained fuzzing and release artifacts remain open.

Earlier performance work and all recorded measurements remain valid history.
The main optimization pass is now a later track, after production qualification,
per the user's revised priority. Malformed-input tests and fuzzing grow
continuously; hardening is not postponed until 0.9. Large milestones can have
several prereleases.

Version 0.3 can be useful before general hosted C support. Version 1.0 does not
claim every GNU ld flag/script, C++ ecosystem, LLVM bitcode LTO, Windows, or every
ELF architecture. Any added ecosystem needs its own compatibility corpus.

## Compatibility work explicitly tracked

| Audit area | Current status | Remaining scope / reason |
| --- | --- | --- |
| Partial linking (`ld -r`) | Implemented ET_REL combination, tested with GNU/LLD producers and consumers | Wider build-system qualification; partial GC and final-image options remain unsupported |
| Thin archives | GNU external and nested regular-archive members implemented and tested | Sustained fuzzing and wider build-system use |
| C++ interoperability | Exceptions, cleanup/rethrow, RTTI, lifecycle and late-loaded thread destructors pass focused compiler/runtime matrices | Preserve this usable level; ET_REL GNU unique definitions, static C++ and broader ecosystem expansion are outside current scope |
| Static PIE / static IFUNC | Static ET_EXEC IFUNC plus declared static PIE glibc/musl CRT startup, ASLR, TLS and glibc IFUNC ordering tested | Wider static PIE real-program qualification; no static TLS GD/LD/TLSDESC relaxation |
| Library discovery | SONAME/DT_NEEDED, RPATH/RUNPATH, strict transitive validation and explicit sysroot/script search; indirect references do not extract archives, matching tested GNU behavior | Full GNU default search policy remains qualification work; supply indirect callbacks as objects or explicit archive roots |
| Non-PIC imported data | Declared executable COPY subset, including aligned/read-only storage, versions, identity and preload overrides | Protected/unique/overlapping data and mixed spellings remain explicit PIC requirements; no text relocations |
| Linker scripts | Tested libc INPUT/GROUP/AS_NEEDED subset | Arbitrary kernel/bare-metal layouts are outside initial v1.0: they need a layout engine and dedicated startup/target tests |
| C registries / legacy lifecycle | C identifier boundaries, contiguous PROGBITS/NOBITS layout and conservative live-reference GC retention implemented; nonempty legacy lists explicitly rejected on final links | Registry policy is hidden and image-local; no selectable precise-GC mode. Modern lifecycle arrays remain supported and partial links preserve legacy lists |
| LTO | Explicit GCC/LLVM diagnostics, including selected archive members | Deliberately unsupported by user decision; no LTO implementation is planned |
| Large binaries | Object/section bounds, relocation overflows, GC, roughly 1 GiB sparse NOBITS and an opt-in runner for 64 MiB file-backed data/200k symbols and relocations/65k sections tested | Real large programs, peak memory and multicore scale qualification continue in v0.8/v0.9 |

These are tracked work items, with acceptance status below. The v0.7
Linux compatibility milestone includes the following before its scope freezes:

- **Partial linking (`-r`) — implemented declared subset**: combine objects into
  ET_REL, preserving relocations, local/global symbol references, common symbols,
  COMDAT groups, alignment and metadata. Multi-stage links, subsequent archive
  extraction, TLS/lifecycle/debug metadata, symbol versions and IFUNCs have
  independent GNU/LLD producer/consumer tests. No partial GC or final-image
  options; empty archive selections currently fail explicitly.
- **Thin archives — implemented, wider qualification ongoing**: lazy external
  members, paths relative to the archive, GNU indexes, nested regular-archive
  members, missing/changed members, bounded reads and publication alias checks
  have deterministic GNU-ld comparisons and full-suite regression checks.
  Container fuzzing passes a short campaign; sustained fuzzing and wider real
  build-system use remain qualification work.
- **Static libc — declared ET_EXEC subset implemented**: static musl and installed
  static glibc pass focused runtime tests. Static IFUNC emits canonical IPLT
  addresses and IRELATIVE records for CRT-owned startup. GCC/Clang, GNU/LLD
  consumers, threads, aligned TLS, memory functions, lifecycle, GC and real
  `-static` driver invocations pass. SystemTap notes and repeated GNU property
  notes from libc archives have independent format/interoperability checks.
  Static real-program replay also passes Chibicc and its 41 C tests, all four
  Fas Sieve programs and cJSON's 21 test programs, with independent GNU C links.
  Wider libc/sysroot qualification remains open. The static PIE subset now emits
  interpreter-free ET_DYN with CRT-owned self-relocation and archive-only inputs.
  Narrow local GOTPCRELX bootstrap rewrites make glibc entry viable before GOT
  relocation. GCC/Clang and independent GNU/LLD tests cover ASLR, relative data,
  lifecycle, aligned/threaded LE/IE TLS, glibc IFUNC ordering, absent weak calls,
  actual GCC `-static-pie` and concurrent deterministic reentry. Isolated musl
  CRT self-relocation/threads/lifecycle pass separately. Wider real static PIE
  programs and static TLS GD/LD/TLSDESC relaxation remain separate gates.
- **Library discovery**: SONAME/DT_NEEDED, conditional dependencies and explicit
  RPATH/RUNPATH selection are implemented. Fas/GNU/LLD producer/consumer tests
  cover runtime transitive chains, inherited paths and environment precedence.
  Opt-in strict link-time validation now follows transitive DT_NEEDED with
  `-rpath-link`, DSO paths, version and symbol checks, bounded reads and alias
  protection. GNU/LLD comparisons cover the declared explicit-search subset.
  Explicit `--sysroot`, `=`/`$SYSROOT` paths and bounded `SEARCH_DIR` handling now
  cover isolated glibc and musl static/dynamic startup, GNU ordering comparisons,
  nested/static-policy scripts, runtime string separation, alias protection and
  concurrent distinct-root invocation tests. Regular/thin archive order/group
  comparisons establish that indirect DSO references do not extract members,
  matching GNU ld. Explicit objects, whole archives, `-u` roots and direct DSO
  references supply callbacks. Full GNU default search policy remains a separate
  gate; the current default still delegates transitives to the loader.
- **C driver/build compatibility**: executable COPY relocations cover the declared
  data subset, with read-only copies protected after startup. `/dev/null` output,
  explicit multiple-definition policy and shared-output interpreter handling
  support Chibicc's unmodified self-hosting and driver suites. Both stages and
  all 82 C runtime comparisons pass. Wider compiler/libc versions remain open.
- **Native compiler hosted startup — implemented**: automatic C runtime selection
  and explicit hosted/freestanding overrides, positional C compilation,
  embedded C/assembly/packages and C harnesses supplying `main` have driver
  regressions. GCC/Clang dry-run plans supply CRT/interpreter/library policy;
  Fas performs the link. Glibc fixed/PIE/static/static-PIE and isolated musl
  fixed/PIE/static plans pass. Closed programs and explicit freestanding object
  links retain the tool-free path. C driver discovery is a bootstrap dependency;
  a compiler-independent hosted SDK configuration remains future work.
  The combined ABI/driver snapshot passes all four original Sieve sources with
  native objects linked by Fas/GNU and direct native driver executables, including
  static glibc. Native runs disable LLVM tools; C header imports and hosted
  discovery retain the documented C compiler bootstrap dependency.
- **Scale**: replace initial resource caps only with large-input, memory and
  overflow tests. Exercise address-space gaps and relocation boundaries, not just
  large file sizes; continue multicore scale qualification in v0.8 and large CI
  in v0.9. Current tests inspect a roughly 1 GiB NOBITS layout against GNU ld,
  plus overflow/GC/publication behavior. The full scale runner also passes a
  64 MiB file-backed payload, 200,000 definitions/relocations, 65,260 retained
  sections and 1,024 actual input files with boundary rejections. These synthetic
  inputs do not qualify real large programs or layouts beyond the current cap.

Keep C++ at its current usable interoperability level. The v0.7 GCC/Clang
corpus now passes cleanup/rethrow, partial links, exceptions across DSOs in both
directions, RTTI/dynamic_cast/typeinfo identity, virtual destruction and C++ global
initialization/shutdown against installed libstdc++/libgcc. Fas/GNU/LLD producer
and consumer matrices pass, including late-loaded exceptions, thread-local
destructors, unloading and reloading. ET_REL GNU unique definitions, static C++
and larger applications are outside the current scope.
Supporting unwind metadata alone does not make the whole C++ ecosystem supported.
No additional C++ implementation is required solely to complete the Fas/C release.

Arbitrary `SECTIONS` scripts, address assignments and kernel/bare-metal layouts
remain outside the initial hosted Linux release; they require an explicit layout
engine and dedicated target/startup tests. LLVM bitcode and GCC LTO remain
deliberately unsupported; their compiler-specific code generation/plugin machinery
is outside the user's desired scope. Ordinary native objects remain the supported
compiler boundary.

## Performance target

Proposed stretch target: **twice as fast as LLD on a predefined representative
corpus of large supported links**, with equivalent semantics and optimizations.
Use the geometric mean of per-input `LLD time / fas-ld time`, publish every case,
and track small-link latency and memory separately. This is a goal, not a result
or a promise required to label a correct supported release usable.

The current tiny synthetic corpus is insufficient to validate that target. Its
257-object case is 55.21 ms for fas-ld versus 33.89 ms for LLD. Twice as fast as
that LLD result would mean about 16.95 ms, requiring roughly a 3.3x improvement
over our current implementation on that input. Improving our own time by 2x
would be a different and less demanding target.

OCaml is not ruled out by these results. OCaml 5 provides real parallel domains;
the engineering risks are allocation/GC pressure, boxed data, memory bandwidth,
hashing, copying and synchronization. Profile before choosing packed arrays or
mapped storage, and use a persistent pool rather than spawning a domain per
file or relocation. Small links must avoid unnecessary parallel startup costs.
The v0.2 profiling pass removed eager successful-path diagnostics and added a
sorted-relocation fast path. The preserved follow-up measurements are documented
in `benchmarks/README.md`; they do not establish the large-link stretch target.

Mold's current published benchmark reports a 4.9x median speedup over LLD, with
some cases above 10x, on its specified workloads/hardware. That is not a universal
factor transferable to our machine or workload. We should include mold in local
comparisons when available. Beating mold by another 2x would be a separate,
considerably more speculative target.

## Later tracks

- Main performance pass after the supported production baseline is qualified:
  packed symbol/relocation storage, archive indexing, fewer copies/allocations
  and domain workers. Preserve behavior with the established corpus and
  differential tests. Existing SHA-1/IFUNC/read-cache changes and timing reports
  remain the starting evidence; the LLD stretch target is unachieved.

- Port the tested implementation to freestanding Fas when the language/runtime
  supports its actual needs. Keep the corpus and benchmark gates; do not assume
  that a language port automatically improves performance. This track need not
  wait for or block a particular OCaml linker version.
- Add another ELF CPU through target-specific relocation/layout rules.
- Add Windows as a PE/COFF implementation with its own compatibility tests.

## References

- [Current support matrix](README.md) and [measured baseline](benchmarks/README.md).
- [Detailed design and acceptance plan](../../../Fas%20linker%20scope%20and%20plan.md).
- [mold benchmarks](https://github.com/rui314/mold#benchmark).
- [LLD design](https://lld.llvm.org/NewLLD.html).
- [OCaml parallel programming](https://ocaml.org/manual/5.4/parallelism.html).
