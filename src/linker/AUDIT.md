# Linux x86-64 Fas/C release audit — 2026-11-05

Scope: review the current implementation, supported contract, tests, corpus,
performance evidence and release workflow. Fas and ordinary native C objects are
the release priorities. Existing C++ coverage is useful additional coverage;
keep it at the current usable level. Compiler-specific LTO is explicitly out of scope.

Result: substantial linker functionality exists, but v0.7 is still in progress.
The passing regression suite does not cover all relevant behavior. This review
reproduced two correctness bugs and two previously untracked compatibility gaps.
No implementation changes were made during the original audit. The observations
below preserve that original evidence; the subsequent repair status is recorded
separately here. The roadmap distinguishes current status from historical plans.

## Subsequent repair status — 2026-11-05

- **Synthetic dynamic exports fixed:** generated image endpoints use regular
  `.dynsym` section indexes so loader lookups include the image load bias.
  Explicit input absolute symbols remain absolute. Header/startup/loader
  internals and registry boundaries are hidden when synthesized; explicit input
  definitions retain their visibility. Regression groups cover independent
  loaders, hash styles, versions, fixed/PIE output, GC and concurrent reentry.
- **Permission-mask handling fixed:** capture the effective creation permissions
  on the temporary file, make it private before writing image bytes, and restore
  the captured permissions before rename. No global umask read/change is used.
  Regressions cover 022/027/077 masks, replacement and failure preservation,
  executables/DSOs/partial objects/maps/native objects, plus concurrent domains.
- **Legacy lifecycle gap diagnosed:** final links reject nonempty allocatable
  `.ctors`/`.dtors` and priority suffixes even under GC. Modern arrays remain
  supported; `-r` preserves legacy lists for independent final consumers.
- **C registry subset implemented:** C identifier boundaries for allocatable
  non-TLS PROGBITS/NOBITS, compatible contiguous contributions with alignment,
  hidden synthesized visibility, explicit overrides and conservative live-reference
  GC retention. Tests cover independent GNU/LLD consumers, static/fixed/PIE/shared
  images, archives, partial links, callbacks, COMDAT, merge records and empty/BSS
  sections. Boundary references do not cause archive member extraction.

Focused semantic checks are now in the regular Dune suite:
`test/synthetic_integration.py`, `test/publication_integration.py`,
`test/legacy_integration.py` and `test/registry_integration.py`. The original audit
JSON remains an immutable observation report, not the repaired binary's result.
The final full-suite run passed with musl enabled, including all 3,782 compiler
checks and the linker suites. Its log is
`/tmp/fas-linker-audit-fixes-regression-v2-20261105.log`; the earlier repair run
exposed a lifecycle-array grouping regression, corrected before the final run.
GNU CRT and Clang can have different `sh_entsize` values on lifecycle arrays;
they must still be combined as one array. Existing real-compiler lifecycle
tests caught the regression and pass on the corrected build.

The [repair verification record](audit/repair-regression-2026-11-05.json) records
the command, passing counts, linker/log hashes and source hashes. The original
39-probe audit was also replayed on the repaired binary. Modern
lifecycle and registry programs execute successfully, `_end` loader lookup
matches its internal address, restrictive masks produce 0700 executables and
0600 objects/DSOs/maps, and legacy lifecycle lists fail with explicit diagnostics.
The snapshot is `/tmp/fas-linker-audit-repaired-20261105`; the durable observation
report is [repaired results](audit/repaired-results-2026-11-05.json). These are
correctness results, with no new performance measurements.

The subsequent [sysroot regression record](audit/sysroot-regression-2026-11-05.json)
pins the explicit sysroot/search implementation and a passing full run with musl
enabled: 3,782 compiler checks, 203 Python cases across 37 groups, and the OCaml
unit/zlib gates. Eleven sysroot groups cover GNU ordering, canonical script
location, strict dependency reads, target/runtime path separation, bounded
search, output aliases and concurrent links with distinct roots. This qualifies
the declared explicit-search subset, not GNU's implicit system search policy.

`test/publication_fault_integration.py` adds four passing operational regression
groups using a test-only Linux interposer. ENOSPC, a partial write followed by
EIO, close/chmod/rename failures preserve previous artifacts and remove live
invocations' temporaries. Short writes and EINTR produce byte-identical output.
A deterministic killed-writer probe confirms the old file survives and the
orphaned partial temporary is private; subsequent invocations ignore it.
Map-first publication remains two individual transactions. The focused log is
`/tmp/fas-publication-fault-focused.log`; Dune integration also passes in
`/tmp/fas-publication-fault-dune-20261105.log` (that incremental run did not enable
the optional musl environment).

The [static PIE verification record](audit/static-pie-regression-2026-11-05.json)
pins a further passing forced full run: 3,782 compiler checks and 215 Python
cases across 39 groups, with musl enabled and no skips, plus OCaml unit/zlib
checks. Eight static PIE groups qualify the declared glibc/musl CRT subset;
publication fault tests cover this output mode too. The separate
[cJSON static PIE replay](corpus/results/2026-11-05/static-pie-cjson.json) passes
all 21 upstream programs with matching GNU output. Snapshots and commands remain
in `/tmp/fas-corpus-static-pie-20261105`. No speed or production qualification
claim follows from these local correctness results.

The [migration verification record](audit/migration-regression-2026-11-05.json)
pins the move to `src/linker`, all 87 byte-preserved compiler source moves and
37 unchanged historical JSON reports. The migrated full suite passes 3,782
compiler checks and 216 Python linker cases across 39 groups with musl and no
skips. Additional evidence covers the development build, a staged install,
four [large-input groups](scale/results/2026-11-05/README.md), static PIE fuzzing
and four upstream zlib build configurations. The new [CI definition](CI.md)
passes Actionlint validation; it has not yet run on GitHub. This remains local
correctness qualification, with no new performance claim.

## Original reproduced findings

### P1: image-relative linker symbols are exported as absolute addresses

Location: `dynamic.ml`, export selection around line 450 and symbol emission
around line 759; synthetic definitions originate in `resolver.ml`.

A PIC C shared library references `_end` and exports `get_end()`, which returns
that address. A separately GCC-linked host loads it with `RTLD_NOW|RTLD_LOCAL`.
`dlsym(handle, "_end")` must agree with `get_end()` for that library. Fas links and
loads successfully, but the addresses differ; the probe exits 3. GNU ld and LLD
both pass, exiting 0.

The internal relocation planner recognizes synthetic addresses as relative to
the image. However, the dynamic symbol emitter copies their synthetic `SHN_ABS`
classification into `.dynsym`. The loader consequently omits the library's load
bias when resolving the exported symbol. Ordinary user-defined absolute symbols
still need absolute semantics, so a fix must distinguish those cases.

Required follow-up: define binding/visibility and address classification for
each synthetic symbol; cover DSO exports, exported PIE symbols, independent
consumers, ASLR, symbol versions and genuinely absolute input symbols. The
demonstrated failure is the DSO `_end` case; the rest are regression requirements.

### P1: publication overrides the caller's permission mask

Location: `file_io.ml:73`, with requested modes supplied by `static_link.ml` and
`partial_link.ml`; the native compiler's object publication shares this helper.

With child-process `umask 077`, Fas creates executables as 0755 and partial
objects, DSOs and maps as 0644. GNU ld and LLD create executables as 0700 and
partial objects/maps as 0600; their DSOs are 0700 on this host. The relevant bug
is Fas granting group/other access despite the restrictive mask, independently
of the chosen DSO executable-bit policy.

The temporary file starts at 0600, but unconditional final `fchmod` applies the
unmasked requested mode. Required follow-up: honor creation permissions while
retaining atomic publication, failure preservation and concurrent library calls.
Do not temporarily change the process-wide umask to read it: concurrent OCaml
domains can be linking at the same time. Test masks 022/027/077, existing-output
policy, maps, native objects and concurrent invocations.

### P2: legacy `.ctors` / `.dtors` are accepted without lifecycle behavior

Location: ordinary PROGBITS acceptance in `elf_reader.ml`; lifecycle recognition
in `layout.ml` and `lifecycle.ml` handles modern arrays and init/fini fragments.

Clang `-fno-use-init-array` emits legacy constructor/destructor lists. A volatile
constructor-state probe links successfully with Fas, but returns 7 because the
constructor never runs. A destructor probe prints only `main`, omitting `done`.
Both failures occur with and without section GC. Modern-array controls pass for
all three linkers.

GNU ld passes the legacy probes. Installed LLD 22 also skips these legacy lists
with the supplied modern GCC CRT, so this is specifically a demonstrated GNU
compatibility difference, not a claim that all independent linkers support the
combination. The current Fas contract does not promise legacy lifecycle lists,
but silently accepting them is easy to mistake for working constructor support.

Required follow-up: either implement and test the legacy ordering/CRT contract,
including priority suffixes and partial links, or reject affected final links
with a targeted diagnostic. Existing array support must continue working. Legacy
support itself need not block v1 if the unsupported input is diagnosed clearly.

### P2: section registry boundary symbols are missing

Location: fixed `Resolver.predefined_names` and per-contribution layout in
`layout.ml`; GC must participate in any implementation.

A retained C `registry` section containing one record, referenced through
`__start_registry` and `__stop_registry`, fails with an undefined-symbol error.
GNU ld and LLD pass the same probe with and without GC. The fixture uses both
`used` and `retain` so LLD's different default start/stop GC policy does not
confound the comparison.

These synthesized boundaries are documented by
[GNU ld](https://sourceware.org/binutils/docs/ld/Input-Section-Example.html).
[LLD documents the GC policy differences](https://lld.llvm.org/ELF/start-stop-gc.html).
Their absence affects native C metadata/registration patterns independently of
C++ or LTO.

Required follow-up: synthesize boundaries for the declared section-name subset,
make matching contributions contiguous, specify GC retention policy, and test
multiple objects, archives, alignment, explicit overrides, visibility, partial
links and absent sections. Add this to C compatibility qualification rather than
assuming that arbitrary layout scripts are necessary for this use case.

## Coverage and remaining release work

| Area | Existing evidence | Remaining work / release decision |
| --- | --- | --- |
| Fas native toolchain | Tool-free native objects and links, separate compilation, archives, scalar C interoperability, supplied freestanding fixtures | All four real Sieve sources still use LLVM-generated objects for linker acceptance. The native backend rejects their variadic calls. Qualify native Fas applications and finish the required external ABI in the backend; this is separate from linker relocation support. |
| Hosted C | GCC/Clang drivers, fixed/PIE/shared output, CRT lifecycle, glibc and isolated musl, threads/TLS, declared executable COPY subset and full Chibicc self-hosting/driver suites | Publish supported compiler/libc/kernel baselines and run them in CI. COPY storage with overlapping aliases, protected/unique data and mixed version spellings requires PIC; arbitrary text relocations remain unsupported. |
| Static C | Static ET_EXEC glibc/musl and IFUNC startup; declared static PIE with CRT self-relocation, ASLR, lifecycle, threaded/aligned TLS and glibc IFUNC ordering | Wider real static PIE programs and libc versions remain qualification work. Static GD/LD/TLSDESC relaxation is absent and must remain explicit. |
| Archives and partial links | Regular/thin GNU archives, indexes, groups, weak/common/COMDAT, multi-stage `-r`, independent consumers | Broader build-system acceptance, sustained container/member fuzzing, and declared behavior for empty archive selections. BSD archive support is optional unless a required toolchain emits it. |
| Discovery and scripts | SONAME/DT_NEEDED, RUNPATH/RPATH, explicit sysroot paths, bounded SEARCH_DIR, strict transitive checks, common libc scripts; GNU-compatible indirect archive boundary | Wider GNU default search policy and additional required driver flags. Indirect DSO references intentionally do not extract archive members; use explicit objects/roots or direct DSO references. GCC currently needs `-fno-use-linker-plugin` even for ordinary native links. Decide whether that documented flag is sufficient for v1; ignoring compiler plugins silently is unsuitable. |
| Debugging and unwinding | DWARF 4/5 readelf/addr2line/GDB fixtures, zlib input, CFI and personality/LSDA pointers | Wider optimized/debug configurations and larger programs. Extended unwind lengths, zstd/legacy compressed debug and output compression remain explicit limits; add only when required by the chosen matrix. |
| Scale and memory | Checked arithmetic, relocation overflows, roughly 1 GiB sparse NOBITS, 64 MiB file-backed data, 200k symbols/relocations, 65,260 retained payload sections and 1,024 independent files with boundary rejections | Current caps include 1024 input files, 128 MiB per file, 256 MiB aggregate input/decoded debug, 512 MiB output and 1 GiB image. Real large programs, peak memory, further address-space boundaries and resource failures remain unqualified; limits are unchanged. |
| Speed | Warm-cache real-program/object/archive baselines, 21 samples per workload; invocation-local read reuse gives measured Chibicc gains with identical outputs | One worker per Fas link; Fas still trails LLD on the real workloads. Packed storage, archive/index costs, parallel work, cold-cache behavior and larger programs remain v0.8 work. No 2x-LLD result is established. Measurement requires a quiet-PC interval. |
| Correctness automation | Independent inspectors/loaders, GNU/LLD comparisons, malformed fixtures, short AFL campaigns, reentry and publication failure tests; correctness/weekly fuzz CI defined and validated | Execute and qualify hosted CI and longer fuzzing; broaden generated semantic differential tests, dependency graphs and deterministic queue replay. Fuzz campaigns primarily detect parser exceptions/hangs, not wrong emitted program behavior. |
| Operational reliability | Atomic individual-file publication, alias protection, bounded regular reads, creation permissions and concurrent regression coverage; test-only Linux interposition verifies ENOSPC, partial-write/EIO, close/chmod/rename failures, short writes/EINTR and a killed writer | Wider filesystem/resource qualification remains. Binary/map publication is not one transaction. A killed writer may leave a private incomplete temporary; later links ignore it. Crash durability via fsync is not currently promised; decide whether v1 needs that guarantee. |
| Distribution and API | Source-build executable, staged Makefile install as `fas-ld`, wrapped invocation-owned library, repeated/concurrent-call checks; CI installation gate defined | Hosted CI qualification, versioned CLI/library contract and durable release artifacts remain open. The Makefile installs only the executable; no installed OCaml API is promised. Most internal modules are exposed by inferred interfaces; define the supported public surface before promising API stability. |
| Future targets and port | Dedicated x86 relocation module, checked ELF structures and separate file IO | ELF/x86 constants and layout policy remain spread across modules; there is no implemented multi-format target dispatch. Establish interfaces when adding another CPU/PE-COFF, with explicit unsupported-target errors throughout. The freestanding Fas port remains a separate track; current OCaml/Unix bootstrap dependencies are intentional. |

Existing corpora have useful but bounded acceptance: Chibicc's stage 1/stage 2
compilers, both upstream driver suites and 82 C runtime comparisons pass, as do
cJSON's 21 upstream tests. The new Chibicc gate uses a checksum-pinned unmodified
snapshot and logs every actual Fas invocation. Doom
covers the missing-WAD startup path. Wolf3D covers three deterministic headless
frames, not general gameplay. Add broader C projects and real build systems;
successful link commands alone do not establish application behavior.

C++ exceptions/RTTI/lifecycle coverage stays as regression evidence for shared
ELF machinery. ET_REL GNU unique definitions, static C++ and larger C++ applications
are outside the current scope. LLVM/GCC LTO is deliberately unsupported, with no
implementation planned. Arbitrary kernel/bare-metal `SECTIONS` scripts, PE/COFF
and other CPUs remain outside initial Linux v1. These boundaries
do not prevent a production release for a clearly published Fas/C matrix.

## Evidence and reproduction

The subsequent read-cache checkpoint passes 229 Python cases across 40 groups,
OCaml checks and 3,782 compiler checks with musl enabled. Chibicc's full self-host
gate passes again. The user supplied a quiet-PC interval for 21-sample real-link
and synthetic object/archive comparisons. Reusing repeated positional input bytes
reduces the two dynamic Chibicc medians by 13%/24%, with identical output bytes;
Fas still trails LLD. See [performance evidence](benchmarks/README.md) and
[`audit/read-cache-regression-2026-11-05.json`](audit/read-cache-regression-2026-11-05.json)
for source/tool/input/output hashes and limitations. Large distinct DSO sets,
cold caches and worker scaling remain unqualified.

Continued C compatibility work is recorded in
[`audit/copy-driver-regression-2026-11-05.json`](audit/copy-driver-regression-2026-11-05.json).
The new release gate passes 228 Python cases across 40 groups, OCaml checks and
3,782 compiler checks with musl enabled. Checksum-pinned unmodified Chibicc
self-hosts both stages and passes 126 driver checks plus 82 GNU runtime
comparisons. Two fresh 180-second COPY-planning/shared-parser campaigns complete
237,413 executions without saved crashes/hangs. These short campaigns do not
replace sustained fuzzing. The record pins the final linker, sources and logs;
no performance comparison or hosted Ubuntu run is claimed.

The previous full root release suite passed, including 3,782 compiler checks and
the linker suites, with musl enabled. Its log is
`/tmp/fas-static-planning-final-regression-20261105.log`. This was existing evidence,
not a full-suite rerun during the audit. The new probes demonstrate gaps in that
suite; they do not invalidate its recorded passing cases.

Real-program evidence remains in
[`corpus/results/2026-11-05`](corpus/results/2026-11-05/README.md): 71 dynamic and 69
static recorded rows, including aggregate rows; 67 final static links were
subsequently byte-identical to the executed outputs. Short fuzzing reports remain
under `fuzz/results/2026-11-05`; the last two final-source modes recorded 219,322
executions with no saved crashes/hangs. Those runs are not sustained release
qualification or semantic proof.

The additional audit ran **39 tool/probe combinations**, with no timing claims.
The snapshot, source/object inputs, executables and full report are under
`/tmp/fas-linker-release-audit-verified-20261105`. A durable observation report is
[`audit/results-2026-11-05.json`](audit/results-2026-11-05.json). The audited Fas
binary SHA-256 is
`3a473fa4df88743c9def94cd89285b13fd1cb12ca1af95aaefa7239d0f68c7a9`.
GNU ld is 2.46 and LLD is 22.1.8. Reproduce from `fas` with a new directory:

```sh
python3 src/linker/audit/run.py _build/default/src/linker/bin/main.exe \
  --output /tmp/fas-linker-audit-new
```

The runner's zero exit status means observations completed. It deliberately
records known failing behavior and is not registered as a passing Dune test.
When findings are fixed, move their semantic assertions into the regular suite.
It snapshots the selected Fas executable, records tools/commands/input hashes,
uses only generated benign C/assembly fixtures, and changes umask only inside
the relevant child processes.

## Work order after this review

1. Completed implementation: synthetic exports, permission masks, legacy lifecycle
   diagnostics and the declared registry boundary/GC subset, with regressions.
2. Qualify required native Fas programs and freeze the Fas/C compiler/libc/input
   matrix, including the static PIE and sysroot scope decisions.
3. Complete representative scale/memory work and measure performance on an idle
   machine; then implement and qualify parallelism.
4. Add sustained CI/fuzzing, semantic differential tests, failure injection,
   packaging and release artifacts. Keep additional C++ expansion and LTO out of
   these release gates.
