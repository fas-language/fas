# fas-ld 1.0.0 final acceptance — 2026-11-07

**Complete and published for the frozen Linux x86-64 ELF contract.**
[Download the release](https://github.com/fas-language/fas/releases/tag/fas-ld-v1.0.0). Performance optimization is next;
no speed advantage over LLD is claimed.

The immutable release source/tag is `3cf2962665405578b7a32a35f25e810e2ff188d5` / `fas-ld-v1.0.0`.
This documentation checkpoint follows that source commit; the archives/tag stay
on the tested source. The source archive retains its original candidate wording.
This record and the release notes report completed final acceptance. Linker
versions are independent of Fas language versions; the language latest remains
`v0.6.3`.

## Completed gates

- [Complete hosted matrix](https://github.com/fas-language/fas/actions/runs/42373970629): all nine selected jobs pass on the
  exact frozen source. Correctness includes **273 Python cases in 45 groups,
  no skips**, all eight required corpus/scale/generated-semantic reports,
  SQLite extraquick, isolated musl/zlib matrices, Chibicc self-hosting/driver
  tests and staged installation.
- Official GCC 15.2 C selection: **4,974/4,974 Fas and 4,974/4,974 GNU executions**,
  identical retained objects and matching execution labels. LTO and
  executable-stack trampolines remain outside this selection.
- Lua 5.1.5, bzip2 1.0.8, libpng 1.6.47, libarchive 3.8.11, curl 8.14.1 and Redis
  7.2.8 pass their declared upstream suites and 355 same-input GNU replay outputs,
  including both shared-library directions for four corpora and real thin/mixed
  partial archive pipelines. These are bounded suites, not all configurations.
- Fourteen hosted AFL targets pass **600 seconds each**: **5,576,675 executions**,
  no saved crashes or hangs. Exact target/source identities, durations and
  instrumented edge statistics are retained, not inferred source-coverage rates.
- Full local repository suite: **3,792 compiler checks and 273 linker cases in
  45 groups**, with no linker regression skips.
- All four original native Fas Sieve sources, including `primes_zero3.fas`, pass
  **24 dynamic/static executions** through Fas, GNU and the native driver with
  LLVM tools disabled. Bounded Doom/Wolf3D rendering matches GNU. User game
  sources/assets are not redistributed in these artifacts.
- The independent MIR backend `984de64` passes **124 executions** against the
  delivered linker and GNU on identical objects, with DWARF 5 verification,
  TLS, hosted PIE/EXEC and native RELA static-PIE startup. A test-only object
  export hook exposes existing startup generation; neither working branch is
  changed. The original harness incorrectly expected zero from `sum_loop.fir`,
  which explicitly declares exit 1; the unchanged GNU control confirms exit 1.
  Corrected expectations pass without producer or linker changes. Both records
  are retained, with raw commands, producer snapshot and hook in release evidence.
- Delivered source matches all **1,174 Git blobs and executable modes**.
  Hosted source rebuilding and delivered/rebuilt fixture execution pass. The
  independently downloaded binary links/runs with no tools on PATH. Published
  archives are downloaded again and their package integrity is verified.

## Final one-hour campaigns

| Target | Measured seconds | Executions | Saved crashes / hangs |
| --- | ---: | ---: | --- |
| archive | 3600.351 | 3,112,335 | 0 / 0 |
| shared-link | 3601.213 | 3,106,700 | 0 / 0 |
| copy-link | 3600.523 | 3,103,415 | 0 / 0 |

Total: **9,322,450 executions** on the exact frozen source. The archive campaign
imports its historical timeout input; the shared-output campaign imports both
its historical timeout and repaired descriptor-crash input. All three exact
inputs pass **1,000 direct replays each**, with expected normal CLI outcomes.
Historical timeout causes remain unknown and their original failed runs remain
failed. The descriptor crash has a reproduced and regression-covered cause.

## Findings and unsuccessful history

Final fuzzing of source `6633f26` saved a reproducible local TLS descriptor crash.
A non-loaded metadata relocation could request a descriptor after section GC
removed its hidden/local TLS definition. Descriptor planning now checks the
canonical definition and retained section before creating its TLS dynsym.
Permanent tests cover local/hidden/internal definitions, metadata compatibility,
checked GC rejection, output preservation and DTPOFF debug tombstones. See the
[repair record](../../../fuzz/results/2026-11-07-tls-nonalloc/README.md).
The failed hour, input and before/after replay records are retained. An initial
broader guard on `4778b2e` failed the existing metadata regression; that source's
hosted run and interrupted local campaigns remain unsuccessful. All complete
acceptance above runs on the narrower repaired source `3cf2962665405578b7a32a35f25e810e2ff188d5`.

Earlier seed setup used filenames outside AFL's `id:` convention and imported
zero seeds; those remain normal-corpus passes. A later system suspend interrupted
corrected attempts; their duration verifiers correctly rejected them. The final
campaigns above pass a full measured hour with verified seed hashes and temporary
sleep inhibitors. Original inputs, source snapshots, logs, queues and outcomes
remain in the durable evidence archive.

Earlier libarchive qualification hit three `t <= now` timestamp assertions. The
same unmodified test independently reproduces those assertions with GNU-linked
images (three retained failures); archive timestamps match actual files. That
unchanged complete job passes on rerun, with no upstream-test change. Both its
original failed ZIP and the new final-source acceptance artifacts are published.
This timestamp flake is separate from the unknown historical fuzz timeouts.

## Delivery and scope

`publication-verification.json` records all 17 server asset digests/sizes, the
exact tag target, fresh downloads and unchanged language latest tag.
`publication-final-state.json` additionally records the current public tag URLs
(the earlier metadata retained draft URLs), matching all 17 digests and direct
unauthenticated downloads of both checksum files. The release
retains nine accepted hosted ZIPs, the earlier failed libarchive ZIP, full raw
history and checksums independently of Actions artifact expiry. The evidence
archive also contains the superseded guard's failed correctness ZIP.
`SHA256SUMS` covers binary/source archives, `EVIDENCE_SHA256SUMS` covers supplemental
assets, and `integrity.json` covers this documentation record. These are integrity
records, not signatures or a general compiler-build reproducibility claim.

The original publication helper attempted to fetch an unpublished draft by tag,
which returns HTTP 404. The existing draft was selected through its numeric
release ID, its target/body and all 17 server asset digests were verified, then
publication and fresh-download verification completed. The original helper
remains in the raw evidence; this record retains the corrected publication
script and initial-attempt outcome. No compiler/linker source changed for this
delivery correction.

The native executable embeds the OCaml runtime and needs no OCaml installation
or external linker. Its actual recorded system dependencies require symbols
through **GLIBC_2.38**; system libraries are not bundled. Supported output libc
is a separate boundary. [SUPPORT.md](../../../SUPPORT.md) freezes the complete
contract, resource limits, CLI/API boundaries and individual-file atomic output
semantics. LTO, arbitrary GNU scripts, Windows, other CPUs and general static TLS
relaxation are outside this release. Internal OCaml modules are private/source
coupled, not an installed stable package. Existing C++ interoperability is retained
without scope expansion. Continuing maintenance and broader configurations are
future work, not unfinished acceptance gates for this release.
