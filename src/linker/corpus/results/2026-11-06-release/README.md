# Operational failure and release archive checkpoint — 2026-11-06

The complete shared correctness recipe passes again in fresh Ubuntu 24.04.5
userspace using the pinned toolchain image from the [previous checkpoint](../2026-11-06-clean/README.md).
This candidate adds input-read/resource failure coverage and qualified local
binary/source archives. It remains `fas-ld 0.7-dev`; no production version or
hosted execution is claimed.

| Selection | Observed result |
| --- | --- |
| Workstation full root gate | 3,792 compiler checks; 259 Python linker cases in 43 groups; no skips |
| Fresh Ubuntu regressions | All 259 Python cases, OCaml unit/zlib checks; no skips |
| Scale and glibc/musl libraries | Four full scale groups; zlib dynamic/static/static-PIE; GCC 13/Clang 18 musl matrix, 48 final links and 96 gzip cross-consumer executions |
| Chibicc and SQLite | Two self-host stages, 126 driver checks, 82 GNU runtime comparisons; 40 SQLite API executions, four native Fas consumers and `main.test`, zero errors |
| Original native Fas Sieve sources | All four unchanged sources, including `primes_zero3.fas`, pass dynamic/static glibc: 24 executions across Fas/GNU/native-driver consumers |
| Release artifacts | Both archives verify; rebuilding the source archive in another directory succeeds; delivered and rebuilt linkers link/run the fixture |
| Reproducibility and rejection controls | Independently repackaged real artifacts are byte-identical; failed/mismatched qualifications, changed source and version relabeling are rejected; selection guard rejects wrong case count and reported skip |

The new failure tests inject short/interrupted reads, EIO and premature EOF,
persistent temporary-creation EMFILE/ENOSPC/EACCES and an EEXIST name collision.
They also impose a real zero-byte file-size limit. Existing artifacts survive
failures across final, partial, shared and static-PIE output. Repeated input
failures recover in the same library process: 256 failures per error mode under
a 32-descriptor limit, unchanged descriptor counts and a subsequent successful
link. The only production-code repair here adds the missing input pathname to
read-error diagnostics. Fault controls remain exclusively in the test interposer.

The [packaging guide](../../../release/README.md) explains the archive format and
qualification requirements. The binary contains the project MIT notice, embedded
OCaml runtime license/linking exception, help and build provenance. Runtime
interpreter, shared dependencies and GLIBC symbol requirements are measured from
the ELF. This particular binary requires symbols through `GLIBC_2.38`; it was
executed on Ubuntu glibc 2.39, not independently qualified on glibc 2.38.
Checksums/inventories provide integrity checking, not signatures.

The Ubuntu CLI SHA-256 is
`841be67d2056ed00eb287abdcf73a6277c20bc61789e89788222b85fb86635d6`.
The source rebuild happens to produce the same CLI bytes in this environment;
this does not establish compiler reproducibility across arbitrary toolchains.
The [source manifest](source-manifest.json) identifies all 1,000 selected source
files and executable modes. The archives contain that tested snapshot, including
uncommitted changes; this evidence directory and later documentation updates are
not retroactively inserted into it.

Evidence: [runner report](clean-run.json), [selection summary](qualification-summary.json),
[corpus reports](corpus-reports.json), [build information](build-info.json),
[archive rebuild/run](release-smoke.json), [real controls](release-controls.json),
[workstation regressions](host-regressions.json), [Sieve results](sieve.json) and
[artifact integrity](integrity.json). The first host invocation supplied a
container-only musl sysroot by mistake; its failed setup is preserved separately.
The corrected host-built sysroot passes. Earlier broad-corpus and fuzz results
retain their original binary identities and are not relabeled as this candidate.

Full logs, inputs, binaries and downloadable archives are retained at:

```text
/home/glonex/.cache/fas-linker-qualification/ubuntu24-release-20261106
  results/linker-release/SHA256SUMS
  results/linker-release/fas-ld-0.7-dev-linux-x86_64.tar.gz
  results/linker-release/fas-ld-0.7-dev-linux-x86_64-source.tar.gz
```

Hosted CI, sustained fuzzing, wider compiler/libc/build-system qualification and
the final compatibility freeze remain open. Publication/signing is not performed.
Performance remains deferred. This checkpoint closes the local archive-tooling
gap; it does not complete the production release.
