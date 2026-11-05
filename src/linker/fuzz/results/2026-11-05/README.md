# 2026-11-05 correctness campaigns

Each target ran for 180 seconds, sequentially with one worker. The desktop was
in use; these are correctness results and provide no speed comparison. JSON
reports retain target hashes, source revision/status, commands and AFL statistics.

| Campaign | Target | Executions | Bitmap edges | Corpus | Saved crashes | Saved hangs |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| parsers-and-static | elf | 132782 | 676 | 263 | 0 | 0 |
| parsers-and-static | shared | 124572 | 576 | 222 | 0 | 0 |
| parsers-and-static | archive | 128283 | 807 | 356 | 0 | 0 |
| parsers-and-static | script | 128075 | 237 | 157 | 0 | 0 |
| parsers-and-static | link | 126735 | 914 | 194 | 0 | 0 |
| versions | version | 123622 | 867 | 169 | 0 | 0 |
| shared-before-fix | shared-link | 110354 | 2136 | 453 | 2 | 2 |
| shared-after-fix | shared-link | 111429 | 2148 | 485 | 0 | 0 |
| tlsdesc-ifunc | shared-link | 113163 | 2363 | 482 | 0 | 0 |
| symver | elf | 144419 | 708 | 283 | 0 | 0 |
| symver | archive | 140528 | 835 | 344 | 0 | 0 |
| symver | symver-link | 127759 | 2592 | 495 | 0 | 0 |

The four shared-before-fix saved cases (two crashes and two hangs) replay as uncaught `Invalid_argument("option is None")`
exceptions from the same PLT planning bug. Relocations targeting nonallocatable
metadata could request a PLT stub that was never created. Metadata now uses a
resolved value/tombstone when no runtime stub exists; local symbols cannot select
a global stub by name. All four saved inputs replay successfully after the fix (see `replays.json`), and deterministic
regressions are included in `test/shared_output_integration.py`. The fresh
shared-after-fix campaign saved no findings.

Full source snapshots, instrumented binaries, seeds, queues and findings remain
in these local directories (temporary storage, not durable repository artifacts):

- `/tmp/fas-fuzz-20261105-0727`
- `/tmp/fas-fuzz-versions-20261105`
- `/tmp/fas-fuzz-shared-link-20261105`
- `/tmp/fas-fuzz-shared-link-fixed-20261105`
- `/tmp/fas-fuzz-tlsdesc-ifunc-20261105`
- `/tmp/fas-fuzz-symver-20261105`

Bitmap edges are AFL instrumentation observations, not source coverage. Short
campaigns do not establish production readiness. The host uses an external core
collector; signal findings can be delayed or classified as hangs. Both counters
were checked; uncaught OCaml exceptions are recorded through exit status 2.

zlib: 112310 executions, 230 bitmap edges, 118 corpus entries, 0 saved crashes, 0 saved hangs.
elf: 120616 executions, 898 bitmap edges, 365 corpus entries, 0 saved crashes, 0 saved hangs.
link: 115836 executions, 1596 bitmap edges, 404 corpus entries, 0 saved crashes, 0 saved hangs.

These latest targets include compressed debug and static TLS seeds. Their snapshot
precedes the aggregate decoded-storage budget, which is included in the next
snapshot. Local queues: `/tmp/fas-fuzz-zlib-static-20261105`.

Additional v0.7 campaigns, each 180 seconds per target:
| Campaign | Target | Executions | Bitmap edges | Corpus | Saved crashes | Saved hangs |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| metadata-thin | metadata-link | 112441 | 2748 | 443 | 0 | 0 |
| metadata-thin | archive | 116078 | 909 | 363 | 0 | 0 |
| metadata-thin | shared | 112996 | 550 | 213 | 0 | 0 |
| partial | partial-link | 104863 | 1335 | 440 | 0 | 0 |
| partial | elf | 111238 | 855 | 291 | 0 | 0 |

The metadata-thin snapshot includes driver policies, GNU hash/build IDs and thin
containers. The partial snapshot also includes ET_REL combination and GNU/LLD
partial-link metadata compatibility fixes. Full snapshots and queues remain in
`/tmp/fas-fuzz-metadata-thin-20261105` and `/tmp/fas-fuzz-partial-20261105`.
These short runs do not qualify the implementation for production.

DSO requirement parser/validation campaigns (180 seconds per target):

| Target | Executions | Bitmap edges | Corpus | Saved crashes | Saved hangs |
| --- | ---: | ---: | ---: | ---: | ---: |
| shared | 110291 | 648 | 214 | 0 | 0 |
| dependency | 122429 | 718 | 231 | 0 | 0 |

The dependency target validates one mutated DSO as an in-memory graph, including
requirements, provider nodes and references. It does not open dependency paths.
The snapshot includes strict discovery and version parsing; it precedes the
later explicit LTO diagnostics. Full local artifacts:
`/tmp/fas-fuzz-dependencies-20261105`.

GNU unique input follow-up (180 seconds per target):

| Target | Executions | Bitmap edges | Corpus | Saved crashes | Saved hangs |
| --- | ---: | ---: | ---: | ---: | ---: |
| shared | 110076 | 652 | 210 | 0 | 0 |
| dependency | 110233 | 695 | 213 | 0 | 0 |

This snapshot adds GNU unique DSO seeds and explicit LTO diagnostics. It precedes
the later dependency token/path identity fixes, which have deterministic loader
tests. Local artifacts: `/tmp/fas-fuzz-unique-dependencies-20261105`.

Unwind/static IFUNC/SDT/version-layout campaign (180 seconds per target):

| Target | Executions | Bitmap edges | Corpus | Saved crashes | Saved hangs |
| --- | ---: | ---: | ---: | ---: | ---: |
| elf | 105379 | 993 | 375 | 0 | 0 |
| link | 121266 | 2073 | 430 | 0 | 0 |
| shared-link | 109517 | 2687 | 351 | 0 | 0 |
| partial-link | 108252 | 1416 | 430 | 0 | 0 |
| shared | 110128 | 716 | 244 | 0 | 0 |
| dependency | 113730 | 751 | 252 | 0 | 0 |

All 668,272 executions completed without saved findings. The snapshot includes
personality/LSDA, static IFUNC, SystemTap/repeated-property seeds and multi-provider
GNU/LLD version requirements. It precedes the metadata section-type checks and
unused-IFUNC selection fix found by review and runtime differential tests.
The fuzzer does not execute output programs, so zero saved crashes cannot
establish resolver startup semantics. Full artifacts:
`/tmp/fas-fuzz-unwind-static-20261105`; report: `unwind-static.json`.

Final follow-up after metadata type validation, unused-resolver selection and the
ordinary static-link scan fast path (180 seconds per target):

| Target | Executions | Bitmap edges | Corpus | Saved crashes | Saved hangs |
| --- | ---: | ---: | ---: | ---: | ---: |
| elf | 107751 | 1013 | 394 | 0 | 0 |
| link | 111571 | 2177 | 522 | 0 | 0 |

This fresh binary imported 805 queue inputs from the preceding ELF/static-link
campaigns, with SHA-256 provenance in `unwind-static-final.json`. Its snapshot
matches the final OCaml implementation. Both campaigns total 887,594 executions
with no saved crashes or hangs. Final local artifacts:
`/tmp/fas-fuzz-unwind-static-final-20261105`. The usual short-campaign limits apply.

After migration, `static-pie.json` records a fresh 180-second static PIE emission
campaign: 136,902 executions, 2,736 bitmap edges, 590 queued inputs and no saved
crashes or hangs. It imports the interrupted pre-pause campaign's queue and pins
the instrumented target, source and seed provenance. Full artifacts remain at
`/tmp/fas-static-pie-fuzz-migrated-20261105`. The target exercises parsing,
resolution, GC, GOTPCRELX and IFUNC emission; it does not execute output programs
or qualify CRT startup. Runtime regressions supply separate semantic evidence.
