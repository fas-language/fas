# 2026-11-05 local compatibility results

Correctness only; the desktop was in use. Source and toolchain input hashes are
preserved in the accompanying JSON. Reproduction commands are in `../../README.md`.

| Corpus | Result | Limits |
| --- | --- | --- |
| Chibicc | GCC-built compiler linked with fas-ld; all 41 C test programs in the snapshot compile, link, run and match GNU ld, dynamically and with static glibc | Stage 2 and compiler-driver tests unverified |
| Fas Sieve | All four original sources pass their prime-count check with LLVM/native objects linked by Fas/GNU; native automatic hosted driver also passes, dynamically and with static glibc | Native runs disable `opt`/`llc`; header imports and hosted discovery still require a C compiler |
| Doom 1.10 reference | 62 C objects link fixed/PIE; startup matches GNU ld; headless 32-frame rendering hash `52e20f0b442bf948` matches across all four links | Instrumented device hooks, local Doom shareware IWAD; no general gameplay verification |
| Minimal Wolf3D reference | Links; instrumented copy renders three E1M1 frames headlessly and matches GNU ld framebuffer hash `38d3d4bed65dfa55` | Deterministic bounded harness, not general gameplay |
| cJSON v1.7.19 | All 21 upstream core/utility test programs pass and match GNU ld | GCC PIC, PIE, installed glibc |
| musl 1.2.5 isolated sysroot | Static LE/IE, fixed/PIE LE/IE/GD/LD/TLSDESC, lifecycle/thread/errno checks; late dlopen TLS | No static GD/LD/TLSDESC relaxation; glibc is tested separately |
| Installed static glibc | Chibicc and its 41 tests, four original Fas Sieve programs and all 21 cJSON tests pass; C programs match GNU ld | ET_EXEC; no static PIE or static game dependency closure |

The JSON captures observed Sieve pass counts/elapsed time only as program output.
Those values are not linker or language performance results.

`native-hosted.json` pins the combined ABI/driver snapshot at `5fd063f`: all
four sources pass 20 executions (eight LLVM object consumers, eight native
object consumers and four direct native driver builds). The same run repeats
Doom startup and Wolf3D rendering acceptance. `native-static.json` adds 12
native static glibc executions. Selected backend failures now fail the corpus;
they are no longer recorded as optional limitations. `doom-render.json` adds
32-frame E1M1 skill-3 rendering with identical Fas/GNU fixed/PIE hashes. The
first development attempt used unsupported `-iwad`; correcting the harness to
use Doom 1.10's `DOOMWADDIR` discovery resolved it without linker changes.
The reports preserve separate harness hashes and snapshots. Full logs are in
`/tmp/fas-native-projects-20261105`, `/tmp/fas-native-sieve-static-20261105` and
`/tmp/fas-doom-render-v2-20261105`. Assets and binaries are not checked in.

Full local snapshots, binaries and command logs:

- `/tmp/fas-corpus-replay-20261105`
- `/tmp/fas-corpus-cjson-20261105`
- `/tmp/fas-musl-20261105` (build log, source archive hash and isolated sysroot)
- `/tmp/fas-musl-acceptance.log` (musl tests)

The musl and cJSON releases are pinned corpus inputs, not claims about the latest
available releases. No external project's source tree was modified.

The `discovery-replay.json` rerun contains 71 passing recorded cases across all
five project groups after partial linking, thin archives and discovery work.
The strict-dependency replay is preserved separately: Chibicc/Sieve/cJSON/Doom
pass, while the original Wolf3D attempt records a GNU unique DSO export rejection
in indirect libstdc++. The focused rerun after DSO input support passes and
retains the same three-frame hash; both the failure and fix remain reviewable.
This does not qualify general C++ source/object interoperability.

The latest `unwind-strict-replay.json` contains all 71 passing recorded cases
after personality/LSDA, SDT and repeated-property work. The linker binary is
snapshotted at `/tmp/fas-corpus-final-static-unwind-20261105/tools/fas-ld`.
`static-glibc-replay.json` contains 69 passing recorded cases after the unused-IFUNC
resolver fix; its snapshot and full commands are in
`/tmp/fas-corpus-static-glibc-final-20261105`. Counts include per-project aggregate
rows, so they are not counts of distinct upstream programs. Earlier prose stated
40 Chibicc tests; the manifests and runner enumerate 41.

`static-planning-replay.json` records 67 subsequent Fas links with the final
IFUNC planning fast path. Every output is byte-identical to its previously run
static corpus executable. The final binary, outputs and report remain under
`/tmp/fas-static-planning-replay-20261105`.

The migrated layout adds checksum-pinned zlib 1.3.1 upstream configure/make
qualification. `zlib-{dynamic,strict,static,static-pie}.json` record four passing
configurations, 32 actual Fas linker invocations and 82 gzip cross-consumer runs.
Upstream normal/shared/large-file tests pass where applicable; GNU-linked
examples and gzip programs consume the same upstream objects and archives.
Dynamic modes also exercise a GNU-linked consumer of the Fas-built versioned
DSO. These are correctness results, not performance comparisons. Each report
pins its harness, linker snapshot, source inputs and complete local command log.
