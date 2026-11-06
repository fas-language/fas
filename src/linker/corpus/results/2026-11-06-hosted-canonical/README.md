# Hosted canonical-address qualification — 2026-11-06

The repaired implementation at `4b3f876f7e51d5f1bdc6128b2c780df1de1e1f62`
passes complete hosted correctness and official GCC C execution selections.
This is a **partial observation**, taken at 19:12 UTC: four popular-C jobs have
also passed, while curl, libpng and the full fuzz selection remain running.
It does not establish a production release or complete the release-candidate
freeze. The [previous checkpoint](../2026-11-06-canonical/README.md) retains
the original five GCC failures and the repair; those failures remain failures.

| Selection | Result at this observation |
| --- | --- |
| [Hosted correctness](https://github.com/fas-language/fas/actions/runs/42272940442) | Passed: 272 Python cases in 45 groups, no skips; all eight required scale/corpus/semantic reports |
| [Official GCC torture](https://github.com/fas-language/fas/actions/runs/42272937898) | Fas 4,974/4,974 executions; GNU 4,974/4,974 on identical retained inputs; matching selections, no failures |
| [Popular C](https://github.com/fas-language/fas/actions/runs/42272944421) | Lua 5.1.5, bzip2 1.0.8, libarchive 3.8.11 and Redis 7.2.8 jobs passed; curl and libpng pending |
| [Full fuzz selection](https://github.com/fas-language/fas/actions/runs/42272946903) | Running; no duration, coverage or findings conclusion yet |
| Delivered archives | Hosted source rebuild and both linked-program checks passed; downloaded archive integrity independently verified |
| Extra musl follow-up | 20 ordinary-function address/preload executions and 12 saved-GCC-object executions passed |
| Original game projects | Doom fixed/PIE startup and 32-frame rendering passed; Wolf3D three-frame rendering passed; GNU comparisons matched |

## Hosted correctness and GCC

Correctness runs on Ubuntu 24.04.5, Linux `7.0.0-1017-azure`, OCaml 5.4.1,
Dune 3.24.2, GCC 13.3, Clang 18.1.3, binutils 2.42 and glibc 2.39. The
installed, delivered and source-rebuilt linker hashes all match:
`523660c182b87f80165b32b350069ede9b9676cd5ff24419474c49ac4a847665`.

The four scale groups and 384 generated-program executions pass, including
32 partial stages. Glibc zlib dynamic/static/static-PIE, GCC/Clang musl zlib and
Chibicc self-host/driver checks pass. SQLite 3.54.1 passes 40 C API executions,
four native Fas consumers and all 1,300 isolated extraquick scripts per linker:
398,243 Fas / 398,245 GNU assertions, zero errors. The only assertion-count
difference is the randomized FTS5 big-token script, 299 versus 301.

The separate GCC job uses the pinned GCC 15.2.0 container and official
`execute.exp` selection at `-O0`, `-O2` and `-Os`. Each linker reports 9,948
DejaGnu PASS entries, including 4,974 execution passes, and 57 UNSUPPORTED
entries. There are 4,974 primary links per linker with identical retained object
keys and matching execution-label hashes. LTO and executable-stack trampolines
are outside this declared selection. This is not the full GCC test suite.
Its separately built Fas CLI hash is
`0077b4ce461ee8110134f500435364dca452c5ca35b28ddb55052a514a0cd757`.

## Completed popular-C jobs

All four completed project artifacts contain the same Ubuntu Fas CLI hash as
correctness. Upstream selections and same-input GNU replay pass. Bzip2 and
libarchive also pass both shared-library producer/consumer directions.
Libarchive's independent GNU configuration control has no checked numeric or
boolean feature differences. The full upstream libarchive phase retains its
optional-feature skips; it is separate from the strict bounded archive matrix.

Lua and libarchive pass regular/thin archives and both mixed Fas/GNU partial-link
orders, with Fas/GNU/LLD final consumers: 36 final images, 276 runtime checks and
473,820 libarchive assertions. The bounded libarchive matrix reports no failed
tests/assertions or skips. Lua's `readonly.lua` deliberately exits 1 with its
expected diagnostic; the harness checks this explicitly. Lua uses included
examples rather than a separately distributed complete upstream test suite.
Redis covers the declared 14 Tcl units, not its cluster/TLS/large-memory suites.

## Downloaded archives and local follow-ups

All six available job ZIPs were downloaded and independently rehashed against
GitHub's artifact digest. ZIP CRC checks pass, member names are unique, and every
retained JSON report agrees with its actual ZIP member. `artifact-validation.json`
records this second verification and actual retained linker hashes.

| Hosted development archive | SHA-256 |
| --- | --- |
| Binary | `5f3bffb9bf1c10c0326ea6b34efada123bb35bda61a044d59f2b801789196147` |
| Source | `1319761c6046b33d14a3c2caa847ee6acf3fbe59930e425ed8ba5f251909d253` |

The downloaded source archive's 1,108 source files match the published commit's
Git blob hashes and executable modes exactly, with no extra source files. Its
manifest SHA-256 is
`2614c37822006e1eed22873ba15032cf37642a9d92848f3cad6bf6625242c3ff`.
The delivered binary independently links and runs the object/data/BSS fixture
with `PATH=/nonexistent`. Hosted source rebuilding already passed; the download
follow-up does not claim a second local source rebuild.

Workstation follow-ups use Fas CLI
`ccdf827ca1323c6149037bd46d41ee38653ef66e95f220ed52cea6e4ab654182`.
The isolated musl 1.2.5 checks cover ordinary functions and preload, including
mixed explicit/default imports and the six exact GCC objects behind the earlier
failure. They do not qualify the complete GCC suite on musl or IFUNC on musl.

The original Doom source passes fixed/PIE missing-WAD startup and deterministic
32-frame E1M1 rendering with framebuffer hash `52e20f0b442bf948`. Wolf3D passes
three deterministic E1M1 frames with hash `38d3d4bed65dfa55`. GNU comparisons
match. Instrumentation changes only snapshots and device/frame-control hooks;
these are bounded headless probes, not general gameplay acceptance. The initial
combined invocation supplied a nonexistent Wolf3D asset directory; its failure
is preserved in `games-initial.json`. `wolf3d-corrected.json` records the passing
replay with the actual original asset directory.

## Retention and remaining gates

`latest.json` and `assessment.json` freeze this partial hosted observation.
The six `*-reports.json` files preserve completed-job reports, including exact
test selections. Source identity, qualification, archive rebuilding, downloaded
archive execution and musl/game follow-ups have separate records. Copied helper
scripts have a `.txt` suffix and preserve the exact cache/repository paths used;
they are evidence, not installed tools. `integrity.json` hashes every other file
in this directory. Complete ZIPs and read-only monitors remain under
`/home/glonex/.cache/fas-linker-qualification/hosted-canonical-20261106`.

Later outcomes must be recorded as a new observation. The strict full fuzz gate
requires all fourteen targets at 600 seconds each, actual execution/edge metrics,
the expected clean source commit and no untriaged saved findings. Earlier short
campaigns cannot satisfy it. Earlier saved timeouts retain their existing
watchlist records; successful replays do not explain their unknown causes.
Final contract freeze and durable release publication remain open. Version
stays `0.7-dev`; no performance campaign, additional C++ scope or LTO is added.
