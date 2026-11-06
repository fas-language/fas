# First hosted C corpus checkpoint — 2026-11-06

The qualification branch is published in `fas-language/fas`. All six popular-C
jobs in [the first full hosted run](https://github.com/fas-language/fas/actions/runs/42240863721)
pass on GitHub's Ubuntu 24.04 runners at source
`7e81760b79fd228f221d5f83b8bfe1c3011f57d9`. This is a **partial checkpoint**:
the original run also has two setup failures. A subsequent corrected correctness
run passes; GCC and fuzzing remain in progress. Neither the original full run
nor the release is fully qualified.

| Hosted selection | Observed result |
| --- | --- |
| Lua 5.1.5 | Included examples and bytecode compile/run pass with Fas and GNU |
| bzip2 1.0.8 | Sample checks and shared/static round trips pass with both linkers |
| libpng 1.6.47 | All 33 CTest entries pass with both linkers |
| libarchive 3.8.11 | Selected upstream CTest runs pass with both linkers; skips remain in the logs |
| curl 8.14.1 | Each linker reports 1,553 tests OK and 286 skips from 1,839 considered |
| Redis 7.2.8 | The selected 14 upstream Tcl units pass with both linkers |
| Same-input GNU controls | 355 retained Fas outputs are relinked successfully and the declared runtime selections pass |
| Shared-library boundaries | Both producer/consumer directions pass for bzip2, libpng, libarchive and curl |
| Independent GNU configure controls | No checked numeric/boolean feature differences for libpng, libarchive or curl |
| Lua/libarchive archive pipelines | 36 final links, 276 runtime checks, 473,820 libarchive assertions; zero failed assertions or skips in this bounded archive matrix |
| Artifact validation | All six downloaded archives match GitHub's SHA-256 metadata and pass ZIP CRC validation |

Every retained linker copy in these six artifacts has SHA-256
`cd2726a3049a309b10e03b5f8b487c81a18384b52beecfd2167f8370b4c9a830`.
This identifies the hosted build, separately from the workstation and Docker
builds in earlier checkpoints. The GNU control uses the same native inputs;
these are runtime compatibility results, with no performance comparison.

The archive matrix covers regular archives, thin archives, Fas-to-GNU partial
stages and GNU-to-Fas partial stages with three final consumers. Its strict
assertion/skip counts apply to the selected eight libarchive cases, rather than
to the complete upstream CTest phase. Lua uses its included examples, curl
retains upstream optional-feature/server skips, and Redis excludes its broader
cluster/TLS/large-memory selections. The project snapshots and full command logs
remain in the downloaded archives; individual JSON reports retain their precise
inputs and selections.

## Corrected hosted correctness run

[Run 42255531115](https://github.com/fas-language/fas/actions/runs/42255531115)
passes at `cabde167466d7b71e91154283fbf1e7e1911153d`, with the same linker binary
hash as the six project jobs. The verified source manifest contains 1,064 files.
An independent comparison of all file hashes and executable modes matches that
manifest exactly to the published commit, with no additional build files.
The runner is Ubuntu 24.04.5 with Linux `7.0.0-1017-azure`, OCaml 5.4.1,
Dune 3.24.2, GCC 13.3, Clang 18.1.3, binutils 2.42 and glibc 2.39.

- All 265 linker regression cases in 44 Python groups pass without skips.
- All eight required scale/corpus/semantic reports pass with matching source
  and binary identities. Generated programs pass 384 executions with 32
  partial-link stages. The four full scale groups pass.
- Glibc zlib dynamic/static/static-PIE, GCC/Clang musl consumers and Chibicc
  self-host/driver selections pass.
- SQLite passes 40 C API executions, four native Fas consumers and the same
  1,300 extraquick scripts per linker: 398,251 Fas / 398,249 GNU assertions,
  zero errors. The count difference is solely the randomized FTS5 big-token
  script, 307 versus 305 assertions.
- Installation, source/binary archive verification, source rebuilding and
  linked-program smoke tests pass. The rebuilt CLI is byte-identical to the
  delivered/qualified CLI.

Hosted development archive hashes:

| Archive | SHA-256 |
| --- | --- |
| `fas-ld-0.7-dev-linux-x86_64.tar.gz` | `1f4eb488f993128cbfdc0cefda26f2951d97f177f9e4360a028d5e6503846441` |
| `fas-ld-0.7-dev-linux-x86_64-source.tar.gz` | `54474e7745fdf89d65b4831176780d9086bbc5c0847569a2fbfcf5bf4930295a` |

These archives precede this result directory and its documentation updates.
They contain the exact source identified by the retained manifest, not this
later evidence. They remain development artifacts, without a production release
or signature claim. The local 3,792-check compiler gate is earlier evidence;
this hosted recipe runs the linker selection and specified native consumers.

## Setup failures and continuing jobs

The original push correctness job and full-run correctness job stop before
linker tests: source packaging sees the action's local `_opam` compiler switch
as untracked source and rejects an OCaml compiler symlink. The GCC container
job also stops before tests because opam requires missing Bubblewrap.
Its logs additionally expose a host/container path mismatch in artifact paths.

Commit `cabde167466d7b71e91154283fbf1e7e1911153d` ignores the local opam switch,
matches the existing local Docker recipe's opam sandbox configuration, and uses
one container-absolute GCC output root. Manual dispatch can select one suite;
selection-specific concurrency groups allow independent reruns. Actionlint,
diff checks and a source-snapshot check with an actual ignored compiler symlink
pass. The production linker/compiler modules are unchanged by this repair.

At this checkpoint:

- [Correctness with SQLite extraquick](https://github.com/fas-language/fas/actions/runs/42255531115)
  passes in the subsequent observation described above.
- [GCC torture](https://github.com/fas-language/fas/actions/runs/42255525799)
  has passed setup and reached the official execution corpus.
- The first full run's fuzz selection remains in progress.
- Push run `42255515327` is deliberately cancelled as a duplicate of the explicit
  corrected correctness run; cancellation is not passing evidence.

`checkpoint.json` records observation time, source identities, run/job states,
the failures and exact repair file list. Later results must be recorded as a
new observation rather than changing these original failures into passes.
`correctness-observation.json` adds that subsequent successful run, retaining
the original `checkpoint.json` observation and failures.

Hosted GCC/fuzz results, sustained qualification, final contract
freeze and release publication remain open. Version remains `0.7-dev`;
there is no additional C++ scope, LTO implementation or performance campaign.

## Retained evidence

`popular-summary.json` summarizes the six completed jobs. The six `*-reports.json`
files preserve their corpus, cross-library and archive-pipeline reports.
The separate correctness reports, qualification summary, source manifest and
release smoke record identify the successful hosted recipe and archives.
`artifacts-first.json` retains GitHub artifact IDs, sizes, digests and expiry;
`popular-archive-validation.json` records CRC checks and retained linker hashes.
`integrity.json` hashes every other file in this result directory.

Complete downloaded ZIPs and a read-only status/log/artifact monitor are in
`/home/glonex/.cache/fas-linker-qualification/hosted-7e81760-20261106/`.
The monitor retains subsequent completed-run logs, inventories and artifacts,
verifying artifact digests and CRCs. Its JSON observations and any later results
are external to this immutable checkpoint. GitHub's artifact retention is finite;
local copies retain the original archive bytes.
