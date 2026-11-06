# Thin archive and partial-link qualification — 2026-11-06

GNU ar can emit a thin long-name reference such as `/0             /` when an
object's basename is exactly 15 characters long. Fas rejected this ordinary
archive while GNU ld and LLD accepted it. The new real-library pipeline found
the problem in libarchive's `archive_acl.c.o`; a two-object source regression
reproduces it independently, with indexed/lazy and unindexed/whole selection,
partial output and Fas/GNU/LLD final consumers. Thin references now accept that
terminal name marker, retaining the existing numeric, offset and resource checks.
Regular-archive parsing is unchanged.

| Check | Result |
| --- | --- |
| Forced workstation root gate | 3,792 compiler checks; 265 linker cases in 44 Python groups; no skips |
| Workstation archive pipelines | Lua 5.1.5 and libarchive 3.8.11; 154 archive members; 36 final links; 276 runtime checks |
| Libarchive selected assertions | 473,820 checked; zero failed assertions/tests or skips across the matrix |
| Fresh Ubuntu Lua rebuild | Original Fas/GNU upstream selection passes; 24 archive-pipeline links and 180 runtime checks with Fas/GNU/LLD 18 |
| Current-binary real-program replay | All 355 retained outputs from six C projects are byte-identical to their executed Fas images |
| Fresh Ubuntu correctness recipe | All 265 linker cases, eight corpus/scale/semantic reports, no regression skips |
| Generated programs in fresh recipe | 8 seeds; 384 final links/executions; 32 partial links |
| SQLite in fresh recipe | Same 1,300 extraquick scripts per linker; Fas 398,231 assertions, GNU 398,247; zero errors |
| Archive-runner guard controls | 10 pass, including empty/failed/skipped/duplicate summaries and a changed CRT hash |
| Package and selection controls | 13 pass; independent packaging byte-identical |
| Installation/source delivery | Installed CLI, source-archive rebuild and both linked-program smoke tests pass |
| Workflow syntax | Actionlint passes; hosted execution remains unqualified |

The four archive pipelines are regular archives, lazy thin archives,
Fas-to-GNU partial stages repackaged as regular archives, and GNU-to-Fas partial
stages repackaged as thin archives. All three final consumers run each selection.
Partial stages aggregate whole archives, changing extraction granularity; output
byte equality is not required for these transformations. Lua repeats its 13
example scripts and bytecode compile/execute checks. Libarchive runs eight
upstream read/write cases for tar, zip, cpio and 7zip with its reference data,
requiring exactly one executed test, positive assertions, zero failures and
zero skips per invocation. This is not another complete libarchive CTest run.

The 355-output replay uses the original upstream campaign recorded at the
[semantic checkpoint](../2026-11-06-semantic/README.md), after verifying recorded
object/archive/CRT hashes. It does not claim a new complete six-project upstream
runtime campaign. The replay report records every output hash; its complete
commands remain in the preserved external run. Historical upstream libarchive
CTest logs also include temporary-directory collision diagnostics in both linker
phases; their successful CTest exit statuses do not imply zero startup assertion
diagnostics. The new sequential eight-case matrix requires zero assertions failed.

The fresh reference image is
`sha256:aa87b9d35c03f5128095052cb377c52e776275203270eb0d5f76ed86558537b7`:
Ubuntu 24.04.5, OCaml 5.4.1, Dune 3.24.2, GCC 13.3, Clang/LLD 18,
binutils 2.42, glibc 2.39 and isolated musl 1.2.5. Containers share the
workstation kernel. SQLite's count difference is confined to the recorded
randomized `fts5bigtok.test`, 285 versus 301 assertions. Every selected script
finishes with zero errors. Version remains `0.7-dev`.

## Timeout triage and remaining qualification

The previous 4,304-byte shared-output timeout is **not reproducible**: three
direct replays with its original target finish promptly, and a new 600-second
campaign starting from that exact input completes 440,771 executions with zero
saved crashes, hangs or timeouts. The normal CLI reports a debug relocation
overflow, with and without GC. Ten replays with the current target and two
current-CLI checks also pass their expected outcomes.

The archive campaign with the parser repair completes 457,298 executions in
600 seconds, saving zero crashes and **one new timeout**. Its 5,418-byte input
also finishes promptly: five initial and 1,000 subsequent direct replays pass;
the CLI rejects an out-of-range section link. A further 600-second campaign
starting from that exact input completes 429,568 executions with zero saved
crashes, hangs or timeouts, using the same current target and two-second limit.

Both inputs remain on the watchlist. Their root causes are **unknown**; these
observations do not establish scheduling as the cause. Original failed campaigns
remain failed. One standalone `afl-showmap` fork-server attempt failed during
setup with a bad-file-descriptor diagnostic; it supplies no successful replay
evidence. Successful follow-ups use `afl-fuzz`, matching the original runner.
AFL edge occupancy is not source/branch coverage. These bounded local runs do
not complete sustained or hosted fuzz qualification.

Hosted execution, another-kernel qualification, final compatibility-contract
review and publication remain open. No LTO or additional C++ scope was added;
the main performance pass remains deferred. This is not a production release
or a faster-than-LLD claim.

## Evidence and artifacts

JSON records retain source/binary identities, exact selections, original
findings, failed/superseded runs and follow-ups. `integrity.json` hashes every
other file in this directory. Complete external run roots are under
`/home/glonex/.cache/fas-linker-qualification/`:

- `ubuntu24-thin-fifteen-20261106`: fresh recipe, full logs, inputs, installation,
  source rebuilding, archives and 13 package controls.
- `archive-pipelines-final-20261106`: members, archives, partial objects, all
  programs, command logs and final harness snapshots.
- `ubuntu24-lua-archives-20261106`: independent source rebuild and archive matrix;
  checksum-pinned Lua archive, isolated container and original upstream checks.
- `popular-thin-fifteen-relink-20261106`: all 355 current-binary outputs and commands.
- `shared-timeout-triage-20261106`, `fuzz-thin-fifteen-20261106` and
  `archive-timeout-followup-20261106`: inputs, targets, queues, findings,
  logs and replay/campaign records.

The source manifest and qualified archives precede this result directory and
the final documentation updates. Production modules, tests, harnesses and
workflow files match the qualified snapshot; later differences are documentation
and qualification records. Archives are development artifacts, not published
or signed releases:

| Artifact | SHA-256 |
| --- | --- |
| `fas-ld-0.7-dev-linux-x86_64.tar.gz` | `72c9f688cf916d0de7fdd11a2aa8522eaefe4f92dd1626838d2912afd01e4b29` |
| `fas-ld-0.7-dev-linux-x86_64-source.tar.gz` | `41d68b36ed8b92a63e48a8d5fa8ead1e18960de6ddb57135a78ca63d27ab8389` |

They are retained in `ubuntu24-thin-fifteen-20261106/results/linker-release`.
The delivered, installed and rebuilt Ubuntu CLI SHA-256 is
`c6e5a7a5ed11dcb620ce13606d30b3ac30274fb6817b85d6a42e06d904cb1c4a`.
System shared libraries are not bundled; runtime requirements and native OCaml
notices follow the existing release-package contract.
