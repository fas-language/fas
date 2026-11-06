# Hosted failure follow-up — 2026-11-06

The [first hosted checkpoint](../2026-11-06-hosted/README.md) remains historical
evidence. Its six popular-C jobs and corrected correctness job pass, but the
remaining GCC and fuzz jobs subsequently fail. This follow-up records those
failures and a local compatibility repair, not production qualification.

[GCC run 42255525799](https://github.com/fas-language/fas/actions/runs/42255525799)
reaches the official tests at `cabde167466d7b71e91154283fbf1e7e1911153d`.
Every attempted Fas link rejects the pinned container's `crt1.o` because its
section addresses are nonzero. Fas records zero execution passes; the GNU
same-input replay records 4,956 against the unchanged requirement of 4,974.
The report also contains replay-input failures. A fresh official run is required;
the focused checks below do not satisfy that gate.

The exact GCC 15.2.0 image contains `.note.GNU-stack` with `sh_addr=0x5c` and
`.note.ABI-tag` with `sh_addr=0x20`. The reader previously rejected these fields
before linking. The repair accepts them while retaining section-relative symbol
and relocation calculations; output layout supplies addresses. Fas partial
output still uses zero section addresses.

The new reduced regression changes section addresses in text, initialized data,
BSS, unwind data and notes. Direct execution and mixed partial/final links pass
with Fas, GNU and LLD: 12 executions. It fails against the earlier Fas reader.
The complete local root gate passes: 266 linker cases across 44 groups without
skips, plus 3,792 compiler checks. Using the exact failed container's startup
files, a program with TLS and a constructor links and runs in fixed, PIE,
static and static-PIE modes through both Fas and GNU: eight executions.
These focused container checks use an Ubuntu 24.04 build of the modified source
so the linker's own GLIBC requirements are compatible with the GCC image.

[The original full run's fuzz job](https://github.com/fas-language/fas/actions/runs/42240863721/job/127970478118)
reports a successful campaign step but fails artifact upload: original AFL queue
filenames contain colons, which upload-artifact rejects. No detailed campaign
report or queues survived that runner, so it supplies no retained coverage or
execution-count claim. Commit `1685dad347902e6664271515799ac1b3ea9b19ed`
packages the original files inside a tar archive and uploads a separate report.
The two-second-per-target upload smoke is not release qualification. The full
ten-minute-per-target campaign must be repeated with retained artifacts.

`observation.json` records original run identities, artifact verification,
failure counts and the local gate. `gcc-crt.json`, `gcc-crt.log`, `check.c` and
`check.sh` retain the focused reproduction. The host regression log is retained
with its digest. `integrity.json` hashes this directory's other files.
Full downloaded CI archives and source snapshots remain under
`/home/glonex/.cache/fas-linker-qualification/`.

The first host gate attempt used a container-prefix musl wrapper from the host
and failed to find its specifications file. It was stopped and superseded by
the complete passing run with `/tmp/fas-musl-20261105/sysroot`. An initial
container diagnostic pipeline also exited before linking because of SIGPIPE.
Neither attempt is counted as passing evidence.

The complete hosted matrix on the repaired source, sustained qualification,
final contract review and release publication remain open. Version stays
`0.7-dev`; performance, additional C++ scope and LTO are unchanged.
