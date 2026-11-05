# Migrated large-input qualification

`linux-x86-64.json` records four passing full-profile groups under the migrated
layout. The executable snapshot and complete artifacts are at
`/tmp/fas-linker-scale-migrated-20261105`.

Coverage includes 1,024 distinct input files, 65,260 retained payload sections,
200,000 named definitions and pointer relocations, and a 64 MiB file-backed
payload. Runtime checks, GNU consumers, deterministic repetition and overflow
publication-preservation checks pass. The report pins sources, tools, linker and
outputs. This does not measure speed, peak memory or multicore scaling, and does
not raise the documented input/output limits.
