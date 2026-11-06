# Complete hosted functional/corpus baseline — 2026-11-06

At 19:27 UTC, implementation `4b3f876f7e51d5f1bdc6128b2c780df1de1e1f62`
has passing hosted correctness, official GCC C execution and all six popular-C
jobs. The full repaired-source fuzz campaign remains running. This qualifies the
declared reference functional/corpus baseline; it does not complete the release
candidate freeze, sustained fuzz gate or production publication. Version remains
`0.7-dev`.

The [first repaired-source observation](../2026-11-06-hosted-canonical/README.md)
records the complete 272-case correctness selection, source/archive rebuilding,
4,974 official execution passes per linker, four popular-C jobs, musl follow-ups
and bounded Doom/Wolf3D probes. The [next observation](../2026-11-06-hosted-canonical-followup/README.md)
adds hosted curl, 355 byte-identical current-binary relinks and function-version/
supplied-weak pointer probes. Both records remain unchanged.

[The six-project run](https://github.com/fas-language/fas/actions/runs/42272944421)
is now successful. Libpng's remaining job passes all 33 CTest entries under both
Fas and GNU, same-input replay of eight outputs and both shared-library
producer/consumer directions. Independent GNU configuration checks 15 numeric/
boolean features with no differences. Its downloaded artifact SHA-256 is
`1d74fadba638e99a23cd1f122eabb5eff7684ec7df5c24553286f7ff27a721f0`.
GitHub digest, ZIP CRC and retained JSON member comparisons pass. Its Fas CLI
hash matches all five other popular-C jobs and hosted correctness:
`523660c182b87f80165b32b350069ede9b9676cd5ff24419474c49ac4a847665`.

The selected Lua, bzip2, libpng, libarchive, curl and Redis upstream/runtime suites
all pass with same-input GNU controls for 355 retained outputs. Four libraries
pass both shared-library directions. Lua/libarchive regular/thin/mixed partial
pipelines pass with Fas/GNU/LLD final consumers. Optional-feature and selected
upstream skips remain documented in the individual reports; this is the declared
bounded selection, not all configurations of all projects.

`artifact-validation.json` records eight completed-job artifacts and another
SHA-256 check of each local ZIP. Previous checkpoint directories retain the
other seven full report sets. `libpng-reports.json` adds the final project's
reports; `popular-artifacts.json` retains its hosted inventory. `latest.json`,
`assessment.json` and `observation.json` preserve the partial overall state.
`integrity.json` hashes every other file in this directory.

The [full fuzz run](https://github.com/fas-language/fas/actions/runs/42272946903)
must finish and deliver all fourteen 600-second targets before final acceptance
can be evaluated. The original failed runs and unexplained timeout watchlist
remain recorded. The read-only downloader and bounded final assessor retain
subsequent results in the cache documented by the preceding observation.
Performance, additional C++ scope and LTO remain outside this work.
