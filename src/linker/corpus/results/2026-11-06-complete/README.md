# Completed implementation qualification — 2026-11-06

At 21:04:50 UTC, source `4b3f876f7e51d5f1bdc6128b2c780df1de1e1f62`
passes all four hosted selections: correctness, official GCC C execution,
all six popular-C jobs and all fourteen coverage-guided fuzz targets.
This completes implementation qualification; the final versioned release,
longer timeout follow-ups and durable publication remain separate gates.
Earlier checkpoint observations and failed campaigns remain unchanged.

* [Correctness](https://github.com/fas-language/fas/actions/runs/42272940442):
  272 Python cases in 45 groups without skips, all eight required corpus/scale/
  generated-semantic reports, SQLite extraquick, staged install, source-archive
  rebuilding and delivered/rebuilt executable fixture execution.
* [GCC 15.2](https://github.com/fas-language/fas/actions/runs/42272937898):
  all 4,974 official C executions pass independently with Fas and GNU using
  identical retained objects and matching execution selections.
* [Popular C](https://github.com/fas-language/fas/actions/runs/42272944421):
  Lua, bzip2, libpng, libarchive, curl and Redis pass their declared suites;
  four shared-library corpora pass both producer/consumer directions.
* [Fuzz](https://github.com/fas-language/fas/actions/runs/42272946903):
  600 seconds per target, 6,406,370 total executions, no saved crashes or hangs.
  The minimum measured target duration is 602.142 seconds. All 14 modes and
  the exact clean source commit pass the independent campaign verifier.

`final-assessment.json` retains exact tool/source/binary identities, per-target
AFL execution/edge statistics, job conclusions and all nine downloaded artifact
SHA-256 values. GitHub artifact digests and ZIP CRC validation pass. Edge counts
are not source coverage percentages; counts from different modes are not a
unique combined coverage metric. Checksums are integrity records, not signatures.
`monitor-complete.json` retains the completed downloader observation. Existing
functional, musl, real-Fas and bounded game evidence is linked from the previous
[functional baseline](../2026-11-06-hosted-baseline/README.md) and its follow-ups.

These are bounded selections, not all upstream tests or all possible toolchain
configurations. The reference GCC selection excludes executable-stack
trampolines and LTO. SQLite's public suite is not its full release qualification.
The two historical saved timeout causes remain unknown despite clean replays;
longer final-source follow-ups will retain that limitation explicitly.
