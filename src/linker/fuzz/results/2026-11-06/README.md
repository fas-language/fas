# Output-layout follow-up campaigns — 2026-11-06

These campaigns follow conventional-section coalescing. The first two reuse
saved queues from the earlier unwind/static corpus; imported entries and hashes
are retained in the report. Each campaign uses one AFL worker. The two campaign
processes overlapped; no timing comparison is made.

| Target | Requested seconds | Executions | Bitmap edges | Saved crashes | Saved hangs |
| --- | ---: | ---: | ---: | ---: | ---: |
| link | 600 | 361899 | 2342 | 0 | 0 |
| shared-link | 600 | 438853 | 3192 | 0 | 0 |
| partial-link | 180 | 133575 | 1509 | 0 | 0 |
| static-pie-link | 180 | 157493 | 2751 | 0 | 0 |
| metadata-link | 180 | 135682 | 3146 | 0 | 0 |
| symver-link | 180 | 149800 | 3139 | 0 | 0 |
| copy-link | 180 | 158905 | 1564 | 0 | 0 |

Total: **1,536,207 executions**, zero saved crashes or hangs. Both campaigns
exit successfully. `section-coalescing.json` and `output-modes.json` pin targets,
commands, source hashes, imported queues and complete local report hashes.
Source snapshots, binaries, logs and queues remain at:

- `/home/glonex/.cache/fas-linker-qualification/fuzz-sections-20261106`
- `/home/glonex/.cache/fas-linker-qualification/fuzz-output-modes-20261106`

These bounded in-memory targets do not execute emitted programs. Bitmap edges
are instrumentation observations, not source/branch coverage. Short local
campaigns do not replace sustained release qualification. The host's external
core collector can delay signal findings; both saved-crash and saved-hang
counts were checked, and uncaught OCaml exceptions use exit status 2.
