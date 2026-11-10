# Single-thread optimization evidence

See the [analysis and reproduction notes](../../../research/2026-11-10-serial-optimization.md).

- `staged/report.json`: initial shared-PC baseline, whole-array merge sort and
  merge-sort/index candidates, plus one-worker native references; 15 samples.
- `idle-final/report.json`: user-declared idle window, including per-target
  validation; 21 samples. `idle-context.json` records the measurement context.
- `fields-check/report.json`: rejected integer-field decoder versus the
  per-target candidate with the legacy zero-width-marker fallback; 15 samples.
  `rejected-fields-elf_reader.ml.txt` is that experiment's reader source.
- `hash-check/report.json`: isolated full-block SHA-1 load experiment; 21 samples.
- `final-replay/report.json`: final source versus baseline and native references;
  21 samples, after the idle window. All 14 final Fas images match the baseline.
- `counters/`, `final-counters/`, `hotpath-counters/`: separate hardware-counter
  repetitions, exact commands and untimed statistics. Counter CSVs contain
  perf's mean/error fields; ordinary replay samples remain in the reports.
- `validation.json`, `final-tests*`: final full local test command, source hashes
  and results. The earlier `full-tests*` and targeted logs preserve staging.
- `fuzz-smoke/`: three bounded 45-second campaigns and acceptance verification;
  full queues, seeds, source and instrumented executable remain external.
- `sqlite-final-flat.txt`: final no-ID self profile, with trailing whitespace
  trimmed for storage. Complete call stacks/binary data are indexed externally.
- `sqlite-closure-recheck.json`: unchanged prior GNU-observed input closure;
  not proof of identical traversal by all linkers.
- `artifact-index.json`: hashes for 2,045 retained external files, including
  snapshots of binaries, linked outputs, profiles, source and fuzz artifacts.

Every report records tool identities, exact per-tool arguments, runtime checks,
raw samples and output hashes. No parallel parsing, daemon, fork-based early
return, host affinity/governor/cache change or dependency was added to Fas.
The results are a local optimization checkpoint, not a new released binary or
replacement for the existing full production qualification evidence.
