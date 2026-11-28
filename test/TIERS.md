# Compiler validation tiers

Run `bash test/tier.sh quick` during development, `bash test/tier.sh push`
before committing a checkpoint, and `bash test/tier.sh milestone` for full
integration validation. A run reports each part's elapsed time and result
and exits nonzero if any part fails.

| Tier | Coverage |
| --- | --- |
| Quick | Dune build and tests, the strict C manifest, C regressions, driver flags, streamed Fir, 100 random C programs, and C type and ABI tests. |
| Push | Quick plus 23 differential C projects. The target is about 15 minutes. |
| Milestone | Quick and all 28 projects, including PostgreSQL, CPython, OpenSSL, Perl and Git. Repeats the compiler checks with a native-built fcc and adds native Microsoft ABI, preprocessor, kernel boot, Csmith, Fir and ABI fuzzers, and linker API, shadow and fuzz tests. |

Quick and push build a fresh LLVM `-O2` fcc for each run. A project's
cached Clang reference build and output can be reused when its cache
key matches. Set `TIER_JOBS` to choose the total job budget. It defaults
to 3 and is capped at 6 across the project workers.

Each run prints its full log path under `_build/tiers`. Project results
there include per-project durations and full build and test logs.
