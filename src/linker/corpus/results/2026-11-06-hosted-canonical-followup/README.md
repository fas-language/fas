# Curl and retained-input follow-up — 2026-11-06

This supplements the [19:12 UTC hosted observation](../2026-11-06-hosted-canonical/README.md)
without changing that earlier record. The tested linker implementation remains
`4b3f876f7e51d5f1bdc6128b2c780df1de1e1f62`. The later `aff5e98` commit records
evidence and documentation only.

[Curl's job](https://github.com/fas-language/fas/actions/runs/42272944421/job/128081914481)
now passes. Each linker reports 1,553 tests OK out of 1,839 considered; optional
feature/server and upstream selection skips remain recorded. Both shared-library
producer/consumer directions pass. The independent GNU configuration control
checks 510 numeric/boolean features and reports no differences. Same-input GNU
replay covers 326 retained curl outputs.

The downloaded artifact has SHA-256
`de3d35ce49df3c84f238c06bc7c0fe8904d75127386b49817cb762db1affc90a`.
Its bytes match GitHub's digest; ZIP CRC and unique-member checks pass, and all
retained JSON reports agree with their actual members. Retained Fas tools have
the same Ubuntu CLI hash as correctness and the other completed projects:
`523660c182b87f80165b32b350069ede9b9676cd5ff24419474c49ac4a847665`.

Separately, the current workstation linker reproduces all 355 outputs from the
retained passing six-project upstream corpus byte for byte: Lua 2, bzip2 4,
libpng 8, libarchive 10, curl 326 and Redis 5. The replay verifies unchanged
toolchain inputs, each retained object/archive hash and each previously executed
Fas image before linking. Its CLI hash is
`ccdf827ca1323c6149037bd46d41ee38653ef66e95f220ed52cea6e4ab654182`.
This proves output equality for those inputs; it is not a fresh upstream runtime
campaign, performance comparison or substitute for libpng's new hosted job.

Additional pointer probes exercise two versions of the same imported function
simultaneously and a weak function supplied by the DSO. Narrow `32`/`32S`
constants agree with GOT entries, initialized `64` pointers, PLT calls and DSO
address getters. Distinct versions remain distinct. Fas and GNU pass both the
ordinary and mixed default/explicit-name variants, normally and under preload.
LLD 22 also passes both executions of the distinct-version variant. Its mixed
variant exits 99 at a pointer-equality assertion, reproducing the earlier mixed
canonical-address observation. That report remains `failed`; it is not relabeled
as a passing LLD control. The series records ten successful executions and one
LLD failure, with no Fas/GNU failures. The first probe invocation used a missing
unversioned LLD command; the corrected probe selects the installed versioned LLD.
The source fixtures and both reports are retained separately.

At this observation, five repaired-source hosted popular-C jobs pass. Libpng
is still setting up OCaml, and both the previous-source and repaired-source
full fuzz campaigns are running. The complete repaired-source fuzz selection
must finish and retain its artifacts before its duration/coverage/findings gate
can be assessed. The older source's GCC failures retain their original status.

The read-only downloader remains active. An additional background assessor waits
for download completion, then writes `final-assessment.json` in the recorded
cache using the exact source/selection/duration checks. It records results;
it does not resume coding or publish a release. `automation.json` records its
process IDs, bounded lifetime and output path. The scripts preserve their
actual local paths for reproduction. If the downloader or assessor expires,
inspect its explicit expiry/error records before using any incomplete result.

`curl-reports.json`, `curl-test-summary.json` and `artifact-validation.json`
preserve the downloaded evidence and checks. The replay script, log and report
preserve all 355 output identities. `latest.json` and `observation.json` freeze
this subsequent partial observation; `integrity.json` hashes the other files.
Version stays `0.7-dev`; freeze and durable release publication remain open.
