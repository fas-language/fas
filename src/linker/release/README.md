# Release archives

`package.py` builds matching binary/source archives, with the exact
qualified executable, full selected source tree, license notices, CLI help,
qualification summary, source manifest and ELF runtime dependency information.
The shared [correctness recipe](../ci/correctness.sh) captures the source manifest
before building, checks it after testing, then creates and verifies both archives
under `$RUNNER_TEMP/linker-release`. It then rebuilds the source archive in a
new directory and runs a linked fixture using both delivered and rebuilt tools.
`release-smoke.json` records that check. CI retains the archives without
publishing a release.

For a new local qualification, use the [clean runner](../ci/README.md). For an
already completed matching shared-recipe qualification, from the repository root:

```sh
python3 src/linker/release/package.py build \
  --manifest /absolute/path/results/release-source-manifest.json \
  --qualification /absolute/path/results/qualification-summary.json \
  --linker _build/default/src/linker/bin/main.exe \
  --output /absolute/path/outside/repository/new-release
python3 src/linker/release/package.py verify /absolute/path/outside/repository/new-release/ARCHIVE.tar.gz
```

Use the actual result paths and archive name. A clean-container binary must be
packaged inside the same container environment, where its tools and license are
available. The clean runner performs this automatically. Existing output
directories are rejected. Failed or mismatched qualification, changed source
contents/executable modes and version relabeling are rejected before creating
output. `--ocaml-license PATH` overrides the installed OCaml license location;
the complete notice is copied verbatim. The binary archive includes the project
MIT license and embedded OCaml runtime's LGPL/linking-exception notice. The
source archive includes the full project license and source tree.

`SHA256SUMS` covers both complete compressed archives. The internal inventory
covers every payload member's name, size, permissions and SHA-256. `verify`
reads without extraction or execution and rejects unlisted/changed members,
duplicate paths, traversal paths, links and unexpected ownership/modes. Inventory
and checksums detect corruption, not a malicious replacement of both artifact
and checksum. Packaging does not sign or publish the archives. Published releases
include these archives, `SHA256SUMS`, release notes and separate complete
qualification evidence under the independent `fas-ld-vVERSION` tag. Linker
releases do not change the Fas language's latest-release designation.

The executable version is used in both archive names; `--version` can assert it
but cannot override it. The frozen executable version is `1.0.0`. A passing local
qualification is not a production or hosted-CI claim. The JSON qualification
record is trusted local input, not a signed attestation.

Archive order, owners, normalized modes, gzip header and timestamps are fixed.
`--epoch` / `SOURCE_DATE_EPOCH` selects the timestamp (default zero). Identical
payloads, qualification metadata, timestamp and packaging toolchain yield
byte-identical archives, even in different output directories. This does not
claim OCaml compiler outputs are reproducible across build paths or toolchains.
Runtime dependencies and required GLIBC symbol versions come from the actual
ELF binary, not from the packaging host's libc version. System shared libraries
are not bundled. Native fas-ld execution needs no Python, readelf, OCaml compiler
or external linker. Build/inspection tooling still uses Python 3; packaging uses
readelf and the installed OCaml license.

Regression coverage includes deterministic archive comparisons, a relocated
packaged executable linking/running a program with no tools on PATH, synthetic
qualification rejection, source changes and unsafe/corrupt archive rejection.
Synthetic qualification records used by tests are explicitly labeled fixtures.
