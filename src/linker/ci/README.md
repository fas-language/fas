# Clean local correctness qualification

The hosted workflow runs correctness checks on relevant pushes and pull requests;
the weekly schedule runs the full selection. Manual dispatch defaults to `all`.
Its `suite` input can also select `correctness`, `popular-c`, `gcc-torture` or
`fuzz` to repeat one selection without restarting unrelated long-running jobs.
Concurrency groups include the selection, so separate manual selections coexist:

```sh
gh workflow run linker.yml --repo fas-language/fas --ref YOUR_BRANCH -f suite=gcc-torture
```

Fuzzing defaults to 600 seconds per target. To check artifact delivery after a
workflow change, select `suite=fuzz` with `fuzz_seconds=2`. This short run is an
upload smoke check, not release qualification. Full campaigns retain their
original AFL filenames inside `linker-fuzz.tar.gz`, plus a separate JSON report;
direct upload rejects the colons in AFL queue/finding names.

The GCC job uses its pinned compiler container, with opam sandboxing disabled
inside that container, and retains inputs/results from a common container path.

The hosted correctness job and the local Docker runner use the same
[`correctness.sh`](correctness.sh) recipe. It builds isolated musl, forces the
linker regression suite, runs the full scale selection, glibc zlib in three
linkage modes, GCC/Clang musl zlib in three modes, Chibicc self-host/driver tests,
SQLite/native Fas consumers, generated valid-program semantic comparisons and
a staged installation. Normal runs select
SQLite `main.test`; `--extraquick` selects the weekly upstream suite instead.
The final verifier requires 273 Python regression cases, no reported skips and
passing reports from all eight corpus/scale/semantic selections. It also verifies
the source hashes and executable modes captured before the build. A successful
recipe creates and verifies matching [binary/source archives](../release/README.md)
with checksums in `results/linker-release`, then rebuilds the source archive and
runs a fixture using both delivered and rebuilt linkers. Update the explicit
regression count when adding/removing cases deliberately.

From the repository root, with Docker access:

```sh
python3 src/linker/ci/clean.py --output /absolute/path/outside/repository/new-run
```

The runner builds the checksum-pinned Ubuntu 24.04 base with OCaml 5.4.1 and
Dune 3.24.2, then copies tracked and non-ignored untracked source files into a
new output directory. It records that precise manifest, including uncommitted
changes, and checks that source files remain unchanged after a successful run.
Host build products, `.git`, the host toolchain and home directory are absent
from the container mounts. The output directory is its sole host bind mount;
the container runs with the caller's uid/gid. `SYS_PTRACE` enables the GDB
regressions. There is no privileged container or Docker socket mount.
The container's file-descriptor limit is 65,536: Docker's much larger default
can prevent Valgrind 3.22 from starting before it creates its diagnostic log.

The manifest, image ID, package inventory, logs, reports, native inputs and
binaries remain in the output directory. The named container is stopped on
completion/interruption and retained for diagnosis; its name is in `report.json`.
Use `--no-build` to reuse the toolchain image and `--image NAME` to select its
tag. The immutable image ID is resolved before execution. The default per-phase
timeout is two hours; `--timeout SECONDS` overrides it.

This is a fresh userspace check sharing the host kernel. It does not establish
hosted GitHub Actions execution, another kernel's compatibility, or the weekly
popular-C/GCC/fuzz jobs. Ubuntu apt and opam repositories remain online build
inputs; recording their resolved package versions and image ID does not make
the image build hermetic. Pinned project archives are verified by the fetcher.
Musl output interpreters use container-absolute sysroot paths: replay those
artifacts with the same `/qualification` mount, rather than directly on the host.
No performance comparison is made.
