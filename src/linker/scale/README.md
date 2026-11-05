# Large-input correctness qualification

From the repository root, with OCaml/Dune, Python 3 and GNU as/ld/readelf:

```sh
dune build --profile release src/linker/bin/main.exe
python3 src/linker/scale/run.py _build/default/src/linker/bin/main.exe \
  --output /tmp/fas-linker-scale-new --profile full
```

The output directory must be new. It retains a snapshot of `fas-ld`, generated
assembly/objects, executables, command logs and `report.json`. The report records
tool versions, linker/source hashes and per-case results, including failures.
The default `quick` profile uses smaller payloads and symbol tables. Both profiles
test the section-count rejection boundary.

The full profile checks:

- 1,024 independent input files succeed; a 1,025th input fails explicitly.
- 65,260 distinct payload sections survive final linking with correct contents.
  Crossing the output section-index limit fails in both final and partial links.
- 200,000 named definitions and 200,000 pointer relocations execute correctly.
  A partial link is consumed by both Fas and GNU ld, and repeated Fas final
  output is byte-identical.
- A 64 MiB file-backed payload retains every byte, checked by SHA-256, with
  runtime checks for interior pointers and endpoint contents. An input larger
  than the 128 MiB file limit fails before parsing.
- Rejections preserve the old output's contents, mode and inode, and leave no
  temporary publication files.

GNU ld produces and executes independent comparison images. Output layouts need
not match: GNU combines `.rodata.*`/`.data.*`, while Fas retains the declared
section policy. Inspectors account for these differences.

These checks run separately from `dune runtest` because their retained artifacts,
input sizes and memory use are larger than ordinary regression fixtures. Allow
roughly 1 GiB of free disk space for a full run. Sparse oversized-input fixtures
have large logical sizes. The runner does not compare speed or qualify peak
memory, multicore behavior, all resource caps, or real programs of this size.
Existing regular tests separately cover nearly 1 GiB of sparse NOBITS address
space and signed/unsigned relocation overflow. None of these results justify
raising the documented limits on their own.
