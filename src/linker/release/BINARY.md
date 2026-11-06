# fas-ld Linux x86-64

This archive contains the fas-ld command-line linker. See
`.fas-release/qualification.json` for the archive's correctness qualification,
the accompanying release evidence for complete release acceptance, and
`help.txt` for the installed CLI. The frozen support contract is in the matching
source archive at `src/linker/SUPPORT.md`.

Copy `bin/fas-ld` to a directory on PATH or invoke it directly. The linker needs
no OCaml installation at runtime. It uses the system ELF interpreter and shared
libraries listed in `.fas-release/build-info.json`; the required GLIBC symbol
versions are recorded there. Those libraries are not bundled. C programs still
need a compiler, libc development files and suitable startup objects.

The matching source archive contains build instructions, the supported feature
matrix, test harnesses and known limitations under `src/linker/`. LTO is not
supported. Windows and other CPU architectures are future work.

Check both downloads with `sha256sum -c SHA256SUMS`. The source archive also
contains `src/linker/release/package.py verify ARCHIVE`, a Python 3 inspection
tool which validates every member without extracting or executing it. Checksums
and internal inventories detect corruption; they are not signatures.

The project license is in `LICENSE`. The embedded OCaml runtime's license and
linking exception are reproduced in `licenses/OCaml-LICENSE`.
