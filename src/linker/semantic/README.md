# Generated valid-program comparisons

This supplements malformed-input fuzzing with programs whose runtime results
are computed independently in Python. From the repository root:

```sh
python3 src/linker/semantic/run.py --output /tmp/fas-semantic-new \
  --cases 64 --lld ld.lld
```

The output must be new and outside the repository. `--seed N --cases COUNT`
selects consecutive deterministic seeds; defaults are `0xFA500` and 64 cases.
Use your installed LLD executable name, such as `ld.lld-22`, or omit `--lld`
for Fas/GNU comparisons. The executable's invocation name is preserved because
LLD selects its driver through that name. GNU as/ar and ld are required.
No compiler frontend, system libc startup or external project download is needed.

Each case generates a bounded acyclic call graph with 4–9 nodes and an independent
64-bit modulo-arithmetic/rotation oracle. It varies constants, graph edges,
provider order and alignments. The emitted assembly uses GOTPCRELX and PC-relative
64-bit data references, callee-save registers and CFI, weak/strong definitions,
common BSS declarations with different sizes/alignments, dead code, an archive
provider with GNU_RETAIN and an initially undeclared forced undefined root.
The program checks zero-initialized BSS and writes its eight-byte result.

Four pipelines are compared: direct objects; lazy regular-archive extraction;
Fas then GNU partial linking; and GNU then Fas partial linking. Both partial
pipelines carry a `-u` root through stages so the final link must select its
provider without another `-u` option. GNU's preservation policy is used in
Fas; LLD 22's partial linker differs, so LLD is a final consumer here.

All pipelines produce static ET_EXEC and interpreter-free static PIE, each with
and without section GC. Every selected final linker must match the Python
runtime oracle, retain the forced archive provider, use the expected ELF kind,
combine common sizes and remove/retain dead symbols according to GC policy.
The generated PIEs use PC-relative access and linker GOT bootstrap rewrites;
they do not test a libc CRT self-relocator or general absolute-pointer startup.
No generated or fuzz-mutated input from outside this harness is executed.

With three final consumers, 64 cases select 3,072 final links/executions and 256
partial links. The normal CI recipe selects eight fixed cases: 384 final
links/executions and 32 partial links. Its selection guard checks every expected
pipeline/mode/consumer combination and all seed identities. Regular Dune tests
also run the complete two-case matrix and deliberately alter the Python oracle
to prove that a successful executable with a wrong result is rejected.

Outputs include all generated sources, objects, intermediate/final images,
models, command results, a linker/harness snapshot and `report.json`. Failures
keep their case directory for reproduction. This is correctness qualification;
no timing comparison, general randomized-C equivalence proof, hosted CI run or
production-readiness claim follows from it.
