# Non-allocated TLS descriptor finding — 2026-11-07

The final frozen-source shared-output hour saved one reproducible crash near
its end. The original campaign remains failed. `finding.input` retains the
4,336-byte input and `minimal.s` is an independent assembly reproducer.

A relocation-section target mutation moved TLS instruction/GOT references into
non-allocated metadata. Metadata does not root TLS definitions under section GC,
but GOT preparation created a runtime descriptor whose local dynsym emission
then unconditionally accessed the discarded template placement. This raises
`Invalid_argument("option is None")` on source `6633f26`. The normal CLI also
reproduces it with `-shared --gc-sections`.

Dynamic descriptor planning now checks that its canonical locally bound TLS
symbol has a section definition retained by GC before creating its TLS dynsym.
A discarded definition raises a checked linker error. Existing metadata relocation
behavior and DTPOFF debug offsets remain supported. GNU independently rejects
the minimal input without GC as a relocation in an invalid section; with GC it
discards the template and succeeds. Fas accepts the metadata reference when its
template exists and diagnoses a discarded descriptor definition explicitly.

The regression covers local, hidden and internal TLS definitions, ordinary
metadata acceptance, GC diagnostics, prior-output/permission preservation,
temporary cleanup and DTPOFF debug tombstones. All existing descriptor runtime
and shared-output metadata tests pass. The saved input passes 1,000 repaired
target replays, normal CLI acceptance without GC and checked rejection with GC.
`replay.json` records before/after identities and diagnostics.

An initial broader TLS validation guard rejected instruction/GOT relocations in
all non-allocated sections. Full regression found this incompatible with an
existing synthetic metadata fixture on the hosted compiler baseline. That guard
was superseded by the narrower descriptor-definition check; original failed
local/hosted results remain retained and are not release acceptance.

At this repair checkpoint, the candidate was not published and complete
qualification remained required. The repaired source subsequently passed
[final acceptance](../../../corpus/results/2026-11-07-v1.0.0/README.md)
and was published as fas-ld 1.0.0. Original failed results remain unchanged.
