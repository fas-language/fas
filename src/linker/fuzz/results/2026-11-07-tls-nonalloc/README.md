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

TLS validation now explicitly rejects instruction/GOT TLS relocations in
non-allocated sections, including TLSGD, TLSLD, GOTTPOFF, GOTPC32_TLSDESC and
TLSDESC_CALL. DTPOFF debug offsets remain supported. GNU independently rejects
the minimal input without GC as a relocation in an invalid section; with GC it
discards the template and succeeds. Fas deliberately diagnoses this unsupported
input in either mode rather than creating runtime state from non-loaded metadata.

The regression covers all five relocation models with and without GC, checked
diagnostics, prior-output/permission preservation and temporary cleanup. All
existing descriptor runtime tests pass. The original saved input passes 1,000
replays with the repaired instrumented target and checked CLI rejection in both
GC modes. `replay.json` records before/after identities and diagnostics.

These focused repairs do not complete release acceptance. The candidate is
not published; full qualification must run again on the repaired source.
