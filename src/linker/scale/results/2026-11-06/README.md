# Final section-layout large-input replay

All four full-profile groups pass on linker SHA-256
`5d4d4afeaaf3ba572d19b78204aa0ba0cca2730358abb450199fc53404775260`.
The compact [result](section-coalescing.json) pins the complete report and local
snapshot at `/home/glonex/.cache/fas-linker-qualification/scale-sections-final-20261106`.

Coverage includes 1,024 actual inputs, 65,260 retained payload sections,
200,000 definitions and pointer relocations, and a 64 MiB file-backed payload.
GNU final consumers, partial links, runtime/all-byte checks, deterministic
reentry and limit/publication preservation pass. This does not increase the
published limits or measure link speed, peak memory or multicore scaling.
