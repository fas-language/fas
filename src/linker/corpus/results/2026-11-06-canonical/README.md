# Canonical function-address repair — 2026-11-06

The hosted run at `3f3d9bcbb84cd7e4c62485f3175da2be0bda9361` passes the
correctness job, including all 266 linker cases, SQLite extraquick and the
source/binary archive checks. Its next official GCC run exposes five remaining
Fas failures: 4,969/4,974 executions versus GNU's 4,974/4,974 using the same
objects. `930513-1.c` takes `sprintf`'s address and `pr54937.c` takes `exit`'s
address using non-PIE 32/32S constants. These require canonical PLT addresses.
This checkpoint repairs that behavior; it does not relabel the failed hosted
run or replace the complete GCC acceptance selection with five reduced tests.

The repair gives imported FUNC/IFUNC symbols one PLT address across direct and
GOT references, initialized pointers, DSO callbacks and preload interposition.
The undefined dynamic symbol carries that address; its call slot uses JUMP_SLOT
to resolve the implementation without referring back to itself. Explicit and
unversioned references to the same DSO provider version share the dynamic symbol
and PLT slot. Provider versions remain separate keys. This follows the
[ELF function-address convention](https://refspecs.linuxbase.org/elf/elf.pdf).
Protected functions and narrow PIE imports remain explicitly unsupported.

| Check | Result |
| --- | --- |
| Full workstation root gate | 3,792 compiler checks and 272 linker cases across 45 groups, no skips |
| Reduced canonical-address matrix | 40 executions: ordinary/indirect functions, 32/32S constants, GOT and initialized pointers, DSO pointer equality and preload overrides |
| Independent controls | GNU/Fas/LLD 22 for default imports; GNU/Fas for mixed explicit/unversioned imports |
| Exact retained GCC objects | All six objects link and execute through repaired Fas and GNU in the original pinned GCC 15.2.0/glibc image; the earlier Fas binary reproduces five link failures |
| Original native Fas Sieve programs | All four unchanged sources pass dynamically/statically through Fas, GNU and the native driver: 24 executions |
| Campaign acceptance controls | Four test cases reject short/incomplete runs, missing statistics, dirty/mismatched source identities and saved findings |
| Bounded function-planning fuzz follow-up | 120 seconds, 82,626 executions, 1,596 discovered instrumented edges and no saved crashes/hangs; not the full hosted selection |

The reduced mixed-import probe initially fails both Fas and LLD 22 with unequal
function pointers; GNU succeeds. Its failed log is retained. LLD is therefore
not used as an equality oracle for that selection. The final test also preserves
an unrelated local function with the same name and zero-valued unresolved weak
function references.

The first root attempt times out twice in a deliberate RELRO write-fault test.
This workstation sends core dumps to a piped desktop collector; RLIMIT_CORE=0
does not suppress that path. The child now sets PR_SET_DUMPABLE=0 before the
actual write, and the parent specifically requires SIGSEGV for an unwritable
page. The complete forced rerun passes without increasing the timeout or
skipping cases. The initial log identity remains recorded separately.

Fuzz acceptance now verifies the requested duration, exact completed target
selection, positive execution/edge counts and no saved findings requiring
triage. Hosted acceptance also requires the expected source commit and a clean
working tree. A real downloaded fourteen-target upload smoke passes the
two-second selection and fails the ten-minute selection. Its tar retains 161
original `queue/id:` filenames, and the tar report matches the separate JSON
copy. The `copy-link` target now includes ordinary and indirect function
constants alongside data references to exercise canonical PLT planning.

`hosted-gcc-failure.json` retains counts, selections and verified artifact
identity. `gcc-replay.*` records exact input/binary hashes and commands;
`ubuntu-build-source.json` identifies its older-userspace build snapshot.
`host-regressions.log`, `sieve.json`, `fuzz.json`, `upload-smoke.json` and `observation.json`
record the other checks, superseded attempts and remaining gates. Full source
snapshots, downloaded ZIPs, queues and original logs remain in the recorded
cache. `integrity.json` hashes the other files in this directory.

Complete hosted qualification on the repaired source, the retained
600-second-per-target fuzz selection, final compatibility freeze and release
publication remain open. Version stays `0.7-dev`; performance remains deferred,
existing C++ coverage is preserved and LTO remains unsupported.
