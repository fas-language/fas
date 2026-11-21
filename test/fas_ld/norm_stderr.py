#!/usr/bin/env python3
"""Compare OCaml and fas-ld stderr with known nondeterminism normalized.

Returns exit 0 if stderr is equivalent after:
1. Stripping timing/GC fields from --stats JSON lines (while still requiring
   identical phase sets, identical counts dicts, and identical surrounding lines).
2. Allowing OCaml's random-hashtable choice between bare `<sym>` and `<sym>@@<ver>`
   on the two Symver missing-version-node / non-shared-output diagnostics when
   both linkers fail with exit code 1.
"""

import json
import re
import sys


MISSING_NODE_RE = re.compile(
    r"^fas-ld: symbol ([^@\s]+)(?:@@?(\S+))? references missing version node (\S+)$"
)
DEFINED_VER_RE = re.compile(
    r"^fas-ld: defined symbol version ([^@\s]+)(?:@@?(\S+))? requires shared output$"
)


def normalize_stats_line(line: str):
    if not line.startswith('{"wall_seconds":'):
        return None
    try:
        obj = json.loads(line)
    except json.JSONDecodeError:
        return None
    if not isinstance(obj, dict) or "phases" not in obj or "counts" not in obj:
        return None
    phases = obj.get("phases")
    counts = obj.get("counts")
    if not isinstance(phases, dict) or not isinstance(counts, dict):
        return None
    return json.dumps(
        {"phases": sorted(phases.keys()), "counts": counts},
        sort_keys=True,
        separators=(",", ":"),
    )


def normalize_lines(text: str):
    out = []
    for line in text.splitlines():
        norm = normalize_stats_line(line)
        out.append(norm if norm is not None else line)
    return out


def same_missing_version_diag(ocaml_line: str, fas_line: str) -> bool:
    m1 = MISSING_NODE_RE.match(ocaml_line)
    m2 = MISSING_NODE_RE.match(fas_line)
    if m1 and m2:
        return m1.group(1) == m2.group(1) and m1.group(3) == m2.group(3)
    m1 = DEFINED_VER_RE.match(ocaml_line)
    m2 = DEFINED_VER_RE.match(fas_line)
    if m1 and m2:
        return m1.group(1) == m2.group(1)
    return False


def main() -> int:
    if len(sys.argv) != 4:
        return 2
    exit_code = int(sys.argv[1])
    ocaml_text = open(sys.argv[2], "r", errors="replace").read()
    fas_text = open(sys.argv[3], "r", errors="replace").read()

    ol = normalize_lines(ocaml_text)
    fl = normalize_lines(fas_text)
    if ol == fl:
        return 0
    if exit_code == 1 and len(ol) == len(fl) and len(ol) >= 1:
        if ol[:-1] == fl[:-1] and same_missing_version_diag(ol[-1], fl[-1]):
            return 0
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
