#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM
ulimit -c 0 || true
ulimit -Sv 4194304

python3 - "$ROOT" "$TMP" "$CC" <<'PY'
import pathlib
import re
import subprocess
import sys

root = pathlib.Path(sys.argv[1])
tmp = pathlib.Path(sys.argv[2])
cc = sys.argv[3]
fixture = root / "test/c_import/float_macros.h"
probe = tmp / "macros.c"
probe.write_text(
    "#define _GNU_SOURCE\n#include <float.h>\n#include <math.h>\n"
    f'#include "{fixture}"\n'
)
definitions = subprocess.check_output(
    [cc, "-dM", "-E", str(probe)], text=True
)
names = set()
for line in definitions.splitlines():
    match = re.match(r"#define\s+([A-Za-z_]\w*)(.*)$", line)
    if match and not match.group(2).startswith("("):
        name, value = match.groups()
        if value.strip() and (name.startswith("M_") or name.startswith("FAS_FLOAT_")):
            names.add(name)
required = {
    "FLT_MAX", "FLT_MIN", "FLT_EPSILON", "FLT_TRUE_MIN", "DBL_MAX",
    "DBL_MIN", "DBL_EPSILON", "DBL_TRUE_MIN", "M_PI", "M_E", "M_SQRT2",
    "M_LN2",
}
names.update(required)
ordered = sorted(names)
expansion_probe = tmp / "expansions.c"
with expansion_probe.open("w") as output:
    output.write("#define _GNU_SOURCE\n#include <float.h>\n#include <math.h>\n")
    output.write(f'#include "{fixture}"\n')
    for index, name in enumerate(ordered):
        output.write(f"__FAS_BEGIN_{index} {name} __FAS_END_{index}\n")
expanded = subprocess.check_output([cc, "-E", "-P", str(expansion_probe)], text=True)
lines = expanded.splitlines()

def wrapped(text):
    text = text.strip()
    while text.startswith("("):
        depth = 0
        closing = None
        for index, char in enumerate(text):
            if char == "(":
                depth += 1
            elif char == ")":
                depth -= 1
                if depth == 0:
                    closing = index
                    break
        if closing != len(text) - 1:
            break
        text = text[1:-1].strip()
    return text

decimal = re.compile(r"(?:[0-9]+\.[0-9]*|\.[0-9]+|[0-9]+[eE][+-]?[0-9]+)(?:[eE][+-]?[0-9]+)?")
hexadecimal = re.compile(r"0[xX](?:[0-9a-fA-F]+(?:\.[0-9a-fA-F]*)?|\.[0-9a-fA-F]+)[pP][+-]?[0-9]+")

def literal_type(text):
    text = wrapped(text)
    if text.startswith("-"):
        text = wrapped(text[1:])
    suffix = text[-1] if text and text[-1] in "fFlL" else ""
    body = text[:-1] if suffix else text
    valid = (decimal.fullmatch(body) is not None and ("." in body or "e" in body.lower())) or hexadecimal.fullmatch(body) is not None
    if not valid:
        return None
    if suffix in ("l", "L"):
        return "long"
    return "f32" if suffix in ("f", "F") else "f64"

accepted = []
long_macros = []
for index, name in enumerate(ordered):
    start = f"__FAS_BEGIN_{index} "
    end = f" __FAS_END_{index}"
    expansion = next(
        (line[len(start):-len(end)].strip() for line in lines if line.startswith(start) and line.endswith(end)),
        None,
    )
    kind = literal_type(expansion) if expansion is not None else None
    if kind == "long" and name.startswith("M_"):
        long_macros.append(name)
    elif kind in {"f32", "f64"} and name not in {
        "FAS_FLOAT_OVERFLOW", "FAS_FLOAT_UNDERFLOW", "FAS_FLOAT_HEX_UNDERFLOW"
    }:
        accepted.append((name, kind))
missing = required - {name for name, _ in accepted}
if missing:
    raise SystemExit("required float macros were not classified: " + ", ".join(sorted(missing)))
if not accepted:
    raise SystemExit("no floating macros were classified")

fas = [
    'use "C" <<C\n#define _GNU_SOURCE\n#include <float.h>\n#include <math.h>\n#include <stdio.h>\nC\n',
    f'use "C" "{fixture}"\n',
    "const FAS_FLOAT_CONST f32 = FLT_EPSILON + FLT_TRUE_MIN\n",
    "const FAS_FLOAT_VECTOR vec[4, f32] = {FLT_TRUE_MIN, FLT_EPSILON, M_PIf, M_Ef}\n",
    "fn main() i32 {\n",
]
oracle = [
    "#define _GNU_SOURCE\n#include <float.h>\n#include <math.h>\n#include <stdio.h>\n#include <stdint.h>\n#include <string.h>\n",
    f'#include "{fixture}"\n',
    "static uint32_t f32bits(float value) { uint32_t bits; memcpy(&bits, &value, sizeof bits); return bits; }\n",
    "static uint64_t f64bits(double value) { uint64_t bits; memcpy(&bits, &value, sizeof bits); return bits; }\n",
    "static const float fas_float_const = FLT_EPSILON + FLT_TRUE_MIN;\n",
    "static const float fas_float_vector[4] = {FLT_TRUE_MIN, FLT_EPSILON, M_PIf, M_Ef};\n",
    "int main(void) {\n",
]
for name, kind in accepted:
    if kind == "f32":
        fas.append(f'  printf(c"{name}:%08x\\n", bitcast[u32]({name}))\n')
        oracle.append(f'  printf("{name}:%08x\\n", (unsigned)f32bits({name}));\n')
    else:
        fas.append(f'  printf(c"{name}:%016llx\\n", bitcast[u64]({name}))\n')
        oracle.append(f'  printf("{name}:%016llx\\n", (unsigned long long)f64bits({name}));\n')
fas.extend(
    [
        '  printf(c"CONST:%08x\\n", bitcast[u32](FAS_FLOAT_CONST))\n',
        '  printf(c"VECTOR0:%08x\\n", bitcast[u32](FAS_FLOAT_VECTOR[0]))\n',
        '  printf(c"VECTOR1:%08x\\n", bitcast[u32](FAS_FLOAT_VECTOR[1]))\n',
        '  printf(c"VECTOR2:%08x\\n", bitcast[u32](FAS_FLOAT_VECTOR[2]))\n',
        '  printf(c"VECTOR3:%08x\\n", bitcast[u32](FAS_FLOAT_VECTOR[3]))\n',
        '  printf(c"CVT:%016llx\\n", bitcast[u64](cvt[f64](FLT_EPSILON)))\n',
        '  if M_PI > M_E { printf(c"COMPARE:1\\n") } else { printf(c"COMPARE:0\\n") }\n',
        "  return 0\n}\n",
    ]
)
oracle.extend(
    [
        '  printf("CONST:%08x\\n", (unsigned)f32bits(fas_float_const));\n',
        '  printf("VECTOR0:%08x\\n", (unsigned)f32bits(fas_float_vector[0]));\n',
        '  printf("VECTOR1:%08x\\n", (unsigned)f32bits(fas_float_vector[1]));\n',
        '  printf("VECTOR2:%08x\\n", (unsigned)f32bits(fas_float_vector[2]));\n',
        '  printf("VECTOR3:%08x\\n", (unsigned)f32bits(fas_float_vector[3]));\n',
        '  printf("CVT:%016llx\\n", (unsigned long long)f64bits((double)FLT_EPSILON));\n',
        '  printf("COMPARE:%d\\n", M_PI > M_E);\n',
        "  return 0;\n}\n",
    ]
)
(tmp / "macros.fas").write_text("".join(fas))
(tmp / "oracle.c").write_text("".join(oracle))
(tmp / "long_macros.txt").write_text("\n".join(long_macros) + "\n")
print(
    f"c_float_macros: {sum(name.startswith('FLT_') or name.startswith('DBL_') for name, _ in accepted)} float.h macros, "
    f"{sum(name.startswith('M_') for name, _ in accepted)} direct M_* float macros, "
    f"{len(long_macros)} long double M_* macros"
)
PY

timeout 300 "$CC" -std=gnu17 -Wall -Wextra -Werror -O0 "$TMP/oracle.c" -o "$TMP/oracle"
timeout 30 "$TMP/oracle" >"$TMP/expected"
timeout 300 "$OCAML_FAS" --emit-llvm "$TMP/macros.fas" >"$TMP/program.ll"
"$LLVM_OPT" -passes=verify "$TMP/program.ll" -disable-output
"$LLVM_OPT" -passes='default<O2>' -verify-each "$TMP/program.ll" -S -o "$TMP/program-o2.ll"
"$LLVM_OPT" -passes=verify "$TMP/program-o2.ll" -disable-output
for level in 0 2; do
  timeout 300 "$OCAML_FAS" -O"$level" "$TMP/macros.fas" -o "$TMP/actual-$level"
  timeout 30 "$TMP/actual-$level" >"$TMP/output-$level"
  diff -u "$TMP/expected" "$TMP/output-$level"
done
echo "c_float_macros: float.h, math.h, fixture, const/vector/conversion/bitcast/comparison bits match C at O0/O2"
