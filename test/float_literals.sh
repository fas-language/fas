#!/bin/sh
# This script uses the hidden LLVM test oracle (removal A3, Cli.oracle).
export FAS_ORACLE=llvm
set -eu

ulimit -Sv 4194304
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CC=${CC:-clang-22}
LLVM_OPT=${LLVM_OPT:-opt-22}
LLVM_LLC=${LLVM_LLC:-llc-22}
OCAML_FAS=${OCAML_FAS:-$ROOT/_build/default/bin/main.exe}
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT HUP INT TERM

assert_no_fast_math() {
  if grep -E '(^|[ ,])(nnan|ninf|nsz|arcp|contract|afn|reassoc|fast)([ ,]|$)|llvm\.fmuladd|llvm\.fma' "$1"; then
    echo "float literal test: fast-math flag or fused intrinsic in $1" >&2
    exit 1
  fi
}

python3 - "$TMP" <<'PY'
import random
import sys
from pathlib import Path

directory = Path(sys.argv[1])
randomizer = random.Random(690041)

def random_literals(count, exponent_limit):
    values = []
    for _ in range(count):
        digits = str(randomizer.randrange(1, 10))
        digits += "".join(str(randomizer.randrange(10)) for _ in range(randomizer.randrange(0, 25)))
        exponent = randomizer.randint(-exponent_limit, exponent_limit)
        if len(digits) == 1:
            values.append(f"{digits}e{exponent:+d}")
        else:
            values.append(f"{digits[0]}.{digits[1:]}e{exponent:+d}")
    return values

f32 = [
    "0.0",
    "1.0",
    "1.000000059604644775390625",
    "1.0000000596046447753906250000000000000000000000000001",
    "1.0000000596046447753906249999999999999999999999999999",
    "1.401298464324817e-45",
    "2.802596928649634e-45",
    "1.1754942e-38",
    "1.17549435082228750796873653722224568e-38",
    "3.40282346638528859811704183484516925440e+38",
]
f32.extend(random_literals(1024, 37))
f64 = [
    "0.0",
    "1.0",
    "1.00000000000000011102230246251565404236316680908203125",
    "4.9406564584124654e-324",
    "9.8813129168249309e-324",
    "2.2250738585072009e-308",
    "2.225073858507201383090232717332404064219215980462331830553327416887204e-308",
    "1.797693134862315708145274237317043567981e+308",
]
f64.extend(random_literals(1024, 307))

def write_values(name, ty, values):
    lines = [f"    p[{ty}, {index}] = {value}" for index, value in enumerate(values)]
    return "\n".join(
        [
            f"  fn fas_write_{name}(out addr) void {{",
            "    p addr = out",
            *lines,
            "    return",
            "  }",
        ]
    )

source = "\n".join(
    [
        'extern "C" {',
        write_values("f32", "f32", f32),
        write_values("f64", "f64", f64),
        "}",
    ]
)
(directory / "literals.fas").write_text(source + "\n")

def c_strings(values):
    return ",\n  ".join('"' + value + '"' for value in values)

oracle = f'''#include <locale.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define F32_COUNT {len(f32)}
#define F64_COUNT {len(f64)}

static const char *f32_text[F32_COUNT] = {{
  {c_strings(f32)}
}};
static const char *f64_text[F64_COUNT] = {{
  {c_strings(f64)}
}};

extern void fas_write_f32(void *out);
extern void fas_write_f64(void *out);

int main(void) {{
  float f32_values[F32_COUNT];
  double f64_values[F64_COUNT];
  setlocale(LC_NUMERIC, "C");
  fas_write_f32(f32_values);
  fas_write_f64(f64_values);
  for (size_t i = 0; i < F32_COUNT; ++i) {{
    float expected = strtof(f32_text[i], NULL);
    if (memcmp(&f32_values[i], &expected, sizeof(expected)) != 0) {{
      fprintf(stderr, "f32 literal %zu differs: %s, got %a, expected %a\\n",
              i, f32_text[i], f32_values[i], expected);
      return 1;
    }}
  }}
  for (size_t i = 0; i < F64_COUNT; ++i) {{
    double expected = strtod(f64_text[i], NULL);
    if (memcmp(&f64_values[i], &expected, sizeof(expected)) != 0) {{
      fprintf(stderr, "f64 literal %zu differs: %s, got %a, expected %a\\n",
              i, f64_text[i], f64_values[i], expected);
      return 2;
    }}
  }}
  return 0;
}}
'''
(directory / "oracle.c").write_text(oracle)
PY

"$OCAML_FAS" --emit-llvm "$TMP/literals.fas" >"$TMP/literals.ll"
"$LLVM_OPT" -passes=verify "$TMP/literals.ll" -disable-output
"$CC" -Werror -std=c17 -O0 -ffp-contract=off -fno-fast-math \
  -c "$TMP/oracle.c" -o "$TMP/oracle.o"

for level in 0 2; do
  if [ "$level" -eq 0 ]; then
    cp "$TMP/literals.ll" "$TMP/literals.O$level.ll"
  else
    "$LLVM_OPT" -S "-passes=default<O$level>" "$TMP/literals.ll" \
      -o "$TMP/literals.O$level.ll"
  fi
  assert_no_fast_math "$TMP/literals.O$level.ll"
  "$LLVM_OPT" -passes=verify "$TMP/literals.O$level.ll" -disable-output
  "$LLVM_LLC" -relocation-model=pic -filetype=obj "$TMP/literals.O$level.ll" \
    -o "$TMP/literals.O$level.o"
  "$CC" -Werror -std=c17 -O0 -ffp-contract=off -fno-fast-math \
    "$TMP/literals.O$level.o" "$TMP/oracle.o" -lm -o "$TMP/literals.O$level"
  timeout 60 "$TMP/literals.O$level"
done

echo "float literals: direct binary32/binary64 rounding, 2048 deterministic random decimals, edge values and C bit oracle at O0/O2: ok"
