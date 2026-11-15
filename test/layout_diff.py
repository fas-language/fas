import os
import random
import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CC = os.environ.get("CC", "clang-22")
LLVM_OPT = os.environ.get("LLVM_OPT", "opt-22")
OCAML_FAS = os.environ.get("OCAML_FAS", str(ROOT / "_build/default/bin/main.exe"))
SEED = int(os.environ.get("FAS_LAYOUT_DIFF_SEED", "20260929"))
SCALARS = [
    ("u8", "uint8_t"),
    ("i8", "int8_t"),
    ("u16", "uint16_t"),
    ("i16", "int16_t"),
    ("u32", "uint32_t"),
    ("i32", "int32_t"),
    ("u64", "uint64_t"),
    ("i64", "int64_t"),
    ("usize", "size_t"),
    ("isize", "ptrdiff_t"),
    ("bool", "_Bool"),
    ("addr", "void *"),
    ("handle[Opaque]", "struct Opaque *"),
]


def run(args):
    subprocess.run([str(arg) for arg in args], check=True)


def vector_name(lanes, fas_elem):
    return f"V_{lanes}_{fas_elem.replace('[', '_').replace(']', '').replace(',', '_')}"


def make_type(kind, fas, c, depth=0, vector=None):
    return {"kind": kind, "fas": fas, "c": c, "depth": depth, "vector": vector}


def scalar_types():
    result = [make_type("scalar", fas, c) for fas, c in SCALARS]
    return result


def declaration(ty, field):
    if ty["kind"] == "array":
        return declaration(ty["element"], f"{field}[{ty['count']}]")
    return f"{ty['c']} {field}"


def generate():
    rng = random.Random(SEED)
    scalars = scalar_types()
    vectors = {}
    structs = []
    observed = {"scalars": set(), "arrays": set(), "vectors": set(), "bool_vectors": set(), "aligned": 0, "empty": 0}

    def choose_scalar():
        ty = rng.choice(scalars)
        observed["scalars"].add(ty["fas"])
        return ty

    def choose_vector(bool_only=False):
        lanes = rng.randint(1, 8)
        elem = make_type("scalar", "bool", "_Bool") if bool_only else rng.choice(scalars[:10])
        c_name = vector_name(lanes, elem["fas"])
        vectors[(lanes, elem["fas"])] = (c_name, elem["c"])
        fas = f"vec[{lanes},{elem['fas']}]"
        if bool_only:
            observed["bool_vectors"].add(lanes)
        else:
            observed["vectors"].add(lanes)
        return make_type("vector", fas, c_name, vector=(lanes, elem["fas"]))

    def choose_field(index):
        choice = rng.randrange(100)
        if choice < 43:
            return choose_scalar()
        if choice < 62:
            count = rng.randint(0, 5)
            elem = choose_scalar()
            observed["arrays"].add(count)
            return {"kind": "array", "fas": f"arr[{count},{elem['fas']}]", "c": "", "depth": elem["depth"], "element": elem, "count": count}
        if choice < 75:
            return choose_vector()
        if choice < 85:
            return choose_vector(True)
        if choice < 94 and structs:
            candidates = [s for s in structs if s["depth"] < 3]
            if candidates:
                child = rng.choice(candidates)
                if rng.randrange(4) == 0:
                    count = rng.randint(0, 5)
                    observed["arrays"].add(count)
                    observed["nested_arrays"] = observed.get("nested_arrays", 0) + 1
                    elem = make_type("struct", child["fas"], child["c"], depth=child["depth"])
                    return {"kind": "array", "fas": f"arr[{count},{elem['fas']}]", "c": "", "depth": elem["depth"], "element": elem, "count": count}
                return make_type("struct", child["fas"], child["c"], depth=child["depth"])
        return choose_scalar()

    for index in range(500):
        if index % 29 == 0:
            field_types = []
        else:
            field_types = [choose_field(index) for _ in range(rng.randint(1, 5))]
        align = rng.choice([1, 2, 4, 8, 16, 32, 64]) if index % 9 == 0 or rng.randrange(12) == 0 else None
        if align is not None:
            observed["aligned"] += 1
        if not field_types:
            observed["empty"] += 1
        depth = 1 + max((ty["depth"] for ty in field_types), default=0)
        structs.append(
            {
                "fas": f"S{index}",
                "c": f"struct S{index}",
                "fields": field_types,
                "align": align,
                "depth": depth,
            }
        )

    fas = ['use "C" "layout_attributes.h" as c', "opaque Opaque"]
    for st in structs:
        annotation = f" @align({st['align']})" if st["align"] is not None else ""
        fas.append(f"struct {st['fas']}{annotation} {{")
        for field_index, ty in enumerate(st["fields"]):
            fas.append(f"    f{field_index} {ty['fas']}")
        fas.append("}")
    fas.append('extern "C" {')
    c_lines = [
        "#include <stdbool.h>",
        "#include <stddef.h>",
        "#include <stdint.h>",
        "#include <stdio.h>",
        '#include "layout_attributes.h"',
        "struct Opaque;",
    ]
    for (lanes, elem), (name, c_elem) in sorted(vectors.items()):
        c_lines.append(f"typedef {c_elem} {name} __attribute__((ext_vector_type({lanes}))); ")
    for st_index, st in enumerate(structs):
        for query, result in (("size", f"sizeof[{st['fas']}]"), ("align", f"alignof[{st['fas']}]")):
            name = f"fas_{query}_{st_index}"
            fas.append(f"    fn {name}() usize {{ return {result} }}")
            c_lines.append(f"extern size_t {name}(void);")
        for field_index, _ in enumerate(st["fields"]):
            name = f"fas_offset_{st_index}_{field_index}"
            fas.append(f"    fn {name}() usize {{ return offsetof[{st['fas']}, f{field_index}] }}")
            c_lines.append(f"extern size_t {name}(void);")
        attr = f" __attribute__((aligned({st['align']})))" if st["align"] is not None else ""
        c_lines.append(f"struct{attr} S{st_index} {{")
        for field_index, ty in enumerate(st["fields"]):
            c_lines.append(f"    {declaration(ty, f'f{field_index}')};")
        c_lines.append("};")
    attribute_queries = [
        ("packed_size", "PackedTrailing", "sizeof(struct PackedTrailing)"),
        ("packed_align", "PackedTrailing", "_Alignof(struct PackedTrailing)"),
        ("packed_value_offset", "PackedTrailing", "offsetof(struct PackedTrailing, value)"),
        ("aligned_size", "AlignedTrailing", "sizeof(struct AlignedTrailing)"),
        ("aligned_align", "AlignedTrailing", "_Alignof(struct AlignedTrailing)"),
        ("aligned_value_offset", "AlignedTrailing", "offsetof(struct AlignedTrailing, value)"),
        ("typedef_size", "AlignedTypedef", "sizeof(AlignedTypedef)"),
        ("typedef_align", "AlignedTypedef", "_Alignof(AlignedTypedef)"),
        ("typedef_bytes_offset", "AlignedTypedef", "offsetof(AlignedTypedef, bytes)"),
    ]
    for query, c_ty, _ in attribute_queries:
        name = f"fas_attribute_{query}"
        ty = f"c.{c_ty}"
        if query.endswith("size"):
            fas.append(f"    fn {name}() usize {{ return sizeof[{ty}] }}")
        elif query.endswith("align"):
            fas.append(f"    fn {name}() usize {{ return alignof[{ty}] }}")
        else:
            field = "value" if "value" in query else "bytes"
            fas.append(f"    fn {name}() usize {{ return offsetof[{ty}, {field}] }}")
        c_lines.append(f"extern size_t {name}(void);")
    fas.append("}")
    c_lines.extend(
        [
            "#define CHECK(label, actual, expected) do { size_t a = (actual); size_t e = (expected); if (a != e) { fprintf(stderr, \"%s: Fas=%zu C=%zu\\n\", label, a, e); return 1; } } while (0)",
            "int main(void) {",
        ]
    )
    query_count = 0
    for index, st in enumerate(structs):
        c_lines.append(f"    CHECK(\"S{index} sizeof\", fas_size_{index}(), sizeof(struct S{index}));")
        c_lines.append(f"    CHECK(\"S{index} alignof\", fas_align_{index}(), _Alignof(struct S{index}));")
        query_count += 2
        for field_index, _ in enumerate(st["fields"]):
            c_lines.append(
                f"    CHECK(\"S{index}.f{field_index} offsetof\", fas_offset_{index}_{field_index}(), offsetof(struct S{index}, f{field_index}));"
            )
            query_count += 1
    for query, _, expected in attribute_queries:
        c_lines.append(
            f'    CHECK("{query}", fas_attribute_{query}(), {expected});'
        )
        query_count += 1
    c_lines.extend(["    return 0;", "}", ""])
    fas.append("")
    return "\n".join(fas), "\n".join(c_lines), structs, observed, query_count


def main():
    fas_text, c_text, structs, observed, query_count = generate()
    with tempfile.TemporaryDirectory(prefix="fas-layout-diff-") as temporary:
        temporary = Path(temporary)
        (temporary / "layout_attributes.h").write_text(
            "#include <stddef.h>\n"
            "#include <stdint.h>\n"
            "struct PackedTrailing { uint32_t prefix; uint64_t value; } "
            "__attribute__((packed));\n"
            "struct AlignedTrailing { uint32_t value; } "
            "__attribute__((aligned(16)));\n"
            "typedef struct { uint8_t bytes[32]; } AlignedTypedef "
            "__attribute__((aligned(32)));\n"
        )
        fas_source = temporary / "layout.fas"
        c_source = temporary / "layout.c"
        fas_ir = temporary / "fas.ll"
        fas_source.write_text(fas_text)
        c_source.write_text(c_text)
        run([OCAML_FAS, "--emit-llvm", fas_source, "-o", fas_ir])
        run([LLVM_OPT, "-passes=verify", fas_ir, "-disable-output"])
        optimized = temporary / "fas-o2.ll"
        run([LLVM_OPT, "-passes=default<O2>", "-verify-each", fas_ir, "-S", "-o", optimized])
        run([LLVM_OPT, "-passes=verify", optimized, "-disable-output"])
        for level in (0, 2):
            executable = temporary / f"layout-{level}"
            run([CC, "-std=gnu17", "-Werror", "-Wno-zero-length-array", "-Wno-override-module", f"-O{level}", fas_ir, c_source, "-o", executable])
            run([executable])
    print(f"Layout differential: seed {SEED}, {len(structs)} structs, {query_count} sizeof/alignof/offsetof checks, O0/O2 passed")
    print(f"Layout shapes: scalars={len(observed['scalars'])}, array lengths={sorted(observed['arrays'])}, integer-vector lanes={sorted(observed['vectors'])}, bool-vector lanes={sorted(observed['bool_vectors'])}, aligned={observed['aligned']}, empty={observed['empty']}, nesting={max(st['depth'] for st in structs)}")


if __name__ == "__main__":
    main()
