#!/usr/bin/env python3
"""Differential layout fuzzer: compares cc/layout.fas against clang-22 on
thousands of random C23 struct/union/bitfield/attribute/#pragma pack declarations,
checking sizeof, alignof, offsetof, AND exact bitfield bit offsets.
"""
import os
import random
import subprocess
import sys
import tempfile

CC = os.environ.get("FAS_CC", "clang-22")
CASES = int(os.environ.get("FAS_CC_TYPES_CASES", "2000"))
SEED = int(os.environ.get("SEED", "12345"))

# (probe_spec, c_type, is_bf_eligible, max_bf_bits)
SCALARS = [
    ("bool", "_Bool", True, 1),
    ("char", "char", True, 8),
    ("schar", "signed char", True, 8),
    ("uchar", "unsigned char", True, 8),
    ("short", "short", True, 16),
    ("ushort", "unsigned short", True, 16),
    ("int", "int", True, 32),
    ("uint", "unsigned int", True, 32),
    ("long", "long", True, 64),
    ("ulong", "unsigned long", True, 64),
    ("llong", "long long", True, 64),
    ("ullong", "unsigned long long", True, 64),
    ("i128", "__int128", True, 128),
    ("u128", "unsigned __int128", True, 128),
    ("f16", "_Float16", False, 0),
    ("f32", "float", False, 0),
    ("f64", "double", False, 0),
    ("f80", "long double", False, 0),
    ("ptr", "void *", False, 0),
    ("nullptr", "nullptr_t", False, 0),
    ("cf16", "_Complex _Float16", False, 0),
    ("cf32", "_Complex float", False, 0),
    ("cf64", "_Complex double", False, 0),
    ("cf80", "_Complex long double", False, 0),
    ("ubi1", "unsigned _BitInt(1)", True, 1),
    ("bi3", "_BitInt(3)", True, 3),
    ("ubi7", "unsigned _BitInt(7)", True, 7),
    ("bi13", "_BitInt(13)", True, 13),
    ("ubi24", "unsigned _BitInt(24)", True, 24),
    ("bi33", "_BitInt(33)", True, 33),
    ("ubi65", "unsigned _BitInt(65)", True, 65),
    ("bi96", "_BitInt(96)", True, 96),
    ("ubi129", "unsigned _BitInt(129)", True, 129),
]

BF_SCALARS = [s for s in SCALARS if s[2]]


def gen_cases(rng: random.Random, n_cases: int):
    """Generate n_cases random records (some nested in earlier records)."""
    records = []  # list of dicts
    for idx in range(n_cases):
        is_union = rng.random() < 0.22
        is_packed = rng.random() < 0.20
        al_attr = rng.choice([0, 0, 0, 0, 1, 2, 4, 8, 16, 32])
        pragma_pack = rng.choice([0, 0, 0, 0, 1, 2, 4, 8])
        n_fields = rng.randint(0, 7)

        fields = []
        for fi in range(n_fields):
            is_last = (fi == n_fields - 1)
            r = rng.random()
            # Flexible array member (only last field of non-empty struct)
            if (not is_union) and is_last and len(fields) > 0 and r < 0.06:
                sc = rng.choice(SCALARS)
                f_pk = rng.random() < 0.15
                f_al = rng.choice([0, 0, 0, 2, 4, 8, 16])
                fields.append({
                    "spec": f"flex:{sc[0]}",
                    "c_ty": sc[1],
                    "is_flex": True,
                    "is_arr": False,
                    "arr_len": 0,
                    "is_bf": False,
                    "bw": 0,
                    "packed": f_pk,
                    "align": f_al,
                    "anon": False,
                })
                continue

            # Bitfield
            if r < 0.42:
                sc = rng.choice(BF_SCALARS)
                max_b = sc[3]
                if rng.random() < 0.12:
                    bw = 0  # zero-width bitfield
                else:
                    bw = rng.randint(1, min(max_b, 64)) if max_b > 1 else 1
                    if max_b > 64 and rng.random() < 0.35:
                        bw = rng.randint(1, max_b)
                f_pk = (bw > 0) and (rng.random() < 0.15)
                f_al = rng.choice([0, 0, 0, 0, 2, 4, 8]) if bw > 0 else 0
                fields.append({
                    "spec": sc[0],
                    "c_ty": sc[1],
                    "is_flex": False,
                    "is_arr": False,
                    "arr_len": 0,
                    "is_bf": True,
                    "bw": bw,
                    "packed": f_pk,
                    "align": f_al,
                    "anon": False,
                })
                continue

            # Nested earlier record (without flex array)
            if idx > 0 and r < 0.58:
                cand_idx = rng.randint(max(0, idx - 20), idx - 1)
                if not records[cand_idx]["has_flex"]:
                    f_pk = rng.random() < 0.15
                    f_al = rng.choice([0, 0, 0, 4, 8, 16])
                    fields.append({
                        "spec": f"rec:{cand_idx}",
                        "c_ty": f"R{cand_idx}",
                        "is_flex": False,
                        "is_arr": False,
                        "arr_len": 0,
                        "is_bf": False,
                        "bw": 0,
                        "packed": f_pk,
                        "align": f_al,
                        "anon": False,
                    })
                    continue

            # Fixed-size array
            if r < 0.70:
                sc = rng.choice(SCALARS)
                alen = rng.choice([0, 1, 2, 3, 5])
                f_pk = rng.random() < 0.15
                f_al = rng.choice([0, 0, 0, 4, 8, 16])
                fields.append({
                    "spec": f"arr:{sc[0]}:{alen}",
                    "c_ty": sc[1],
                    "is_flex": False,
                    "is_arr": True,
                    "arr_len": alen,
                    "is_bf": False,
                    "bw": 0,
                    "packed": f_pk,
                    "align": f_al,
                    "anon": False,
                })
                continue

            # Plain scalar
            sc = rng.choice(SCALARS)
            f_pk = rng.random() < 0.15
            f_al = rng.choice([0, 0, 0, 0, 2, 4, 8, 16])
            fields.append({
                "spec": sc[0],
                "c_ty": sc[1],
                "is_flex": False,
                "is_arr": False,
                "arr_len": 0,
                "is_bf": False,
                "bw": 0,
                "packed": f_pk,
                "align": f_al,
                "anon": False,
            })

        has_flex = any(f["is_flex"] for f in fields)
        records.append({
            "idx": idx,
            "is_union": is_union,
            "packed": is_packed,
            "align": al_attr,
            "pragma_pack": pragma_pack,
            "fields": fields,
            "has_flex": has_flex,
        })
    return records


def build_probe_input(records) -> str:
    lines = []
    for rec in records:
        k = "U" if rec["is_union"] else "S"
        pk = 1 if rec["packed"] else 0
        lines.append(f"REC {k} {pk} {rec['align']} {rec['pragma_pack']} {len(rec['fields'])}")
        for f in rec["fields"]:
            ibf = 1 if f["is_bf"] else 0
            fpk = 1 if f["packed"] else 0
            fan = 1 if f["anon"] else 0
            lines.append(f"F {f['spec']} {ibf} {f['bw']} {fpk} {f['align']} {fan}")
    return "\n".join(lines) + "\n"


def emit_c_probe(records) -> str:
    """Emit a C23 program that prints `size align off0:bit0 ...` for each record.
    For non-zero bitfields, we find the exact bit offset by zeroing an instance,
    setting the bitfield to 1, and finding the lowest set bit in the byte image!
    """
    out = [
        "#include <stdio.h>",
        "#include <stddef.h>",
        "#include <stdint.h>",
        "#include <string.h>",
        "",
        "static size_t find_bit_off(const void *p, size_t n) {",
        "    const unsigned char *b = (const unsigned char *)p;",
        "    for (size_t i = 0; i < n; i++) {",
        "        if (b[i]) {",
        "            for (size_t k = 0; k < 8; k++) {",
        "                if (b[i] & (1u << k)) return i * 8 + k;",
        "            }",
        "        }",
        "    }",
        "    return (size_t)-1;",
        "}",
        "",
    ]
    for rec in records:
        idx = rec["idx"]
        pp = rec["pragma_pack"]
        if pp:
            out.append(f"#pragma pack(push, {pp})")
        kw = "union" if rec["is_union"] else "struct"
        attrs = []
        if rec["packed"]:
            attrs.append("packed")
        if rec["align"]:
            attrs.append(f"aligned({rec['align']})")
        attr_s = f" __attribute__(({', '.join(attrs)}))" if attrs else ""
        out.append(f"typedef {kw}{attr_s} R{idx} {{")
        for fi, f in enumerate(rec["fields"]):
            fattrs = []
            if f["packed"]:
                fattrs.append("packed")
            if f["align"]:
                fattrs.append(f"aligned({f['align']})")
            fattr_s = f" __attribute__(({', '.join(fattrs)}))" if fattrs else ""
            if f["is_bf"]:
                if f["bw"] == 0:
                    out.append(f"    {f['c_ty']} : 0;")
                else:
                    out.append(f"    {f['c_ty']} f{fi} : {f['bw']}{fattr_s};")
            elif f["is_flex"]:
                out.append(f"    {f['c_ty']} f{fi}[]{fattr_s};")
            elif f["is_arr"]:
                out.append(f"    {f['c_ty']} f{fi}[{f['arr_len']}]{fattr_s};")
            else:
                out.append(f"    {f['c_ty']} f{fi}{fattr_s};")
        out.append(f"}} R{idx};")
        if pp:
            out.append("#pragma pack(pop)")

    out.append("int main(void) {")
    for rec in records:
        idx = rec["idx"]
        out.append(f"    {{ printf(\"%zu %zu\", sizeof(R{idx}), _Alignof(R{idx}));")
        for fi, f in enumerate(rec["fields"]):
            if f["is_bf"]:
                if f["bw"] == 0:
                    out.append('      printf(" Z");')
                else:
                    out.append(
                        f"      {{ R{idx} _v; memset(&_v, 0, sizeof(_v)); _v.f{fi} = 1; "
                        f"size_t _bo = find_bit_off(&_v, sizeof(_v)); "
                        f"printf(\" %zu:%zu\", _bo / 8, _bo); }}"
                    )
            else:
                out.append(
                    f"      {{ size_t _o = offsetof(R{idx}, f{fi}); "
                    f"printf(\" %zu:%zu\", _o, _o * 8); }}"
                )
        out.append('      printf("\\n"); }')
    out.append("    return 0;\n}")
    return "\n".join(out) + "\n"


def main():
    if len(sys.argv) < 2:
        print("usage: layout_fuzz.py <layout_probe_bin>", file=sys.stderr)
        sys.exit(2)
    probe_bin = sys.argv[1]

    rng = random.Random(SEED)
    records = gen_cases(rng, CASES)
    probe_in = build_probe_input(records)
    c_src = emit_c_probe(records)

    with tempfile.TemporaryDirectory() as d:
        c_path = os.path.join(d, "layout_ref.c")
        exe_path = os.path.join(d, "layout_ref")
        with open(c_path, "w") as f:
            f.write(c_src)
        subprocess.run([CC, "-std=gnu23", "-O0", "-w", c_path, "-o", exe_path], check=True)
        c_out = subprocess.check_output([exe_path], text=True).splitlines()

    fas_out = subprocess.check_output([probe_bin], input=probe_in, text=True).splitlines()

    if len(c_out) != len(fas_out):
        print(f"FAIL: line count mismatch clang={len(c_out)} fas={len(fas_out)}", file=sys.stderr)
        sys.exit(1)

    mismatches = 0
    for idx, (cl, fl) in enumerate(zip(c_out, fas_out)):
        ctoks = cl.split()
        ftoks = fl.split()
        ok = (len(ctoks) == len(ftoks))
        if ok:
            for ct, ft in zip(ctoks, ftoks):
                if ct == "Z":
                    continue  # zero-width bitfield has no C offsetof
                if ct != ft:
                    ok = False
                    break
        if not ok:
            mismatches += 1
            if mismatches <= 15:
                print(f"MISMATCH R{idx}: rec={records[idx]}")
                print(f"  clang: {cl}")
                print(f"  fas:   {fl}")
    if mismatches:
        print(f"FAIL: {mismatches}/{CASES} layout mismatches (seed={SEED})", file=sys.stderr)
        sys.exit(1)
    print(f"layout_fuzz: {CASES}/{CASES} passed (seed={SEED})")


if __name__ == "__main__":
    main()
