#!/usr/bin/env python3
# Malformed-input fuzzer for the Fas front end (roadmap v0.7 hardening:
# "Fas front-end mutation fuzzing: always an error or success, never an
# exception"). Mutates valid .fas files and checks that the compiler
# either succeeds (exit 0) or refuses with diagnostics (exit 1). Anything
# else is a crash: an uncaught OCaml exception (exit 2, "Fatal error"), an
# internal error (exit 3), a stack overflow, a signal, or a run longer
# than the timeout. Crashing inputs are kept in $FAS_REFUSE_KEEP (./crashes).
#
#   fas_refuse.py FAS COUNT SEED FILE.fas...
#
# Most mutations are "gentle" (whole-word swaps within one family:
# keywords, types, operators, names, numbers), so that many mutants get
# past the parser into name resolution, type checking, lowering and the
# backend; the rest edit tokens blindly (delete, duplicate, swap,
# truncate) to exercise the lexer and parser, and a few nest deeply (an
# OCaml stack overflow is a bug too).
import collections, os, random, re, shutil, subprocess, sys, tempfile
from concurrent.futures import ThreadPoolExecutor

tool, n, seed = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
files = sys.argv[4:]
corpus = [open(f, encoding="latin-1").read() for f in files]
TOK = re.compile(r"\s+|//[^\n]*|0x[0-9a-fA-F_]+|\d[\d_]*(?:\.\d+)?(?:e-?\d+)?|\w+|\"(?:[^\"\\]|\\.)*\"|'(?:[^'\\]|\\.)*'|<<=|>>=|==|!=|<=|>=|<<|>>|&&|\|\||\+=|-=|\*=|/=|=>|->|\.\.|.", re.S)
TYPES = ["u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64", "usize", "isize", "bool", "void", "addr",
         "f16", "f32", "f64", "f80", "[4]u8", "[0]i32", "*u8", "?i32", "Nope"]
NUMS = ["0", "-1", "1", "255", "256", "65535", "4294967296", "-9223372036854775808", "9223372036854775807",
        "18446744073709551615", "18446744073709551616", "99999999999999999999999", "0x", "0xffffffffffffffffff",
        "0.5", "1e999", "1e-999", "1_000"]
WORDS = ["fn", "return", "if", "else", "while", "break", "continue", "const", "var", "struct", "extern", "defer",
         "use", "for", "switch", "case", "default", "error", "fails", "fail", "try", "catch", "errdefer", "secret",
         "declassify", "asm", "C", "undefined", "true", "false", "null", "self", "main"]
FAM = [["fn", "const", "var", "struct", "extern"],
       ["if", "while", "for", "switch"],
       ["return", "break", "continue", "fail", "defer", "errdefer"],
       ["try", "catch", "fails"],
       ["u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64", "usize", "isize", "bool", "addr", "void"],
       ["f16", "f32", "f64", "f80", "u64", "i32"],
       ["true", "false", "0", "1"]]
OPS = [["+", "-", "*", "/", "%", "&", "|", "^", "<<", ">>"],
       ["==", "!=", "<", "<=", ">", ">=", "&&", "||"],
       ["=", "+=", "-=", "*=", "/="],
       ["(", ")", "[", "]", "{", "}"],
       [",", ";", ":", "."]]


def gentle(rng, text):
    for _ in range(rng.randint(1, 3)):
        op = rng.randrange(5)
        if op < 2:
            fam = rng.choice(FAM)
            a, b = rng.choice(fam), rng.choice(fam)
            occ = [m.start() for m in re.finditer(r"\b%s\b" % re.escape(a), text)]
            if not occ:
                continue
            if op == 0:
                text = re.sub(r"\b%s\b" % re.escape(a), b, text)
            else:
                k = rng.choice(occ)
                text = text[:k] + b + text[k + len(a):]
        elif op == 2:
            fam = rng.choice(OPS)
            a, b = rng.choice(fam), rng.choice(fam)
            occ = [m.start() for m in re.finditer(re.escape(a), text)]
            if occ:
                k = rng.choice(occ)
                text = text[:k] + b + text[k + len(a):]
        elif op == 3:
            # one use of a name becomes another name of the file
            names = re.findall(r"\b[a-zA-Z_]\w*\b", text)
            if names:
                a, b = rng.choice(names), rng.choice(names)
                occ = [m.start() for m in re.finditer(r"\b%s\b" % re.escape(a), text)]
                k = rng.choice(occ)
                text = text[:k] + b + text[k + len(a):]
        else:
            occ = list(re.finditer(r"(?<![\w.])\d[\d_]*\b", text))
            if not occ:
                continue
            m = rng.choice(occ)
            text = text[:m.start()] + rng.choice(NUMS + ["2", "3", "7", "8", "16", "64", "128", "4096"]) + text[m.end():]
    return text


def blind(rng, text):
    toks = TOK.findall(text)
    for _ in range(rng.randint(1, 4)):
        if not toks:
            break
        k = rng.randrange(len(toks))
        op = rng.randrange(9)
        if op == 0:
            del toks[k]
        elif op == 1:
            toks.insert(k, toks[rng.randrange(len(toks))])
        elif op == 2:
            j = rng.randrange(len(toks))
            toks[k], toks[j] = toks[j], toks[k]
        elif op == 3:
            toks[k] = rng.choice(TYPES)
        elif op == 4:
            toks[k] = rng.choice(NUMS)
        elif op == 5:
            toks[k] = rng.choice(WORDS)
        elif op == 6:
            s = "".join(toks)
            return s[: rng.randrange(len(s) + 1)]
        elif op == 7:
            # a whole line duplicated or deleted
            s = "".join(toks).split("\n")
            i = rng.randrange(len(s))
            if rng.random() < 0.5:
                del s[i]
            else:
                s.insert(i, s[rng.randrange(len(s))])
            return "\n".join(s)
        elif op == 8:
            toks[k] = rng.choice(["(", ")", "{", "}", "[", "]", ",", "=", ":", "\"\\zz\"", "\"\\x\"", "'", "\"",
                                  "/*", "\n", "\x00", "\xff", "é"])
    return "".join(toks)


def deep(rng, text):
    # a deeply nested expression or block where a number or a statement
    # was; 100k levels: an OCaml stack overflow is a bug
    d = rng.choice([1000, 10000, 100000])
    occ = list(re.finditer(r"(?<![\w.])\d+\b", text))
    kind = rng.randrange(3)
    if kind == 0 and occ:
        m = rng.choice(occ)
        return text[:m.start()] + "(" * d + "1" + ")" * d + text[m.end():]
    if kind == 1 and occ:
        m = rng.choice(occ)
        return text[:m.start()] + "1 + " * d + "1" + text[m.end():]
    k = text.find("{")
    if k < 0:
        return text
    return text[:k + 1] + "{" * d + "}" * d + text[k + 1:]


def classify(code, err):
    if code == 0:
        return None
    # only the messages: a diagnostic's indented excerpt quotes the source,
    # whose comments may say "internal error" themselves
    low = "\n".join(l for l in err.splitlines() if l[:1] not in (" ", "\t")).lower()
    if code == 1 and "internal error" not in low and "fatal error" not in low \
            and "exception" not in low and "stack overflow" not in low:
        return None
    lines = [l for l in err.strip().splitlines() if l.strip()]
    first = next((l for l in lines if "error" in l.lower() or "exception" in l.lower()), lines[0] if lines else "(no message)")
    return f"exit {code}: {first[:200]}"


outcomes = collections.Counter()


def one(i):
    rng = random.Random(seed * 1000003 + i)
    idx = rng.randrange(len(corpus))
    text = corpus[idx]
    r = rng.random()
    src = gentle(rng, text) if r < 0.65 else blind(rng, text) if r < 0.97 else deep(rng, text)
    mode = rng.choice([["check"], ["check", "--all-errors"], ["-c"], ["-c"], ["-c", "-g"], ["--emit-ir"]])
    d = tempfile.mkdtemp()
    try:
        open(os.path.join(d, "m.fas"), "w", encoding="latin-1").write(src)
        args = [tool, mode[0], "m.fas", *mode[1:]] if mode[0] == "check" else [tool, *mode, "m.fas", "-o", "o"]
        try:
            res = subprocess.run(args, capture_output=True, timeout=60, cwd=d,
                                 env=dict(os.environ, OCAMLRUNPARAM="b"))
            outcomes[res.returncode] += 1
            bad = classify(res.returncode, res.stderr.decode("latin-1"))
        except subprocess.TimeoutExpired:
            bad = "timeout"
    finally:
        shutil.rmtree(d, ignore_errors=True)
    if bad:
        os.makedirs(keep, exist_ok=True)
        q = os.path.join(keep, f"{seed}_{i}.fas")
        open(q, "w", encoding="latin-1").write(src)
        return f"{q} [{' '.join(mode)}] from {os.path.basename(files[idx])}: {bad}"
    return None


# FAS_REFUSE_KEEP: where crashing inputs go (default ./crashes);
# FAS_REFUSE_ONLY=i,j,...: rerun only these input indices of the seed (each
# input depends only on the seed and its index, so a find reproduces alone)
keep = os.environ.get("FAS_REFUSE_KEEP") or "crashes"
only = os.environ.get("FAS_REFUSE_ONLY")
indices = [int(x) for x in only.split(",")] if only else range(n)
with ThreadPoolExecutor(os.cpu_count()) as ex:
    res = [r for r in ex.map(one, indices) if r]
seen = {}
for r in res:
    seen.setdefault(re.sub(r"\d+", "N", r.split(": ", 1)[-1]), r)
for r in seen.values():
    print(r)
print(f"inputs {n}: compiled {outcomes[0]}, refused {outcomes[1]}, crashes {len(res)} ({len(seen)} distinct)")
sys.exit(1 if res else 0)
