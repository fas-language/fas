#!/usr/bin/env python3
"""Archive/symbol/CLI conformance, using system GNU ld as an independent oracle."""
import itertools
import json
from pathlib import Path
import random
import shutil
import struct
import subprocess
import unittest

import integration as core

run = core.run
LINKER = core.LINKER
elf_symbols = core.elf_symbols
elf_sections = core.elf_sections


def ar_members(data):
    result = []
    at = 8
    while at < len(data):
        size = int(data[at + 48:at + 58])
        result.append(dict(header=at, offset=at + 60, size=size,
                           name=data[at:at + 16].rstrip()))
        at += 60 + size + (size & 1)
    return result


def member(name, data):
    header = (name.ljust(16) + b"0".ljust(12) + b"0".ljust(6) + b"0".ljust(6)
              + b"644".ljust(8) + str(len(data)).encode().ljust(10) + b"`\n")
    assert len(header) == 60
    return header + data + (b"\n" if len(data) & 1 else b"")


class ArchiveTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    link = core.LinkerTests.link
    rejected = core.LinkerTests.rejected

    def archive(self, name, *objects, indexed=True, tool="ar"):
        path = self.root / (name + ".a")
        run(tool, "rcs" if indexed else "rcS", path, *objects)
        return path

    def symbols(self, path):
        return {s["name"]: s for s in elf_symbols(path.read_bytes())}

    def compare_exit(self, name, objects, expected=42, options=()):
        for tool, suffix in ((LINKER, "fas"), ("ld", "gnu")):
            run(self.link(name + suffix, *objects, linker=tool, options=options), expected=expected)

    def test_strong_weak_common_precedence(self):
        weak = self.assemble("weak-answer", ".text\n.weak answer\nanswer: mov $7,%eax; ret")
        weak2 = self.assemble("weak2-answer", ".text\n.weak answer\nanswer: mov $9,%eax; ret")
        for order in itertools.permutations([self.simple, self.answer, weak]):
            self.compare_exit("strong-beats-weak", order)
        self.compare_exit("weak-first", [self.simple, weak, weak2], expected=7)
        self.compare_exit("weak-second", [self.simple, weak2, weak], expected=9)
        for objects in ([weak, self.answer], [self.answer, weak]):
            syms = [s for s in elf_symbols(self.link("one-answer", self.simple, *objects).read_bytes())
                    if s["name"] == "answer"]
            self.assertEqual(len(syms), 1)
            self.assertEqual(syms[0]["info"] >> 4, 1)

        common1 = self.assemble("common-small", ".comm shared,8,64")
        common2 = self.assemble("common-large", ".comm shared,100,16")
        weak_data = self.assemble("weak-data", ".weak shared\n.data\nshared: .quad 99")
        strong_data = self.assemble("strong-data", ".globl shared\n.data\nshared: .quad 42\n.size shared,8")
        start = self.assemble("common-start", ".text\n.globl _start\n_start: mov $60,%eax; xor %edi,%edi; syscall\n.data\n.quad shared")
        for objects in itertools.permutations([common1, common2, weak_data]):
            out = self.link("merged-common", start, *objects)
            symbol = self.symbols(out)["shared"]
            self.assertEqual(symbol["size"], 100)
            self.assertEqual(symbol["value"] % 64, 0)
            self.assertEqual(elf_sections(out.read_bytes())[symbol["section"]]["kind"], 8)
            run(out)
        for order in itertools.permutations([common1, strong_data, weak_data]):
            for tool in (LINKER, "ld"):
                out = self.link("override-common", start, *order, linker=tool)
                symbol = self.symbols(out)["shared"]
                self.assertEqual(symbol["size"], 8)
                self.assertEqual(elf_sections(out.read_bytes())[symbol["section"]]["kind"], 1)

    def test_unresolved_weak_and_strong_reference_upgrade(self):
        weak_start = self.assemble("weak-start", ".weak optional\n.text\n.globl _start\n_start: movabs $optional,%rax; test %rax,%rax; setne %dil; movzbl %dil,%edi; mov $60,%eax; syscall")
        definition = self.assemble("optional-definition", ".globl optional\n.data\noptional: .quad 1")
        library = self.archive("optional", definition)
        self.compare_exit("weak-zero", [weak_start, library], expected=0)
        self.compare_exit("weak-defined", [weak_start, definition], expected=1)
        strong_ref = self.assemble("optional-required", ".globl optional")
        self.compare_exit("weak-upgraded", [weak_start, strong_ref, library], expected=1)
        for tool in (LINKER, "ld"):
            run(tool, "-o", self.root / "weak-late", weak_start, library, strong_ref, expected=1)

    def test_visibility_propagation(self):
        for directive, expected in (("protected", 3), ("hidden", 2), ("internal", 1)):
            ref = self.assemble("visibility-" + directive, f".{directive} answer\n.data\n.quad answer")
            for objects in ([ref, self.answer], [self.answer, ref]):
                out = self.link("visible", self.simple, *objects)
                self.assertEqual(self.symbols(out)["answer"]["other"], expected)
                run(out, expected=42)
        missing = self.assemble("hidden-missing", ".hidden missing\n.globl missing")
        self.rejected(missing, "undefined non-default visibility", self.simple, self.answer)
        hidden_weak = self.assemble("hidden-weak", ".hidden optional\n.weak optional\n.data\n.quad optional")
        run(self.link("hidden-weak-ok", self.simple, self.answer, hidden_weak), expected=42)

    def test_lazy_extraction_chain_order_and_unused_members(self):
        last = self.assemble("chain-last", ".text\n.globl last\nlast: mov $42,%eax; ret")
        middle = self.assemble("chain-middle", ".text\n.globl answer\nanswer: jmp last")
        unsupported = self.assemble("unused-tls", '.section .tdata,"awT",@progbits\n.globl unused_tls\n.type unused_tls,@tls_object\nunused_tls: .quad 1\n.text\nlea unused_tls@TLSGD(%rip),%rdi')
        library = self.archive("backwards-chain", last, unsupported, middle)
        self.compare_exit("archive-chain", [self.simple, library])
        self.assertNotIn("unused_tls", self.symbols(self.link("lazy-symbols", self.simple, library)))
        self.rejected(library, "undefined symbol answer", self.simple)
        self.rejected(self.simple, "TLS relocation requires dynamic output", library, options=("--whole-archive",))
        entrylib = self.archive("entry-only", self.simple, self.answer)
        self.compare_exit("entry-archive", [entrylib])

    def test_groups_repeated_archives_and_whole_archive(self):
        a = self.assemble("cycle-a", ".text\n.globl answer\nanswer: jmp b")
        b = self.assemble("cycle-b", ".text\n.globl b\nb: jmp c")
        c = self.assemble("cycle-c", ".text\n.globl c\nc: mov $42,%eax; ret")
        liba = self.archive("cycle-a", a, c)
        libb = self.archive("cycle-b", b)
        self.rejected(self.simple, "undefined symbol c", liba, libb)
        self.compare_exit("archive-repeat", [self.simple, liba, libb, liba])
        for tool in (LINKER, "ld"):
            out = self.root / "archive-group"
            run(tool, "-o", out, self.simple, "--start-group", liba, libb, "--end-group")
            run(out, expected=42)
        run(LINKER, "-o", self.root / "nested-group", self.simple, "--start-group",
            "--start-group", liba, "--end-group", libb, "--end-group")
        run(self.root / "nested-group", expected=42)
        extra = self.assemble("unused-extra", ".globl extra\n.data\nextra: .quad 1")
        libextra = self.archive("extra", extra)
        out = self.root / "whole-extra"
        run(LINKER, "-o", out, self.simple, self.answer, "--whole-archive", libextra,
            "--no-whole-archive", libextra)
        self.assertIn("extra", self.symbols(out))
        self.rejected(self.simple, "duplicate symbol answer", self.answer,
                      self.archive("duplicate", self.answer), options=("--whole-archive",))
        unindexed = self.archive("unindexed", self.answer, indexed=False)
        self.rejected(self.simple, "no symbol index", unindexed)
        run(self.link("whole-unindexed", self.simple, unindexed, options=("--whole-archive",)), expected=42)

    def test_common_archive_extraction_policy(self):
        start = self.assemble("common-policy-start", ".text\n.globl _start\n_start: mov shared(%rip),%edi; mov $60,%eax; syscall")
        common = self.assemble("common-policy", ".comm shared,8,8")
        strong = self.assemble("common-policy-strong", ".globl shared\n.data\nshared: .quad 42")
        weak = self.assemble("common-policy-weak", ".weak shared\n.data\nshared: .quad 7")
        othercommon = self.assemble("common-policy-other", ".comm shared,128,64")
        self.compare_exit("common-pull-strong", [start, common, self.archive("common-strong", strong)])
        self.compare_exit("common-no-pull-weak", [start, common, self.archive("common-weak", weak)], expected=0)
        libcommon = self.archive("common-common", othercommon)
        for tool in (LINKER, "ld"):
            out = self.link("common-no-pull-common", start, common, libcommon, linker=tool)
            self.assertEqual(self.symbols(out)["shared"]["size"], 8)
        self.compare_exit("common-weak-first", [start, common, self.archive("common-mixed", weak, strong)])

    def test_forward_archive_passes_preserve_weak_selection(self):
        weak = self.assemble("pass-weak", ".text\n.weak target\ntarget: mov $7,%eax; ret")
        middle = self.assemble("pass-middle", ".text\n.globl answer\nanswer: jmp target")
        strong = self.assemble("pass-strong", ".text\n.globl target\ntarget: mov $42,%eax; ret")
        library = self.archive("forward-pass", weak, middle, strong)
        self.compare_exit("forward-selection", [self.simple, library])
        reverse = self.archive("forward-weak", strong, middle, weak)
        self.compare_exit("forward-weak-selection", [self.simple, reverse], expected=7)

        # A queued member can become unnecessary after another extraction.
        # Its unsupported payload must stay unparsed, just like any unused member.
        root = self.assemble("queue-root", ".text\n.globl _start\n_start: call answer; mov %eax,%edi; mov $60,%eax; syscall\n.data\n.quad target")
        both = self.assemble("queue-both", ".text\n.globl answer,target\nanswer: mov $42,%eax; ret\ntarget: ret")
        bad = self.assemble("queue-unused", '.section .tdata,"awT",@progbits\n.globl target\ntarget: .quad 1')
        self.compare_exit("queued-now-unused", [root, self.archive("queue", both, bad)])

    def test_library_search_and_long_names(self):
        directory = self.root / "library directory"
        directory.mkdir(exist_ok=True)
        obj = self.assemble("a_very_long_archive_member_name_with_spaces and symbols", ".text\n.globl answer\nanswer: mov $42,%eax; ret")
        library = directory / "libanswer.a"
        run("ar", "rcs", library, obj)
        for flags in (("-L", directory, "-lanswer"), ("-l", "answer", "-L" + str(directory)),
                      ("-l:libanswer.a", "-L", directory)):
            out = self.root / "library-search"
            run(LINKER, "-o", out, self.simple, *flags)
            run(out, expected=42)
        self.compare_exit("long-member", [self.simple, library])
        llvm_ar = shutil.which("llvm-ar")
        if llvm_ar:
            llvm_library = self.archive("llvm-archive", obj, tool=llvm_ar)
            self.compare_exit("llvm-long-member", [self.simple, llvm_library])
        self.rejected(self.simple, "cannot find", options=("-L", directory, "-lmissing"))

    def test_64_bit_archive_index(self):
        data = self.answer.read_bytes()
        index_size = 8 + 8 + len(b"answer\0")
        header_offset = 8 + 60 + index_size + (index_size & 1)
        index = struct.pack(">QQ", 1, header_offset) + b"answer\0"
        path = self.root / "index64.a"
        path.write_bytes(b"!<arch>\n" + member(b"/SYM64/", index) + member(b"answer.o/", data))
        self.compare_exit("index64-program", [self.simple, path])

    def test_seeded_archive_graphs_and_library_reentry(self):
        rng = random.Random(0xA2C61)
        for case in range(12):
            count = rng.randrange(4, 16)
            objects = []
            for index in range(count):
                targets = sorted(rng.sample(range(index + 1, count),
                                            min(count - index - 1, rng.randrange(4))))
                # Each function returns 42; calls and address references exercise
                # graph extraction without relying on the linker's own output parser.
                source = f'.text\n.globl graph_{index}\ngraph_{index}:\n'
                source += '\n'.join(f'call graph_{target}' for target in targets)
                source += '\nmov $42,%eax; ret\n.data\n'
                source += '\n'.join(f'.quad graph_{target}+3' for target in targets)
                objects.append(self.assemble(f'archive-graph-{case}-{index}', source))
            rng.shuffle(objects)
            libraries = [self.archive(f'graph-{case}-{n}', *objects[n::3]) for n in range(3)]
            start = self.assemble(f'graph-start-{case}', '.text\n.globl _start\n_start: call graph_0; mov %eax,%edi; mov $60,%eax; syscall')
            for tool in (LINKER, "ld"):
                out = self.root / "graph-group"
                run(tool, "-o", out, start, "--start-group", *libraries, "--end-group")
                run(out, expected=42)
        library = self.archive("reentry", self.answer)
        result = run(core.REENTRY, self.simple, library)
        self.assertIn(b"concurrent invocations passed", result.stdout)

    def test_common_validation_and_archive_container_edges(self):
        common = self.assemble("common-validation", ".comm common_value,8,8")
        original = common.read_bytes()
        symbol = next(s for s in elf_symbols(original) if s["name"] == "common_value")
        for field, fmt, value, diagnostic in ((8, "Q", 3, "alignment"),
                                             (8, "Q", 1 << 40, "alignment"),
                                             (16, "Q", (1 << 64) - 1, "size"),
                                             (4, "B", 0x20, "must be global")):
            data = bytearray(original)
            struct.pack_into("<" + fmt, data, symbol["offset"] + field, value)
            path = self.root / "bad-common.o"
            path.write_bytes(data)
            self.rejected(path, diagnostic, self.simple, self.answer)
        data = bytearray(original)
        data[symbol["offset"] + 4] = 0x15  # STB_GLOBAL | STT_COMMON
        path = self.root / "stt-common.o"
        path.write_bytes(data)
        run(self.link("stt-common-program", self.simple, self.answer, path), expected=42)
        empty = self.root / "empty.a"
        empty.write_bytes(b"!<arch>\n")
        run(self.link("empty-archive-ok", self.simple, empty, self.answer), expected=42)
        path = self.root / "bad-container.a"
        containers = [
            (b"!<arch>\n" + member(b"x/", b"x")[:-1] + b"x", "padding"),
            (b"!<arch>\n" + member(b"//", b"unterminated"), "long member name"),
            (b"!<arch>\n" + member(b"//", b"a/\n") + member(b"/1", b""), "long member name"),
            (b"!<arch>\n" + member(b"/", b"\0\0\0\0") * 2, "duplicate symbol index"),
            (b"!<arch>\n" + member(b"//", b"") * 2, "duplicate long name"),
            (b"!<arch>\n" + member(b"/", b"\0\0\0\0junk"), "trailing garbage"),
        ]
        for data, diagnostic in containers:
            path.write_bytes(data)
            self.rejected(self.simple, diagnostic, path, self.answer)

    def test_malformed_archives_and_failure_preservation(self):
        library = self.archive("valid-mutated", self.answer)
        original = library.read_bytes()
        members = ar_members(original)
        index = members[0]
        obj = members[1]
        cases = []
        def change(name, at, payload, diagnostic):
            data = bytearray(original)
            data[at:at + len(payload)] = payload
            cases.append((name, data, diagnostic))
        change("terminator", obj["header"] + 58, b"!!", "member header")
        change("size-negative", obj["header"] + 48, b"-1        ", "decimal")
        change("size-huge", obj["header"] + 48, b"9999999999", "decimal")
        change("bad-offset", index["offset"] + 4, struct.pack(">I", obj["offset"]), "member header")
        change("too-many-symbols", index["offset"], struct.pack(">I", 0xffffffff), "limit")
        change("unneeded-index-name", index["offset"] + 8, b"wrong!", "undefined symbol answer")
        change("bad-long-name", obj["header"], b"/1".ljust(16), "long member name")
        payload = self.answer.read_bytes().replace(b"answer\0", b"wrong!\0")
        change("lying-index", obj["offset"], payload, "missing definition")
        cases.append(("thin-truncated", b"!<thin>\n/0", "range"))
        cases.append(("junk", b"!<arch>\n" + b"x", "byte range"))
        for name, data, diagnostic in cases:
            with self.subTest(name=name):
                path = self.root / "bad-archive.a"
                path.write_bytes(data)
                self.rejected(self.simple, diagnostic, path)
        alias = self.root / "archive-alias.a"
        alias.write_bytes(original)
        run(LINKER, "-o", alias, self.simple, alias, expected=1)
        self.assertEqual(alias.read_bytes(), original)

    def test_archive_mutation_campaign(self):
        original = self.archive("fuzz-source", self.answer).read_bytes()
        path = self.root / "fuzz-archive.a"
        out = self.root / "fuzz-archive-out"
        for size in list(range(80)) + [len(original) - 1, len(original) // 2]:
            path.write_bytes(original[:size])
            self.rejected(self.simple, "fas-ld:", path)
        rng = random.Random(0xA2C41)
        for _ in range(256):
            data = bytearray(original)
            for _ in range(rng.randint(1, 8)):
                data[rng.randrange(len(data))] ^= rng.randrange(1, 256)
            path.write_bytes(data)
            result = subprocess.run([LINKER, "-o", str(out), str(self.simple), str(path)],
                                    capture_output=True, timeout=3)
            self.assertIn(result.returncode, (0, 1), result.stderr.decode(errors="replace"))
            self.assertNotIn(b"Fatal error", result.stderr)

    def test_response_files_quotes_nesting_and_errors(self):
        spaced = self.root / "answer with spaces.o"
        shutil.copy(self.answer, spaced)
        out = self.root / "response output"
        nested = self.root / "nested.rsp"
        nested.write_text('"' + str(spaced) + '"')
        response = self.root / "arguments.rsp"
        response.write_text(f'-o "{out}" \'{self.simple}\' "@{nested}"')
        run(LINKER, "@" + str(response))
        run(out, expected=42)
        response.write_text(f'-o "{out}" {self.simple} ' + str(spaced).replace(" ", "\\ "))
        run(LINKER, "@" + str(response))
        run(out, expected=42)
        for text, diagnostic in (("'unterminated", "unterminated quote"),
                                 ("trailing\\", "trailing backslash"),
                                 ("a\0b", "NUL"), ("@" + str(response), "recursive response")):
            response.write_text(text)
            self.rejected(self.simple, diagnostic, options=("@" + str(response),))
        for arguments in (("--start-group",), ("--end-group",), ("-L",), ("-l",),
                          ("-l:",), ("-l:../answer.a",), ("--start-group",) * 17):
            run(LINKER, *arguments, expected=1)

    def test_statistics_are_json_and_do_not_change_output(self):
        library = self.archive("statistics", self.answer)
        plain = self.link("stats-plain", self.simple, library)
        out = self.root / "stats-output"
        result = run(LINKER, "--stats", "-o", out, self.simple, library)
        stats = json.loads(result.stderr)
        self.assertEqual(plain.read_bytes(), out.read_bytes())
        self.assertEqual(stats["counts"]["objects"], 2)
        self.assertEqual(stats["counts"]["archives"], 1)
        self.assertEqual(stats["counts"]["extracted_members"], 1)
        self.assertEqual(stats["counts"]["input_bytes"], self.simple.stat().st_size + library.stat().st_size)
        self.assertEqual(stats["counts"]["output_bytes"], out.stat().st_size)
        for name in ("read", "parse", "resolve", "emit", "relocate", "write"):
            self.assertGreaterEqual(stats["phases"][name]["wall_seconds"], 0)
        self.assertGreater(stats["gc"]["allocated_words"], 0)

    def test_repeated_reads_preserve_archive_state_and_logical_budgets(self):
        library = self.archive('repeated-read', self.answer)
        alias = self.root/'repeated-read-alias.a'
        alias.symlink_to(library)
        # The first archive occurs before its consumer; the later occurrence
        # must still extract its member, including through a canonical alias.
        for later in (library, alias):
            out = self.root/'repeated-read-output'
            result = run(LINKER, '--stats', '-o', out, library, self.simple, later)
            run(out, expected=42)
            stats = json.loads(result.stderr)['counts']
            self.assertEqual(stats['input_read_cache_hits'], 1)
            self.assertEqual(stats['input_files'], 3)
            self.assertEqual(stats['input_bytes'],
                             2*library.stat().st_size+self.simple.stat().st_size)
        # Each occurrence also owns independent whole-archive member state.
        self.rejected(self.simple, 'duplicate symbol', library, library,
                      options=('--whole-archive',))
        out = self.link('repeated-read-muldefs', self.simple, library, library,
                        options=('--whole-archive', '-z', 'muldefs'))
        run(out, expected=42)
        # Reusing bytes cannot bypass the 256 MiB logical input budget.
        padded = self.root/'repeated-read-padded.o'
        padded.write_bytes(self.answer.read_bytes())
        with padded.open('r+b') as stream:
            stream.truncate(32*1024*1024)
        self.rejected(padded, 'remaining input budget', *([padded]*8))


if __name__ == "__main__":
    unittest.main(verbosity=2)
