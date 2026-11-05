#!/usr/bin/env python3
"""Exercise dynamic/script/TLS linking through the OCaml API across domains."""
from pathlib import Path
import sys
import unittest

DYNAMIC_REENTRY = str(Path(sys.argv.pop(3)).resolve())
import hosted_integration as hosted

core = hosted.core
run = core.run


class LibraryTests(unittest.TestCase):
    setUpClass = hosted.HostedTests.__dict__['setUpClass']
    tearDownClass = hosted.HostedTests.__dict__['tearDownClass']
    assemble = hosted.HostedTests.__dict__['assemble']
    setUp = hosted.HostedTests.setUp
    compile = hosted.HostedTests.compile
    crt = hosted.HostedTests.crt

    def test_dynamic_tls_sysroot_and_exports_reenter_across_domains(self):
        obj = self.compile('api-reentry','__thread int n=42; int main(void){return n;}')
        for mode in (('-pie',),('--dynamic-linker',hosted.INTERPRETER)):
            result = run(DYNAMIC_REENTRY,*mode,'--export-dynamic','--gc-sections',
                         self.crt('Scrt1.o'),self.crt('crti.o'),self.crt('crtbeginS.o'),
                         obj,'-L/usr/lib/x86_64-linux-gnu','-lc',self.crt('crtendS.o'),
                         self.crt('crtn.o'))
            self.assertIn(b'domain reentry',result.stdout)

    def test_shared_versions_tls_and_preemption_reenter_across_domains(self):
        obj = self.compile('api-shared','__thread int n=7; int datum=35; int answer(void){return n+datum;}')
        script = self.root/'api-version.map'
        script.write_text('FAS_1 { global: answer; n; datum; local: *; };')
        result = run(DYNAMIC_REENTRY,'-shared','-soname','libapi.so','--version-script',script,
                     '--gc-sections',obj,hosted.INTERPRETER)
        self.assertIn(b'domain reentry',result.stdout)


if __name__ == '__main__':
    unittest.main(verbosity=2)
