#!/usr/bin/env python3
"""ASLR, dlsym and versioned dynamic exports of linker-created image addresses."""
from pathlib import Path
import shutil
import sys
import unittest

import hosted_integration as hosted

core = hosted.core
run = core.run
REENTRY = str(Path(sys.argv.pop(1)).resolve())


class SyntheticTests(unittest.TestCase):
    setUpClass = classmethod(core.LinkerTests.setUpClass.__func__)
    tearDownClass = classmethod(core.LinkerTests.tearDownClass.__func__)
    setUp = hosted.HostedTests.setUp
    assemble = classmethod(core.LinkerTests.assemble.__func__)
    compile = hosted.HostedTests.compile
    crt = hosted.HostedTests.crt
    hosted = hosted.HostedTests.hosted
    link = core.LinkerTests.link

    def fixture(self, extended):
        names = ['_end', 'literal']
        if extended:
            names += ['end', '_edata', 'edata', '__bss_start', '_etext', 'etext']
        declarations = '\n'.join('extern char ' + name + '[];' for name in names)
        table = ','.join(names)
        obj = self.compile('synthetic-' + str(extended), declarations + '''
static volatile int state;
__attribute__((constructor)) static void init(void){state=42;}
__attribute__((destructor)) static void fini(void){state=0;}
void *values[]={''' + table + '''};
void *get_value(int index){return values[index];}
#if EXTENDED
extern char __ehdr_start[], __executable_start[], _DYNAMIC[];
extern void *__init_array_start[], *__init_array_end[];
int private_layout(void){
 return __ehdr_start==__executable_start && __ehdr_start[0]==127
   && __ehdr_start[1]=='E' && __ehdr_start[2]=='L' && __ehdr_start[3]=='F'
   && _DYNAMIC!=0 && __init_array_end>__init_array_start && state==42;
}
#endif
''', flags=('-DEXTENDED=' + str(int(extended)),))
        absolute = self.assemble('synthetic-absolute', '.globl literal\n.set literal,0x1234')
        return names, obj, absolute

    def loader(self, names, version=False):
        strings = ','.join('"' + name + '"' for name in names)
        lookup = 'dlvsym(handle,names[i],"FAS_1")' if version else 'dlsym(handle,names[i])'
        return self.compile('synthetic-loader', '''
#define _GNU_SOURCE
#include <dlfcn.h>
#include <stdio.h>
#include <string.h>
int main(int argc,char **argv){
 void *handle=argc==2?dlopen(argv[1],RTLD_NOW|RTLD_LOCAL):RTLD_DEFAULT;
 if(argc==2 && !handle){puts(dlerror());return 1;}
 void *(*get_value)(int)=dlsym(handle,"get_value");
 if(!get_value)return 2;
#if EXTENDED
 int (*private_layout)(void)=dlsym(handle,"private_layout");
 if(!private_layout || !private_layout())return 6;
#endif
 const char *names[]={''' + strings + '''};
 for(unsigned i=0;i<sizeof(names)/sizeof(*names);i++){
  dlerror();void *found=''' + lookup + ''';
  const char *error=dlerror();
  if(error || found!=get_value(i)){
   fprintf(stderr,"%s: lookup=%p internal=%p error=%s\\n",names[i],found,get_value(i),error?error:"");
   return 3;
  }
  if(!strcmp(names[i],"literal") && found!=(void*)0x1234)return 4;
 }
 if(argc==2 && dlclose(handle))return 5;
 return 42;
}
''', flags=('-DEXTENDED=' + str(int(len(names) > 2)),))

    def check_symbols(self, path, names):
        data = path.read_bytes()
        symbols = {symbol['name']: symbol for symbol in core.elf_symbols(data, kind=11)}
        sections = core.elf_sections(data)
        for name in names:
            symbol = symbols[name]
            if name == 'literal':
                self.assertEqual((symbol['section'], symbol['value']), (0xfff1, 0x1234))
            else:
                self.assertGreater(symbol['section'], 0)
                self.assertLess(symbol['section'], len(sections))
                self.assertTrue(sections[symbol['section']]['flags'] & 2)
        self.assertEqual(run('readelf', '-aW', path).stderr, b'')
        if '__ehdr_start' in {s['name'] for s in core.elf_symbols(data)}:
            for name in ('__ehdr_start', '__executable_start', '_DYNAMIC',
                         '__init_array_start', '__init_array_end'):
                self.assertNotIn(name, symbols)

    def test_dso_exports_independent_loaders_hashes_versions_and_gc(self):
        names, obj, absolute = self.fixture(True)
        for style in ('sysv', 'gnu', 'both'):
            for versioned in (False, True):
                policy = self.root / 'synthetic.versions'
                policy.write_text('FAS_1 { global: *; };')
                flags = ('--version-script', policy) if versioned else ()
                library = self.link('libsynthetic.so', obj, absolute,
                    options=('-shared', '--gc-sections', '--hash-style=' + style, *flags))
                self.check_symbols(library, names)
                loader = self.loader(names, versioned)
                for tool in (core.LINKER, 'ld'):
                    host = self.hosted('synthetic-host', loader, linker=tool)
                    run(host, library, expected=42)
                if style == 'both' and versioned:
                    run(REENTRY, '-shared', '--gc-sections', '--hash-style=both',
                        '--version-script', policy, obj, absolute)

    def test_exported_pie_and_fixed_executable_addresses(self):
        names, obj, absolute = self.fixture(True)
        loader = self.loader(names)
        for pie in (False, True):
            out = self.hosted('synthetic-executable', obj, absolute, loader, pie=pie,
                              gc=True, extra=('-E', '--hash-style=both'))
            self.check_symbols(out, names)
            run(out, expected=42)

    def test_independent_producer_end_and_real_absolute_semantics(self):
        names, obj, absolute = self.fixture(False)
        loader = self.loader(names)
        host = self.hosted('synthetic-comparison', loader, linker='ld')
        tools = [core.LINKER, 'ld']
        if shutil.which('ld.lld-22'):
            tools.append('ld.lld-22')
        for tool in tools:
            library = self.link('libsynthetic-comparison.so', obj, absolute,
                                linker=tool, options=('-shared', '--gc-sections'))
            self.check_symbols(library, names)
            run(host, library, expected=42)


if __name__ == '__main__':
    unittest.main(verbosity=2)
