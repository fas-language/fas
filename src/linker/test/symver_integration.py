#!/usr/bin/env python3
"""Multiple version aliases: independent linkers, runtime lookup and archives."""
import unittest
import shutil

import hosted_integration as hosted

core=hosted.core
run=core.run
LINKER=core.LINKER


class SymverTests(unittest.TestCase):
    setUpClass=hosted.HostedTests.__dict__['setUpClass']
    tearDownClass=hosted.HostedTests.__dict__['tearDownClass']
    assemble=hosted.HostedTests.__dict__['assemble']
    setUp=hosted.HostedTests.setUp
    link=core.LinkerTests.link
    rejected=core.LinkerTests.rejected
    compile=hosted.HostedTests.compile
    crt=hosted.HostedTests.crt
    hosted=hosted.HostedTests.hosted

    def script(self):
        script=self.root/'aliases.map'
        script.write_text('FAS_1 {}; FAS_2 { global: answer; invoke; slot; local: *; } FAS_1;')
        return script

    def produce(self,name,*objects,linker=LINKER,extra=()):
        return self.link('lib'+name+'.so',*objects,linker=linker,
                         options=('-shared','-soname','lib'+name+'.so','--gc-sections',
                                  '--version-script='+str(self.script()),*extra))

    def test_old_default_versions_self_references_and_independent_consumers(self):
        obj=self.compile('aliases',r'''
int old_impl(void){return 35;}
int new_impl(void){return 42;}
__asm__(".symver old_impl,answer@FAS_1");
__asm__(".symver new_impl,answer@@FAS_2");
extern int answer(void),old_reference(void);
__asm__(".symver old_reference,answer@FAS_1");
int invoke_impl(void){return answer()+old_reference();}
__asm__(".symver invoke_impl,invoke@@FAS_2");
''')
        main=self.compile('aliases-main',r'''
#define _GNU_SOURCE
#include <dlfcn.h>
extern int answer(void),old_reference(void),invoke(void);
__asm__(".symver old_reference,answer@FAS_1");
int main(void){
 if(answer()!=42 || old_reference()!=35 || invoke()!=77) return 1;
 int (*old)(void)=dlvsym(RTLD_DEFAULT,"answer","FAS_1");
 int (*current)(void)=dlvsym(RTLD_DEFAULT,"answer","FAS_2");
 if(!old || !current || old()!=35 || current()!=42) return 2;
 return dlsym(RTLD_DEFAULT,"answer")==current ? 42 : 3;
}
''')
        for producer in (LINKER,'ld'):
            library=self.produce('aliases',obj,linker=producer)
            report=run('readelf','--dyn-syms','--version-info','-W',library)
            self.assertEqual(report.stderr,b'')
            self.assertIn(b'answer@FAS_1',report.stdout)
            self.assertIn(b'answer@@FAS_2',report.stdout)
            self.assertNotIn(b'old_impl',report.stdout)
            self.assertNotIn(b'new_impl',report.stdout)
            exports=[s for s in core.elf_symbols(library.read_bytes(),kind=11) if s['name']=='answer']
            self.assertEqual(len(exports),2)
            for consumer in (LINKER,'ld'):
                out=self.hosted('aliases-main',main,library,linker=consumer,
                                extra=('-rpath','$ORIGIN',))
                run(out,expected=42)

    def test_default_alias_lazy_archive_extraction_and_removed_original(self):
        obj=self.compile('archive-default',r'''
int implementation(void){return 42;}
__asm__(".symver implementation,answer@@FAS_2,remove");
''')
        archive=self.root/'libaliases.a'
        run('ar','rcs',archive,obj)
        caller=self.compile('archive-alias-caller',r'''
extern int answer(void),explicit_reference(void);
__asm__(".symver explicit_reference,answer@FAS_2");
int invoke_impl(void){return answer()==42 && explicit_reference()==42 ? 42 : 1;}
__asm__(".symver invoke_impl,invoke@@FAS_2,remove");
''')
        main=self.compile('archive-alias-main','extern int invoke(void); int main(void){return invoke();}')
        for producer in (LINKER,'ld'):
            library=self.produce('archivealiases',caller,archive,linker=producer,
                                 extra=('--no-undefined',))
            for consumer in (LINKER,'ld'):
                out=self.hosted('archive-alias-main',main,library,linker=consumer,
                                extra=('-rpath','$ORIGIN',))
                run(out,expected=42)

    def test_nondefault_only_requires_explicit_reference(self):
        obj=self.compile('old-only',r'''
int implementation(void){return 42;}
__asm__(".symver implementation,answer@FAS_1,remove");
''')
        explicit=self.compile('old-only-main',r'''
int answer(void);
__asm__(".symver answer,answer@FAS_1");
int main(void){return answer();}
''')
        ordinary=self.compile('ordinary-main','int answer(void); int main(void){return answer();}')
        for producer in (LINKER,'ld'):
            library=self.produce('oldonly',obj,linker=producer)
            for consumer in (LINKER,'ld'):
                out=self.hosted('old-only-main',explicit,library,linker=consumer,
                                extra=('-rpath','$ORIGIN',))
                run(out,expected=42)
            self.rejected(ordinary,'undefined symbol answer',library,
                          options=('-pie','-e','main'))

    def test_default_version_alias_respects_each_reference_visibility(self):
        provider=self.compile('bound-alias-provider',r'''
int old_implementation(void){return 35;}
int implementation(void){return 42;}
__asm__(".symver old_implementation,answer@FAS_1,remove");
__asm__(".symver implementation,answer@@FAS_2,remove");
''')
        override=self.compile('bound-alias-override','int answer(void){return 7;}')
        preload=self.link('bound-alias-override.so',override,options=('-shared',))
        main=self.compile('bound-alias-main',r'''
#include <stdlib.h>
int invoke(void);
int main(int argc,char **argv){return argc==2 && invoke()==atoi(argv[1]) ? 42 : 1;}
''')
        lld=shutil.which('ld.lld-22') or shutil.which('ld.lld')
        tools=(LINKER,'ld',*((lld,) if lld else ()))
        script=self.root/'bound-alias.map'
        script.write_text('FAS_1 {}; FAS_2 { global: answer; invoke; local: *; } FAS_1;')
        for visibility in ('default','internal','hidden','protected'):
            caller=self.assemble('bound-alias-caller',
                '.text\n.globl invoke\n.type invoke,@function\n'
                'invoke: jmp answer@PLT\n'+
                ('' if visibility=='default' else '.'+visibility+' answer\n')+
                '.section .note.GNU-stack,"",@progbits\n')
            for producer in tools:
                with self.subTest(visibility=visibility,producer=producer):
                    library=self.link('libboundalias.so',provider,caller,linker=producer,
                        options=('-shared','-soname','libboundalias.so','--gc-sections',
                                 '--version-script='+str(script)))
                    exports=[s for s in core.elf_symbols(library.read_bytes(),kind=11)
                             if s['name']=='answer']
                    self.assertEqual(sorted(s['other'] & 3 for s in exports),
                        [0] if visibility in ('internal','hidden') else
                        [0,3] if visibility=='protected' else [0,0])
                    names=run('readelf','--dyn-syms','-W',library).stdout
                    self.assertIn(b'answer@FAS_1',names)
                    self.assertEqual(b'answer@@FAS_2' in names,
                                     visibility not in ('internal','hidden'))
                    for consumer in tools:
                        out=self.hosted('bound-alias-main',main,library,linker=consumer,
                                        extra=('-rpath','$ORIGIN',))
                        run(out,'42',expected=42)
                        run('env','LD_PRELOAD='+str(preload),out,
                            '7' if visibility=='default' else '42',expected=42)

    def test_versioned_tls_and_ifunc_aliases(self):
        obj=self.compile('alias-kinds',r'''
__thread int old_slot=35,new_slot=42;
__asm__(".symver old_slot,slot@FAS_1,remove");
__asm__(".symver new_slot,slot@@FAS_2,remove");
static int impl1(void){return 35;}
static int impl2(void){return 42;}
static void *resolve1(void){return impl1;}
static void *resolve2(void){return impl2;}
int old_answer(void) __attribute__((ifunc("resolve1")));
int new_answer(void) __attribute__((ifunc("resolve2")));
__asm__(".symver old_answer,answer@FAS_1,remove");
__asm__(".symver new_answer,answer@@FAS_2,remove");
''')
        source=r'''
#include <pthread.h>
extern __thread int slot,old_slot;
int answer(void),old_answer(void);
__asm__(".symver old_slot,slot@FAS_1");
__asm__(".symver old_answer,answer@FAS_1");
static void *worker(void *unused){
 if(slot!=42 || old_slot!=35 || answer()!=42 || old_answer()!=35) return (void*)1;
 slot=99; old_slot=100; return 0;
}
int main(void){
 pthread_t thread; void *result;
 if(pthread_create(&thread,0,worker,0) || pthread_join(thread,&result) || result) return 1;
 return slot==42 && old_slot==35 && answer()==42 && old_answer()==35 ? 42 : 2;
}
'''
        for producer in (LINKER,'ld'):
            library=self.produce('aliaskinds',obj,linker=producer)
            for dialect in ('gnu','gnu2'):
                main=self.compile('alias-kinds-main',source,flags=('-mtls-dialect='+dialect,))
                for consumer in (LINKER,'ld'):
                    out=self.hosted('alias-kinds-main',main,library,hosted.INTERPRETER,
                                    linker=consumer,extra=('-rpath','$ORIGIN',))
                    run(out,expected=42)

    def test_missing_nodes_duplicate_defaults_and_undefined_versioned_symbols(self):
        obj=self.compile('missing-node',r'''
int implementation(void){return 42;}
__asm__(".symver implementation,answer@@MISSING,remove");
''')
        self.rejected(obj,'missing version node MISSING',options=('-shared',))
        self.rejected(obj,'requires shared output')
        other=self.compile('default-one','int first(void){return 1;} __asm__(".symver first,answer@@FAS_1,remove");')
        duplicate=self.compile('default-two','int second(void){return 2;} __asm__(".symver second,answer@@FAS_2,remove");')
        self.rejected(other,'duplicate symbol answer',duplicate,
                      options=('-shared','--version-script='+str(self.script())))
        missing=self.compile('missing-provider',r'''
int external(void); __asm__(".symver external,external@MISSING");
int answer(void){return external();}
''')
        self.rejected(missing,'undefined versioned symbol',options=('-shared',))
        base=self.compile('base-alias','int original(void){return 1;} __asm__(".symver original,answer@,remove");')
        self.rejected(base,'empty/base symbol version aliases',options=('-shared',))

    def test_alias_export_policy_matches_lld_exact_and_wildcard_rules(self):
        obj=self.compile('alias-policy',r'''
int old_impl(void){return 35;} int new_impl(void){return 42;}
__asm__(".symver old_impl,answer@FAS_1");
__asm__(".symver new_impl,answer@@FAS_2");
''')
        lld=shutil.which('ld.lld') or shutil.which('ld.lld-22')
        scripts=(('FAS_1 {}; FAS_2 { local: *; } FAS_1;',2),
                 ('FAS_1 { local: answer; }; FAS_2 {} FAS_1;',0),
                 ('FAS_1 { local: old_impl; new_impl; }; FAS_2 {} FAS_1;',2))
        for text,count in scripts:
            script=self.root/'alias-policy.map'; script.write_text(text)
            for producer in (LINKER,*((lld,) if lld else ())):
                library=self.link('alias-policy.so',obj,linker=producer,
                                  options=('-shared','--version-script='+str(script)))
                exports=[s for s in core.elf_symbols(library.read_bytes(),kind=11) if s['name']=='answer']
                self.assertEqual(len(exports),count)


if __name__ == '__main__':
    unittest.main(verbosity=2)
