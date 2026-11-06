import copy,importlib.util,json,pathlib,sys
repo=pathlib.Path('/home/glonex/fas-experimental-backend/fas-linker-qualification')
base=pathlib.Path('/home/glonex/.cache/fas-linker-qualification')
sys.path.insert(0,str(repo/'src/linker/corpus'))
spec=importlib.util.spec_from_file_location('archive_pipeline',repo/'src/linker/corpus/archive_pipeline.py')
module=importlib.util.module_from_spec(spec);spec.loader.exec_module(module)
from types import SimpleNamespace
root=base/'archive-pipeline-controls-20261106';root.mkdir()
shutil=__import__('shutil');shutil.copy2(__file__,root/'controls.py')
q=module.Qualification(SimpleNamespace(output=root/'runtime',corpus=base/'popular-semantic-fixed-20261106',linker=repo/'_build/default/src/linker/bin/main.exe',lld=None))
exe=root/'libarchive_test';results=[]
valid='Tests run: 1\nTests failed: 0\nAssertions checked: 42\nAssertions failed: 0\nSkips reported: 0\n'
for name,text,status,accepted in [
 ('valid',valid,0,True),
 ('zero-tests',valid.replace('Tests run: 1','Tests run: 0'),0,False),
 ('zero-assertions',valid.replace('Assertions checked: 42','Assertions checked: 0'),0,False),
 ('failed-test',valid.replace('Tests failed: 0','Tests failed: 1'),0,False),
 ('failed-assertion',valid.replace('Assertions failed: 0','Assertions failed: 1'),0,False),
 ('skip',valid.replace('Skips reported: 0','Skips reported: 1'),0,False),
 ('missing-summary','',0,False),
 ('duplicate-summary',valid+valid,0,False),
 ('nonzero-exit',valid,1,False)]:
 exe.write_text('#!/usr/bin/python3\nprint('+repr(text)+')\nraise SystemExit('+str(status)+')\n');exe.chmod(0o755)
 try:
  detail=q.check([([exe],0,None)],root);actual=True
 except (ValueError,RuntimeError) as e:
  detail=str(e);actual=False
 row={'name':name,'accepted':actual,'expected_accepted':accepted,'detail':detail};results.append(row)
 assert actual==accepted,row
# A copied report with a changed CRT identity must fail before any project links.
q.prior=copy.deepcopy(q.prior);p=next(iter(q.prior['toolchain_inputs']));q.prior['toolchain_inputs'][p]='0'*64
try:
 q.qualify(['lua'])
 raise AssertionError('changed CRT accepted')
except ValueError as e:
 assert 'changed toolchain input' in str(e),e
 results.append({'name':'changed-crt','accepted':False,'expected_accepted':False,'detail':str(e)})
report={'status':'passed','scope':'actual archive_pipeline.Qualification guard controls using synthetic exit-0 summaries and a copied corpus report; no upstream runtime claims','controls':results}
(root/'report.json').write_text(json.dumps(report,indent=2)+'\n');print('PASSED',len(results),'guard controls')
