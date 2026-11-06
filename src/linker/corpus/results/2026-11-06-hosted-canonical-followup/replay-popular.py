"""Relink retained real-program inputs and require byte equality to executed images."""
import hashlib,json,shutil,subprocess
from pathlib import Path
root=Path('/home/glonex/.cache/fas-linker-qualification')
corpus=root/'popular-semantic-fixed-20261106'
repo=Path('/home/glonex/fas-experimental-backend/fas-linker-qualification')
out=root/'hosted-canonical-20261106/popular-byte-replay'
def sha(p): return hashlib.sha256(p.read_bytes()).hexdigest()
def load(p): return json.loads(p.read_text())
data=load(corpus/'report.json')
assert len(data['tests'])==6 and all(t['status']=='passed' for t in data['tests'])
for path,digest in data['toolchain_inputs'].items():
 assert sha(Path(path))==digest, path
out.mkdir(exist_ok=False)
shutil.copy2(repo/'_build/default/src/linker/bin/main.exe',out/'fas-ld')
shutil.copy2(Path(__file__),out/'replay.py')
report={'status':'running','scope':'new linker outputs byte-identical to ELF images executed in the retained upstream suite; no new upstream runtime campaign or timing comparison',
        'runtime_corpus':str(corpus),'runtime_report_sha256':sha(corpus/'report.json'),
        'runtime_linker_sha256':data['linker_sha256'],'linker_sha256':sha(out/'fas-ld'),
        'harness_sha256':sha(Path(__file__)),'toolchain_inputs_unchanged':True,'projects':{}}
def save(): (out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
save()
try:
 for name,detail in data['popular_qualification'].items():
  project=corpus/name;rows={}
  for line in (project/'linker-invocations.jsonl').read_text().splitlines():
   row=json.loads(line)
   if row['status']==0 and row['tool']==data['linker_snapshot'] and row['output']:
    rows[row['output']]=row
  destination=out/name;destination.mkdir()
  results=[];report['projects'][name]=results
  for index,oracle in enumerate(detail['oracle_outputs']):
   row=rows[oracle['path']]
   for original,digest in row['input_sha256'].items():
    assert sha(Path(row['input_replay'][original]))==digest, original
   previous=project/'fas-outputs'/Path(oracle['path']).relative_to(project)
   assert sha(previous)==oracle['fas_sha256'], previous
   output=destination/(str(index)+'-'+Path(row['output']).name)
   args=list(row['replay']);args[args.index('-o')+1]=str(output)
   process=subprocess.run([str(out/'fas-ld'),*args],cwd=row['cwd'],capture_output=True,timeout=120)
   result={'original_output':row['output'],'output':str(output),'argv':args,
           'exit':process.returncode,'stderr':process.stderr.decode(errors='replace'),
           'expected_sha256':oracle['fas_sha256'],'output_sha256':sha(output) if output.exists() else None}
   results.append(result);save()
   assert process.returncode==0 and result['output_sha256']==result['expected_sha256'], result
  print('PASSED',name,len(results),'byte-identical ELF outputs',flush=True)
 report['status']='passed'
except BaseException as error:
 report['status']='failed';report['error']=str(error);raise
finally: save()
