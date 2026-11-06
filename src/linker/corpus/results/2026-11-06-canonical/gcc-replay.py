from pathlib import Path
import hashlib,json,subprocess
root=Path('/qualification');base=root/'canonical-version'
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
linkers={'before':root/'source/_build/default/src/linker/bin/main.exe','fas':base/'source/_build/default/src/linker/bin/main.exe','gnu':None}
rows=[]
for tag,linker in linkers.items():
 tool=base/('tools-'+tag);tool.mkdir(exist_ok=True)
 if linker:
  wrapper=tool/'ld';wrapper.write_text('#!/bin/sh\nexec '+str(linker)+' "$@"\n');wrapper.chmod(0o755)
 for obj in sorted((root/'gcc-direct-imports').glob('*.o')):
  out=base/(tag+'-'+obj.stem)
  command=['gcc','-fno-use-linker-plugin','-no-pie']
  if linker:command+=['-B'+str(tool)+'/']
  command += [str(obj),'-lm','-o',str(out)]
  p=subprocess.run(command,capture_output=True,text=True,timeout=30)
  row={'tool':tag,'object':obj.name,'object_sha256':sha(obj),'argv':command,'link_exit':p.returncode,'stderr':p.stderr}
  if p.returncode==0:
   run=subprocess.run([str(out)],capture_output=True,text=True,timeout=30)
   row.update(execution_exit=run.returncode,output_sha256=sha(out))
  rows.append(row)
  print(tag,obj.stem[:12],'link',p.returncode,'run',row.get('execution_exit'),flush=True)
report={'scope':'exact retained GCC object replay in the pinned GCC 15.2.0 image; not the complete corpus','image':'gcc:15.2.0@sha256:3ae15afe768b06d0c0fe088d822ba5f8045c26630bdacc8d8e7713cf5d8e7289','crt_sha256':sha(Path('/usr/lib/x86_64-linux-gnu/crt1.o')),'linker_sha256':{k:sha(v) for k,v in linkers.items() if v},'runs':rows}
(base/'replay.json').write_text(json.dumps(report,indent=2)+'\n')
assert len(rows)==18
assert sum(r['link_exit']!=0 for r in rows if r['tool']=='before')==5
assert all(r['link_exit']==0 and r['execution_exit']==0 for r in rows if r['tool']!='before')
print('PASS: six identical objects execute through Fas and GNU; five reproduce the old failure')
