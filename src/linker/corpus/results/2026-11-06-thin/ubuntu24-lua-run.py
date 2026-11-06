import hashlib,json,os,pathlib,shutil,subprocess,tarfile,uuid
repo=pathlib.Path('/home/glonex/fas-experimental-backend/fas-linker-qualification');base=pathlib.Path('/home/glonex/.cache/fas-linker-qualification');out=base/'ubuntu24-lua-archives-20261106';out.mkdir()
def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
source=out/'source/src/linker/corpus';source.mkdir(parents=True)
for name in ('run.py','popular.py','archive_pipeline.py'):shutil.copy2(repo/'src/linker/corpus'/name,source/name)
(out/'tools').mkdir();cli=base/'ubuntu24-thin-fifteen-20261106/source/_build/default/src/linker/bin/main.exe';shutil.copy2(cli,out/'tools/fas-ld')
(out/'home').mkdir();(out/'results').mkdir();(out/'inputs').mkdir()
original=base/'expanded-sources-20261105/lua';pin=json.loads((original/'source.json').read_text());archive=next(p for p in original.iterdir() if p.is_file() and p.name!='source.json' and sha(p)==pin['sha256'])
shutil.copy2(archive,out/'inputs/source.tar.gz');shutil.copy2(original/'source.json',out/'inputs/source.json')
with tarfile.open(archive) as tar:tar.extractall(out/'inputs',filter='data')
image=json.loads(subprocess.check_output(['docker','image','inspect','fas-linker-qualification:ubuntu24-ocaml5.4.1'],text=True))[0]['Id'];name='fas-lua-archive-'+uuid.uuid4().hex[:12]
report={'status':'running','scope':'fresh Ubuntu 24.04 userspace with GCC13/binutils2.42/LLD18; shared host kernel; pinned Lua rebuild and archive pipelines; no timings','image':image,'container':name,'linker_sha256':sha(out/'tools/fas-ld'),'source_archive':pin,'harness_sha256':{p.name:sha(p) for p in source.iterdir()},'commands':[]}
shutil.copy2(__file__,out/'runner.py')
def save():(out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
commands='set -e; python3 src/linker/corpus/popular.py --linker /qualification/tools/fas-ld --project lua /qualification/inputs/lua-5.1.5 --output /qualification/results/popular; python3 src/linker/corpus/archive_pipeline.py --linker /qualification/tools/fas-ld --corpus /qualification/results/popular --project lua --lld ld.lld --output /qualification/results/archives'
args=['docker','create','--name',name,'--network','none','--user',f'{os.getuid()}:{os.getgid()}','--ulimit','nofile=65536:65536','--mount',f'type=bind,src={out},dst=/qualification','--env','HOME=/qualification/home','--env','LC_ALL=C.UTF-8','--workdir','/qualification/source',image,'bash','-c',commands]
try:
 report['commands'].append(args);save();subprocess.run(args,check=True,capture_output=True)
 with (out/'qualification.log').open('wb') as log:
  args=['docker','start','--attach',name];report['commands'].append(args);save();c=subprocess.run(args,stdout=log,stderr=subprocess.STDOUT,timeout=900)
 report['exit']=c.returncode;assert c.returncode==0
 assert json.loads((out/'results/archives/report.json').read_text())['status']=='passed'
 report['status']='passed'
except BaseException as e:report['status']='failed';report['error']=str(e);raise
finally:
 subprocess.run(['docker','stop','--time','5',name],capture_output=True,timeout=30);save()
print(report['status'])
