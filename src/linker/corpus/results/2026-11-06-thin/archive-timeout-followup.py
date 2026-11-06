import hashlib,json,os,pathlib,shutil,subprocess,time
root=pathlib.Path('/home/glonex/.cache/fas-linker-qualification');prior=root/'fuzz-thin-fifteen-20261106';triage=json.loads((root/'archive-timeout-triage-20261106.json').read_text());out=root/'archive-timeout-followup-20261106';out.mkdir();shutil.copy2(__file__,out/'followup.py')
seeds=out/'inputs';seeds.mkdir();p=pathlib.Path(triage['findings'][0]['path']);shutil.copy2(p,seeds/'saved-timeout.a')
target=prior/'target.exe';report={'target':str(target),'target_sha256':hashlib.sha256(target.read_bytes()).hexdigest(),'input_sha256':hashlib.sha256(p.read_bytes()).hexdigest(),'direct_replays':[]}
for i in range(1000):
 t=time.monotonic();c=subprocess.run([str(target),'archive',str(p)],capture_output=True,timeout=2)
 report['direct_replays'].append({'exit':c.returncode,'elapsed_seconds':time.monotonic()-t})
 assert c.returncode==0,(i,c.returncode,c.stderr)
(out/'report.json').write_text(json.dumps(report,indent=2)+'\n')
env=dict(os.environ,AFL_NO_UI='1',AFL_SKIP_CPUFREQ='1',AFL_NO_AFFINITY='1',AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES='1',AFL_CRASH_EXITCODE='2')
args=['nice','-n','10','afl-fuzz','-i',str(seeds),'-o',str(out/'campaign'),'-m','none','-t','2000','-V','600','--',str(target),'archive','@@'];report['command']=args;report['afl_environment']={k:v for k,v in env.items() if k.startswith('AFL_')}
with (out/'campaign.log').open('wb') as log:
 c=subprocess.run(args,env=env,stdout=log,stderr=subprocess.STDOUT)
report['returncode']=c.returncode;stats=out/'campaign/default/fuzzer_stats'
if stats.exists():report['stats']={k.strip():v.strip() for k,v in (s.split(':',1) for s in stats.read_text().splitlines())}
report['classification']='not-reproduced' if c.returncode==0 and all(int(report['stats'][k])==0 for k in ('saved_crashes','saved_hangs')) else 'requires-further-triage'
(out/'report.json').write_text(json.dumps(report,indent=2)+'\n');print(report['classification'],report.get('stats',{}).get('execs_done'))
