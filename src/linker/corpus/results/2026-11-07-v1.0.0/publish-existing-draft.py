"""Finish the uploaded draft through its verified numeric release identity."""
from pathlib import Path
import datetime,importlib.util,json,subprocess,sys

ROOT=Path(__file__).resolve().parent
spec=importlib.util.spec_from_file_location('original_publication',ROOT/'publish.py')
original=importlib.util.module_from_spec(spec);spec.loader.exec_module(original)
api,run,sha,verify_assets=original.api,original.run,original.sha,original.verify_assets
REPO,HEAD,TAG,WORKTREE=original.REPO,original.HEAD,original.TAG,original.WORKTREE
DRAFT_ID=437650551

def main():
    q=json.loads((ROOT/'fas-ld-1.0.0-qualification.json').read_text())
    assert q['status']=='accepted' and q['source_commit']==HEAD
    assert run(['git','-C',str(WORKTREE),'rev-parse','HEAD']).strip()==HEAD
    assert not run(['git','-C',str(WORKTREE),'status','--porcelain']).strip()
    out=ROOT/'publication';staged={p.name:p for p in sorted(out.iterdir()) if p.is_file()}
    assert len(staged)==17
    for name in ('SHA256SUMS','EVIDENCE_SHA256SUMS'):
        subprocess.run(['sha256sum','-c',name],cwd=out,check=True,stdout=subprocess.DEVNULL)
    latest_before=api('releases/latest')['tag_name'];assert latest_before!=TAG
    draft=api(f'releases/{DRAFT_ID}')
    assert draft['id']==DRAFT_ID and draft['draft'] and not draft['prerelease']
    assert draft['tag_name']==TAG and draft['target_commitish']==HEAD
    assert draft['name']=='fas-ld 1.0.0 (Linux x86-64)'
    assert draft['body'].strip()==(out/'RELEASE_NOTES.md').read_text().strip()
    assets=verify_assets(draft,staged)
    (ROOT/'release-draft.json').write_text(json.dumps(draft,indent=2)+'\n')
    print('Numeric draft identity and all 17 server asset digests verified; publishing',flush=True)
    request=ROOT/'publish-request.json'
    request.write_text(json.dumps({'draft':False,'make_latest':'false'})+'\n')
    response=json.loads(run(['gh','api','--method','PATCH','repos/'+REPO+f'/releases/{DRAFT_ID}',
                             '--input',str(request)]))
    assert response['id']==DRAFT_ID and not response['draft']
    release=api(f'releases/{DRAFT_ID}')
    assert release['id']==DRAFT_ID and release['tag_name']==TAG
    assert not release['draft'] and not release['prerelease']
    assert release['target_commitish']==HEAD
    assert api('releases/tags/'+TAG)['id']==DRAFT_ID
    (ROOT/'release-published.json').write_text(json.dumps(release,indent=2)+'\n')
    verify_assets(release,staged)
    ref=api('git/ref/tags/'+TAG)['object']
    if ref['type']=='tag':ref=api('git/tags/'+ref['sha'])['object']
    assert ref['type']=='commit' and ref['sha']==HEAD
    latest_after=api('releases/latest')['tag_name'];assert latest_after==latest_before
    delivered=ROOT/'published-download';delivered.mkdir()
    downloads=[*q['archives'],'SHA256SUMS','EVIDENCE_SHA256SUMS','fas-ld-1.0.0-qualification.json']
    subprocess.run(['gh','release','download',TAG,'--repo',REPO,'--dir',str(delivered),
                    *sum((['--pattern',n] for n in downloads),[])],check=True)
    for name in downloads:assert sha(delivered/name)==sha(out/name),name
    subprocess.run(['sha256sum','-c','SHA256SUMS'],cwd=delivered,check=True)
    for name in q['archives']:
        subprocess.run([sys.executable,str(WORKTREE/'src/linker/release/package.py'),
                        'verify',str(delivered/name)],check=True,stdout=subprocess.DEVNULL)
    result={'status':'passed','observed_at':datetime.datetime.now(datetime.timezone.utc).isoformat(),
            'version':'1.0.0','tag':TAG,'source_commit':HEAD,'url':release['html_url'],
            'published_at':release['published_at'],'draft':False,'assets':assets,
            'language_latest_before':latest_before,'language_latest_after':latest_after,
            'published_downloads':downloads,'downloaded_bytes_match_staged_assets':True,
            'downloaded_archive_integrity_passed':True,
            'publication_flow':{'draft_id':DRAFT_ID,'recovery_script_sha256':sha(Path(__file__)),
                                'initial_error':'Unpublished draft is not addressable by tag through REST; verified numeric release ID used'},
            'scope':'SHA-256 integrity and source identity; no signature claim'}
    (ROOT/'publication-verification.json').write_text(json.dumps(result,indent=2)+'\n')
    print('Published and independently downloaded release verified: '+release['html_url'],flush=True)

if __name__=='__main__':main()
