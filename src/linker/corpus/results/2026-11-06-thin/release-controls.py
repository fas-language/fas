import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile

root = Path('/qualification')
results = root/'results'
source = root/'source'
control = root/'controls'
control.mkdir()
package = source/'src/linker/release/package.py'
manifest = results/'release-source-manifest.json'
qualified = results/'qualification-summary.json'
binary = source/'_build/default/src/linker/bin/main.exe'
report = {'status': 'running', 'scope': 'real candidate artifact and selection controls; no timing claim', 'controls': []}
def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()
def run(args, expected=0):
    p = subprocess.run(list(map(str, args)), capture_output=True, text=True, timeout=180)
    assert p.returncode == expected, (args, p.returncode, p.stdout, p.stderr)
    return p
def build(output, qualification=qualified, manifest_path=manifest, extra=(), expected=0):
    return run([sys.executable, package, 'build', '--manifest', manifest_path,
                '--qualification', qualification, '--linker', binary, '--output', output,
                *extra], expected)
replica = root/'reproduced-release'
build(replica)
original = results/'linker-release'
for path in sorted(original.iterdir()):
    assert path.read_bytes() == (replica/path.name).read_bytes(), path
    if path.suffix == '.gz':
        run([sys.executable, package, 'verify', path])
report['archives_byte_identical'] = True
report['archive_sha256'] = {path.name: sha(path) for path in sorted(original.iterdir())}
qualification_data = json.loads(qualified.read_text())
for key, value in [('status', 'failed'), ('linker_sha256', '0'*64),
                   ('source_manifest_sha256', '0'*64), ('source_files_unchanged', False)]:
    bad = control/(key+'.json')
    bad.write_text(json.dumps({**qualification_data, key: value}))
    out = control/(key+'-output')
    p = build(out, qualification=bad, expected=1)
    assert not out.exists()
    report['controls'].append({'name': key, 'exit': p.returncode, 'stderr': p.stderr, 'no_output': True})
bad_manifest = json.loads(manifest.read_text())
bad_manifest['files']['LICENSE']['sha256'] = '0'*64
manifest_path = control/'changed-source.json'
manifest_path.write_text(json.dumps(bad_manifest))
bad_qualification = control/'changed-source-qualification.json'
bad_qualification.write_text(json.dumps({**qualification_data, 'source_manifest_sha256': sha(manifest_path)}))
p = build(control/'changed-source-output', qualification=bad_qualification,
          manifest_path=manifest_path, expected=1)
assert not (control/'changed-source-output').exists()
report['controls'].append({'name': 'source-content', 'exit': p.returncode, 'stderr': p.stderr, 'no_output': True})
p = build(control/'version-output', extra=['--version', '1.0'], expected=1)
assert not (control/'version-output').exists()
report['controls'].append({'name': 'version-relabel', 'exit': p.returncode, 'stderr': p.stderr, 'no_output': True})
for name, expected in [('complete', 0), ('wrong-count', 1), ('reported-skip', 1), ('semantic-seed', 1), ('semantic-combination', 1), ('semantic-counter', 1), ('semantic-status', 1)]:
    out = control/name
    out.mkdir()
    for file in ('linker-regressions.log', 'packages.tsv', 'release-source-manifest.json',
                 'linker-install/usr/bin/fas-ld'):
        target = out/file
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(results/file, target)
    for selection in qualification_data['reports']:
        (out/selection).mkdir()
        shutil.copy2(results/selection/'report.json', out/selection/'report.json')
    if name == 'reported-skip':
        with (out/'linker-regressions.log').open('a') as log:
            log.write("control ... skipped 'injected negative control'\n")
    if name.startswith('semantic-'):
        semantic_path = out/'linker-semantic/report.json'
        semantic = json.loads(semantic_path.read_text())
        if name == 'semantic-seed':
            semantic['cases'][0]['seed'] += 10000
        elif name == 'semantic-combination':
            semantic['cases'][0]['runs'][0] = dict(semantic['cases'][0]['runs'][1])
        elif name == 'semantic-counter':
            semantic['executions'] -= 1
        elif name == 'semantic-status':
            semantic['cases'][0]['runs'][0]['status'] = 'failed'
        semantic_path.write_text(json.dumps(semantic))
    args = [sys.executable, source/'src/linker/ci/verify.py', '--output', out]
    if name == 'wrong-count':
        args += ['--expected-python-cases', '264']
    p = run(args, expected)
    summary = json.loads((out/'qualification-summary.json').read_text())
    if name.startswith('semantic-'):
        assert summary['failures'] == ['linker-semantic report failed'], summary
    elif name == 'wrong-count':
        assert summary['failures'] == ['expected 264 Python cases, observed 265'], summary
    elif name == 'reported-skip':
        assert summary['failures'] == ['regression skips present'], summary
    else:
        assert not summary['failures'], summary
    report['controls'].append({'name': 'selection-'+name, 'exit': p.returncode,
                               'status': summary['status'], 'failures': summary['failures']})
report['qualification_sha256'] = sha(qualified)
report['source_manifest_sha256'] = sha(manifest)
report['linker_sha256'] = sha(binary)
report['status'] = 'passed'
(root/'release-controls.json').write_text(json.dumps(report, indent=2)+'\n')
print(json.dumps(report, indent=2))
