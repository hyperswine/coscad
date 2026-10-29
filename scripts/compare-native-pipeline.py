#!/usr/bin/env python3
"""Compare native check/next on tracked assembly fixtures with recorded outcomes.

The compiler has one geometry backend, so instead of a second live run the
expected outcomes come from a recorded comparison receipt (default: the
2026-10-26 native-against-OpenSCAD receipt, whose OpenSCAD side is the
independent oracle; `--side native` compares with its native side). The
receipt is read, never written. Runs in temporary copies; never rewrites the
source examples or dispatches a print. A matching verdict is evidence for
these fixtures, not arbitrary CAD.
"""
import argparse
import hashlib
import json
import math
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
import time


ROOT = Path(__file__).resolve().parent.parent
BASELINE = ROOT / 'docs/evidence/2026-10-26-fea-backend/native-pipeline-comparison.json'
# Recorded manifests name the native kernel native-experimental.
NATIVE = {'native', 'native-experimental'}
CASES = [
    'examples/assemble/constraints/frame-stand.assemble',
    'examples/assemble/bow3/bow3.assemble',
    'examples/assemble/corner.assemble',
    'examples/assemble/ball/ball.assemble',
    'examples/assemble/tesseract/fit-test.assemble',
]


def mesh_metrics(path):
    values = [tuple(map(float, m)) for m in re.findall(
        r'^\s*vertex\s+(\S+)\s+(\S+)\s+(\S+)\s*$', path.read_text(), re.M)]
    if not values or len(values) % 3 or not all(math.isfinite(x) for p in values for x in p):
        raise ValueError(f'invalid mesh: {path}')
    volume = 0.
    for a, b, c in zip(values[::3], values[1::3], values[2::3]):
        volume += (a[0]*(b[1]*c[2]-b[2]*c[1]) + a[1]*(b[2]*c[0]-b[0]*c[2])
                   + a[2]*(b[0]*c[1]-b[1]*c[0]))/6
    return {'volume_mm3': abs(volume),
            'bounds_mm': [[min(p[d] for p in values) for d in range(3)],
                          [max(p[d] for p in values) for d in range(3)]]}


def close_mesh(a, b):
    return (abs(a['volume_mm3']-b['volume_mm3']) <= max(.01, .001*b['volume_mm3'])
            and all(abs(x-y) <= .01 for av, bv in zip(a['bounds_mm'], b['bounds_mm'])
                    for x, y in zip(av, bv)))


def run(exe, command, source, root):
    start = time.monotonic()
    process = subprocess.run([str(exe), command, source],
                             cwd=root, text=True, capture_output=True, timeout=240)
    record = {'exit_code': process.returncode, 'seconds': time.monotonic()-start,
              'stdout': process.stdout, 'stderr': process.stderr}
    if command == 'check':
        record['verdicts'] = sorted(re.findall(r'^(OVERLAP|WARN|contact)\s+(\S+) x (\S+)',
                                             process.stdout, re.M))
    elif process.returncode == 0:
        base = Path(source).with_suffix('')
        manifest = json.loads((root / (str(base)+'_manifest.json')).read_text())
        record['geometry_backend'] = manifest['geometry_backend']
        record['variants'] = {v['id']: {
            'part': v['part'], 'down': v['down'], 'footprint': v['footprint'],
            'warnings': v['printability']['warning_codes'], 'mesh': mesh_metrics(root/v['mesh'])}
            for v in manifest['variants']}
        record['placements'] = [b['placements'] for b in manifest['beds']]
        record['beds'] = [mesh_metrics(root/b['stl']) for b in manifest['beds']]
    return record


def matches(command, native, expected):
    passed = native['exit_code'] == expected['exit_code'] and native['exit_code'] in (0, 2)
    if command == 'check':
        return passed and native['verdicts'] == [tuple(v) for v in expected['verdicts']]
    passed &= native['exit_code'] == 0
    if not passed:
        return False
    if expected.get('geometry_backend') in NATIVE and native['geometry_backend'] not in NATIVE:
        return False
    nv, ev = native['variants'], expected['variants']
    passed &= nv.keys() == ev.keys() and native['placements'] == expected['placements']
    passed &= len(native['beds']) == len(expected['beds'])
    passed &= all(close_mesh(a, b) for a, b in zip(native['beds'], expected['beds']))
    passed &= all(k in ev and nv[k]['down'] == ev[k]['down']
                  and nv[k]['warnings'] == ev[k]['warnings']
                  and close_mesh(nv[k]['mesh'], ev[k]['mesh']) for k in nv)
    return passed


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--exe', required=True, type=Path)
    parser.add_argument('--baseline', type=Path, default=BASELINE,
                        help='recorded coscad.native-pipeline-comparison.v1 receipt')
    parser.add_argument('--side', choices=['openscad', 'native'], default='openscad',
                        help='which recorded run supplies the expected outcomes')
    args = parser.parse_args()
    exe = args.exe.resolve()
    baseline = json.loads(args.baseline.read_text())
    expected = {(c['source'], c['command']): c[args.side] for c in baseline['cases']}
    recorded_sources = baseline['sources_sha256']
    files = subprocess.check_output(['git', 'ls-files', '-z', 'examples'], cwd=ROOT, text=True).split('\0')
    sources = [Path(f) for f in files if Path(f).suffix in ('.coscad', '.assemble')]
    current_sources = {str(f): hashlib.sha256((ROOT/f).read_bytes()).hexdigest() for f in sources}
    receipts = []
    with tempfile.TemporaryDirectory(prefix='coscad-native-pipeline-') as tmp:
        root = Path(tmp)
        for f in sources:
            (root/f).parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(ROOT/f, root/f)
        for source in CASES:
            # Sources in the case's folder that changed since the receipt; an
            # expectation can be stale for those, so they are reported.
            folder = Path(source).parent
            changed = sorted(f for f in set(recorded_sources) | set(current_sources)
                             if Path(f).parent == folder and recorded_sources.get(f) != current_sources.get(f))
            for command in ('check', 'next'):
                print(f'{command}: {source}', file=sys.stderr, flush=True)
                native = run(exe, command, source, root)
                want = expected[(source, command)]
                receipts.append({'source': source, 'command': command,
                                 'passed': matches(command, native, want),
                                 'sources_changed_since_baseline': changed,
                                 'native': native, 'expected': want})
    passed = all(r['passed'] for r in receipts)
    print(json.dumps({'schema': 'coscad.native-pipeline-baseline.v1', 'passed': passed,
                      'executable_sha256': hashlib.sha256(exe.read_bytes()).hexdigest(),
                      'baseline': {'path': str(args.baseline), 'side': args.side,
                                   'sha256': hashlib.sha256(args.baseline.read_bytes()).hexdigest()},
                      'sources_sha256': current_sources,
                      'criteria': 'Check verdicts and exits; next variant/placement identities, orientation, warning codes, mesh volume 0.1%, bounds 0.01mm, against the recorded run.',
                      'cases': receipts}, indent=2, allow_nan=False))
    raise SystemExit(0 if passed else 1)


if __name__ == '__main__':
    main()
