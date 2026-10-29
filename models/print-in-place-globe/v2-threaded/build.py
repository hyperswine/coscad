"""Rebuild/slice the revision locally. Never connects to or starts a printer.

Meshes render with the native compiler (`coscad stl`); no OpenSCAD is needed.
The compiler is `--coscad PATH`, else $COSCAD, else `cabal list-bin exe:coscad`
(as generate.py resolves it). The native kernel tessellates circles with 50
segments; the recorded meshes were rendered by OpenSCAD at $fn=96 (generate.py
patches the .scad files), so a rebuild is not facet-identical to them.
`--render-only` stops after the meshes and verify.py, before any slicing.
"""
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
import argparse
import os
import subprocess
import sys
import tempfile

B = Path(__file__).resolve().parent
os.environ.setdefault('MPLCONFIGDIR', '/private/tmp/globe-matplotlib')


def script(name):
    subprocess.run([sys.executable, str(B/name)], check=True)


# The `main` of each rendered part, as in generate.py's outputs. generate.py
# keeps only assembly.coscad; every part is that source with another main.
PARTS = {
    'gimbal': 'gimbal', 'rotor': 'rotor', 'base': 'base',
    'support-blocker': 'support_blocker',
    'coupon-female': 'coupon_female',
    'coupon-male': '(◎ 18 2) ⊕ (ζ 2 male)',
}


def compiler(arg):
    if arg or os.environ.get('COSCAD'):
        return arg or os.environ['COSCAD']
    return subprocess.check_output(['cabal', 'list-bin', 'exe:coscad'], cwd=B.parents[2], text=True).strip()


def render(coscad, source, tmp, name):
    src = tmp/(name+'.coscad')
    src.write_text(source+'main = '+PARTS[name]+'\n')
    with (B/(name+'-render.log')).open('w') as log:
        subprocess.run([coscad, 'stl', str(src), '-o', str(tmp/(name+'.stl'))],
                       stdout=log, stderr=log, check=True)
    (B/(name+'.stl')).write_bytes((tmp/(name+'.stl')).read_bytes())
    print('Rendered', name, flush=True)


if __name__ == '__main__':
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--coscad')
    ap.add_argument('--render-only', action='store_true')
    args = ap.parse_args()
    coscad = compiler(args.coscad)
    script('generate.py')
    source = (B/'assembly.coscad').read_text().rsplit('main =', 1)[0]
    with tempfile.TemporaryDirectory(prefix='globe-v2-parts-') as tmp, \
            ThreadPoolExecutor(max_workers=3) as pool:
        list(pool.map(lambda name: render(coscad, source, Path(tmp), name), PARTS))
    script('verify.py')
    if args.render_only:
        sys.exit(0)
    script('prepare_slice.py')
    script('prepare_coupon.py')
    for folder, process, project, output in [
            ('slice', 'process.json', 'globe-project.3mf', 'globe-v2-p1s.gcode.3mf'),
            ('coupon-slice', 'process-coupon.json', 'thread-coupon-project.3mf', 'thread-coupon-p1s.gcode.3mf')]:
        (B/folder).mkdir(exist_ok=True)
        with (B/folder/'slicer.log').open('w') as log:
            subprocess.run(['/Applications/BambuStudio.app/Contents/MacOS/BambuStudio',
                            '--debug', '2', '--load-settings',
                            str(B/'presets/machine.json')+';'+str(B/'presets'/process),
                            '--load-filaments', str(B/'presets/filament.json'),
                            '--arrange', '0', '--orient', '0', '--slice', '1',
                            '--export-3mf', output,
                            '--outputdir', str(B/folder), str(B/project)],
                           check=True, stdout=log, stderr=log)
    script('verify_toolpaths.py')
    script('finalize_package.py')
    script('finalize_coupon.py')
