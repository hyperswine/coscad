"""Rebuild/slice the revision locally. Never connects to or starts a printer."""
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor
import os
import subprocess
import sys

B = Path(__file__).resolve().parent
os.environ.setdefault('MPLCONFIGDIR', '/private/tmp/globe-matplotlib')


def script(name):
    subprocess.run([sys.executable, str(B/name)], check=True)


def render(name):
    with (B/(name+'-render.log')).open('w') as log:
        subprocess.run(['/Applications/OpenSCAD.app/Contents/MacOS/openscad',
                        '--backend=Manifold', '-o', str(B/(name+'.stl')),
                        str(B/(name+'.scad'))], stdout=log, stderr=log, check=True)
    print('Rendered', name, flush=True)


if __name__ == '__main__':
    script('generate.py')
    with ThreadPoolExecutor(max_workers=3) as pool:
        list(pool.map(render, ['gimbal', 'rotor', 'base', 'support-blocker',
                              'coupon-female', 'coupon-male']))
    script('verify.py')
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
