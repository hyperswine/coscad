"""Slice each prepared bed with the Bambu Studio CLI (positions kept: no arrange, no orient)."""
from pathlib import Path
import subprocess, sys
B = Path(__file__).resolve().parent
for job in sys.argv[1:] or ['x1c-bed1', 'x1c-bed2']:
    folder = B/job; out = folder/'slice'; out.mkdir(exist_ok=True)
    with (out/'slicer.log').open('w') as log:
        subprocess.run(['/Applications/BambuStudio.app/Contents/MacOS/BambuStudio', '--debug', '2',
                        '--load-settings', str(folder/'presets/machine.json')+';'+str(folder/'presets/process.json'),
                        '--load-filaments', str(folder/'presets/filament.json'), '--arrange', '0', '--orient', '0', '--slice', '1',
                        '--export-3mf', 'tank.gcode.3mf', '--outputdir', str(out), str(folder/'tank-project.3mf')], stdout=log, stderr=log, check=True)
    print('Sliced', job, flush=True)
