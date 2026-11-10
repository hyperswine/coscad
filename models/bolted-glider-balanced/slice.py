"""Slice each prepared bed with the Bambu Studio CLI (positions kept: no arrange, no orient)."""
from pathlib import Path
import subprocess, sys, json
from provenance import require_geometry, require, snapshot
B = Path(__file__).resolve().parent
for job in sys.argv[1:] or ['a1-wing-right', 'p1s-wing-left', 'x1c-fuselage', 'a1-tail']:
    require_geometry(B)
    folder = B/job
    inputs=json.loads((folder/'prepared-inputs.json').read_text()); require(B,inputs)
    out = folder/'slice'; out.mkdir(exist_ok=True)
    with (out/'slicer.log').open('w') as log:
        subprocess.run(['/Applications/BambuStudio.app/Contents/MacOS/BambuStudio', '--debug', '2',
                        '--load-settings', str(folder/'presets/machine.json')+';'+str(folder/'presets/process.json'),
                        '--load-filaments', str(folder/'presets/filament.json'), '--arrange', '0', '--orient', '0', '--slice', '1',
                        '--export-3mf', 'glider.gcode.3mf', '--outputdir', str(out), str(folder/'glider-project.3mf')], stdout=log, stderr=log, check=True)
    inputs.update(snapshot(B,[folder/'slice/plate_1.gcode', B/'slice.py']))
    (folder/'slice/input-hashes.json').write_text(json.dumps(inputs,indent=2)+'\n')
    print('Sliced', job, flush=True)
