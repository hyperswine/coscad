from pathlib import Path
import subprocess, sys
B = Path(__file__).resolve().parent; NAME = 'beach-outrigger'
jobs = sys.argv[1:] or ['p1s-pla', 'p1s-petg', 'a1-pla', 'a1-petg']
for job in jobs:
    folder = B/job; out = folder/'slice'; out.mkdir(exist_ok=True)
    with (out/'slicer.log').open('w') as log:
        subprocess.run(['/Applications/BambuStudio.app/Contents/MacOS/BambuStudio','--debug','2','--load-settings',str(folder/'presets/machine.json')+';'+str(folder/'presets/process.json'),'--load-filaments',str(folder/'presets/filament.json'),'--arrange','0','--orient','0','--slice','1','--export-3mf',NAME+'.gcode.3mf','--outputdir',str(out),str(folder/(NAME+'-project.3mf'))],stdout=log,stderr=log,check=True)
    print('Sliced', job, flush=True)
