from pathlib import Path
import subprocess
B=Path(__file__).resolve().parent
for name in ['monohull','outrigger']:
 folder=B/name;out=folder/'slice';out.mkdir(exist_ok=True)
 with (out/'slicer.log').open('w') as log:
  subprocess.run(['/Applications/BambuStudio.app/Contents/MacOS/BambuStudio','--debug','2','--load-settings',str(folder/'presets/machine.json')+';'+str(folder/'presets/process.json'),'--load-filaments',str(folder/'presets/filament.json'),'--arrange','0','--orient','0','--slice','1','--export-3mf',name+'.gcode.3mf','--outputdir',str(out),str(folder/(name+'-project.3mf'))],stdout=log,stderr=log,check=True)
 print('Sliced',name,flush=True)
