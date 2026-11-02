from pathlib import Path
import os,json
os.environ['MPLCONFIGDIR']='/private/tmp/boat-review-matplotlib'
src=Path(__file__).resolve().parents[1]/'verify.py'
ns={'__file__':str(src)}
exec(compile(src.read_text().split("report = {'assumptions':")[0],str(src),'exec'),ns)
from scipy.optimize import root
floating=ns['floating'];env=ns['env'];sealed=ns['sealed'];chambers=ns['chambers']
out={'method':'Review extension of existing hydrostatic functions: simultaneous heel/trim equilibrium, not an independent fluid solver. No original files rewritten.','dry':{},'swamped':{},'one_chamber_flooded':[]}
for name,displacer in [('dry',env),('swamped',sealed)]:
 for water in ['fresh','sea']:
  s=floating(displacer,'PLA',water);out[name][water]=s
for c in chambers:
 def residual(angles):
  s=floating(sealed,'PLA','fresh',heel=angles[0],pitch=angles[1],flooded=c['mesh'])
  return [s['righting_arm_mm'],s['trim_arm_mm']]
 sol=root(residual,[0,0],tol=1e-5)
 s=floating(sealed,'PLA','fresh',heel=sol.x[0],pitch=sol.x[1],flooded=c['mesh'])
 out['one_chamber_flooded'].append({'chamber':c['name'],'converged':bool(sol.success),'heel_deg':float(sol.x[0]),'pitch_bow_up_deg':float(sol.x[1]),**s})
out['min_wall_to_outside_mm']=min(c['min_wall_to_outside_mm'] for c in chambers)
out['min_wall_to_cockpit_mm']=min(c['min_wall_to_cockpit_mm'] for c in chambers)
out['chambers']=len(chambers)
(Path(__file__).resolve().parent/'flooded-equilibrium.json').write_text(json.dumps(out,indent=2)+'\n');print(json.dumps(out,indent=2))
