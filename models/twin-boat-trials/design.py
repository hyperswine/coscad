"""Generate two small support-free CoScad hulls; no printer communication."""
from pathlib import Path
import json,subprocess
B=Path(__file__).resolve().parent
ROOT=B.parents[1]
def n(v):return f'{v:.5f}'.rstrip('0').rstrip('.') if v else '0'
def outline(length,width):
 l=length;w=width
 pts=[(-.5*l,-.30*w),(-.5*l,-.46*w),(-.45*l,-.5*w),(-.35*l,-.5*w),(-.2*l,-.5*w),(0,-.5*w),(.15*l,-.5*w),(.35*l,-.48*w),(.48*l,-.18*w),(.5*l,0),(.48*l,.18*w),(.35*l,.48*w),(.15*l,.5*w),(0,.5*w),(-.2*l,.5*w),(-.35*l,.5*w),(-.45*l,.5*w),(-.5*l,.46*w),(-.5*l,.3*w),(-.5*l,.1*w),(-.5*l,-.1*w),(-.5*l,-.3*w)]
 return '✎ '+' '.join(n(a)+' '+n(b) for a,b in pts)
def solid(rings):
 return ' ⇓ '.join(f'(ζ {n(z)} (⮕ 0.02 ({outline(l,w)})))' for z,l,w in rings)
def box(x,y,z,l,w,h):return f'χ {n(x-l/2)} (ψ {n(y-w/2)} (ζ {n(z)} (▬ {n(l)} {n(w)} {n(h)})))'
mono_outer=[(0,92,18),(3,96,22),(15,108,46),(28,116,54)]
mono_inner=[(6,87,16),(16,101,39),(28.3,110,48)]
center_outer=[(0,80,20),(10,94,34),(20,100,38)]
center_inner=[(2.4,75,14),(8,87,26),(20.3,96,34)]
pontoon_outer=[(0,90,8),(8,103,20),(12,110,22),(22,92,2)]
pontoon_inner=[(2.4,85,4),(8,98,16),(11,105,18),(20,90,.2)]
def write(name,defs,expression):
 (B/(name+'.coscad')).write_text('// Small floating boat, mm. Open cavities require no trapped support.\n'+defs+'\nmain = '+expression+'\n')
write('monohull',f'outer = {solid(mono_outer)}\nvoid = {solid(mono_inner)}','outer ⊖ void')
write('monohull-envelope','',solid(mono_outer))
write('monohull-opening','',solid([(28,110,48),(28.02,110,48)]))
center=f'center_outer = {solid(center_outer)}\ncenter_void = {solid(center_inner)}\ncenter_hull = center_outer ⊖ center_void'
pontoon=f'pontoon_outer = {solid(pontoon_outer)}\npontoon_void = {solid(pontoon_inner)}\npontoon = pontoon_outer ⊖ pontoon_void'
arms=[]
for x in [-24,24]:
 for side in [-1,1]:
  a=box(x,16*side,9,6,3,3)
  p=box(x,24*side,15,6,3,3)
  c=box(x,31*side,9,6,3,3)
  arms+=['('+a+' ⇓ '+p+')','('+p+' ⇓ '+c+')']
write('outrigger',center+'\n'+pontoon,'center_hull ⊕ (ψ -38 pontoon) ⊕ (ψ 38 pontoon) ⊕ '+' ⊕ '.join(arms))
write('outrigger-envelope','',solid(center_outer)+' ⊕ (ψ -38 ('+solid(pontoon_outer)+')) ⊕ (ψ 38 ('+solid(pontoon_outer)+'))')
# Rim surfaces locate the first possible flooding point when heeled.
write('outrigger-opening','',solid([(20,94,32),(20.02,94,32)]))
compiler=subprocess.check_output(['cabal','list-bin','exe:coscad'],cwd=ROOT,text=True).strip()
for name in ['monohull','monohull-envelope','monohull-opening','outrigger','outrigger-envelope','outrigger-opening']:
 subprocess.run([compiler,str(B/(name+'.coscad')),'-o',str(B/(name+'.scad'))],check=True)
info={'units':'mm','conventional':{'printer':'P1S','outer_stations':mono_outer,'inner_stations':mono_inner,'aim':'Open hull with thick low keel and flared sides, restoring buoyancy at moderate heel, not guaranteed recovery from full capsize'},'outrigger':{'printer':'A1','center_stations':center_outer,'pontoon_stations':pontoon_outer,'pontoon_centerlines_y':[-38,38],'aim':'Low, wide central hull with two side outriggers and arched support-free connecting arms'},'process_intent':{'material':'PLA','nozzle_mm':.4,'layer_mm':.16,'wall_loops':6,'infill':'100%','supports':False,'reason':'Solid low-keel geometry and multiple continuous wall paths; open hulls avoid internal supports. Water tightness still requires a physical leak test.'}}
(B/'design.json').write_text(json.dumps(info,indent=2)+'\n')
