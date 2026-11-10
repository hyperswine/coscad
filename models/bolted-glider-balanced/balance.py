"""Zero-ballast design gate from sliced extrusion moments and explicit hardware.
Optional --measured JSON maps part or hardware names to measured grams.
Mass estimates include removable ears; uncertainty includes their removal. Never adds ballast.
"""
from pathlib import Path
import argparse, hashlib, itertools, json, math
import numpy as np
from assembly import B, d, W, T, PLACE, WING_TO_USE, TAIL_TO_USE
from toolpath_mass import read_job
from provenance import require_geometry, require_slice, require
require_geometry(B)

parser=argparse.ArgumentParser(); parser.add_argument('--measured',type=Path); args=parser.parse_args()
measured=json.loads(args.measured.read_text()) if args.measured else {}
validation=json.loads((B/'validation.json').read_text()); require(B,validation['inputs'])
a=validation['aero']
items=[]; sources={}
for job in ('a1-wing-right','p1s-wing-left','x1c-fuselage','a1-tail'):
    require_slice(B,job)
    path=B/job/'slice/plate_1.gcode'; sources[job]=hashlib.sha256(path.read_bytes()).hexdigest()
    deposited=read_job(B,job)
    sliced=json.loads((B/job/'slice/result.json').read_text())['sliced_plates'][0]['filaments'][0]['total_used_g']
    assert abs(sum(row['mass_g'] for row in deposited.values())-sliced)/sliced < .02, 'extrusion mass disagrees with slicer'
    for part,row in deposited.items():
        c=(PLACE[part]@np.r_[row['print_cg_mm'],1])[:3]
        items.append({'name':part,'mass_g':row['mass_g'],'cg_mm':c.tolist(),'kind':'printed'})
# Approximate steel button-head screw masses, including a separate nut for every screw.
# Screw CG is 2 mm below its head bearing face; nut CG is at the cavity midplane.
for side in (1,-1):
    for i,u in enumerate(W['screw_u']):
        for kind,mass,z in [('M5x12',2.6,W['counterbore_floor']-2),('M5-nut',1.2,-5)]:
            c=(WING_TO_USE@[W['root_gap']+W['screw_station'],u*W['root_chord'],z,1])[:3]; c[0]*=side
            items.append({'name':f'wing-{side}-{i}-{kind}','mass_g':mass,'cg_mm':c.tolist(),'kind':'hardware'})
for i,(x,dy) in enumerate(T['screws']):
    for kind,mass,z in [('M3x10',0.8,T['platform_top_z']+T['stab_block_top']+T['fin']['foot'][2]-2),('M3-nut',0.4,T['platform_top_z']-T['platform_thickness']/2)]:
        items.append({'name':f'tail-{i}-{kind}','mass_g':mass,'cg_mm':(TAIL_TO_USE@[x,T['screw_y_ref']+dy,z,1])[:3].tolist(),'kind':'hardware'})
assert set(measured)<= {i['name'] for i in items}, 'unknown measured mass name'
for i in items:
    if i['name'] in measured: i['mass_g']=float(measured[i['name']])
    assert math.isfinite(i['mass_g']) and i['mass_g']>0
m=sum(i['mass_g'] for i in items); cg=sum(i['mass_g']*np.array(i['cg_mm']) for i in items)/m
mac=a['mac_mm']; np_y=a['neutral_point_y']; sm=(np_y-cg[1])/mac
# Exact extrema of the mass-weighted CG over independent mass intervals.
# At an extremum, heavy items on one side and light items on the other maximize
# the signed moment about the candidate CG. Bisection locates its zero.
# +/-5% printed mass, +/-20% EACH fastener, +/-2 mm printed centroid error.
def extreme_cg(maximize):
    ys=[i['cg_mm'][1]+((2 if maximize else -2) if i['kind']=='printed' else 0) for i in items]
    lo,hi=min(ys),max(ys)
    for _ in range(70):
        mid=(lo+hi)/2; moment=0
        for i,y in zip(items,ys):
            error=.05 if i['kind']=='printed' else .2
            heavy=(y>mid) if maximize else (y<mid)
            moment+=i['mass_g']*(1+error if heavy else 1-error)*(y-mid)
        if moment>0: lo=mid
        else: hi=mid
    return (lo+hi)/2
limits=[extreme_cg(False),extreme_cg(True)]
cg_range=limits
# Model sensitivity, not a confidence interval: tail efficiency 0.8..1.0, downwash 0.4..0.65.
neutral=[]
for eta,de in itertools.product((.8,1.0),(.4,.65)):
    r=eta*a['stab_area_mm2']/a['wing_area_mm2']*a['lift_slope_stab_per_rad']/a['lift_slope_wing_per_rad']*(1-de)
    neutral.append((a['wing_ac_y']+r*(a['wing_ac_y']+a['tail_arm_mm']))/(1+r))
sm_range=[(min(neutral)-max(limits))/mac,(max(neutral)-min(limits))/mac]
rep={'status':'design estimate; not flight validated','items':items,'gcode_sha256':sources,
     'total_g':m,'hardware_g':sum(i['mass_g'] for i in items[5:]),'ballast_g':0,
     'cg_mm':cg.tolist(),'neutral_point_y_mm':np_y,'mac_mm':mac,'static_margin':sm,
     'target_static_margin':[.10,.15],'cg_sensitivity_y_mm':cg_range,
     'neutral_point_sensitivity_y_mm':[min(neutral),max(neutral)],'static_margin_sensitivity':sm_range,
     'wing_loading_g_dm2':m/(a['wing_area_mm2']/1e4),
     'speed_m_s_assuming_CL_0.6':math.sqrt(2*m*.001*9.81/(1.225*a['wing_area_mm2']*1e-6*.6)),
     'gates':{'nominal_margin_10_to_15_percent':.10<=sm<=.15,'sensitivity_margin_above_5_percent':sm_range[0]>=.05,'lateral_cg_within_1_mm':abs(cg[0])<1}}
# Linear trim estimate, deliberately exposing the unknown modified-airfoil polar.
# Cm=-0.06 and alpha0=-3 deg are assumptions, not measured Clark-Y data.
def trim(cm,alpha0):
    ratio=a['tail_efficiency']*a['stab_area_mm2']/a['wing_area_mm2']
    clt=(cm+(cg[1]-a['wing_ac_y'])/mac*.6)/(ratio*a['tail_arm_mm']/mac)
    clw=.6-ratio*clt
    wing_alpha_above_zero=math.degrees(clw/a['lift_slope_wing_per_rad'])
    body_alpha=wing_alpha_above_zero+alpha0-W['incidence_deg']
    downwash=wing_alpha_above_zero*a['downwash_gradient']
    return {'wing_CL':clw,'tail_CL':clt,'tail_incidence_deg':math.degrees(clt/a['lift_slope_stab_per_rad'])-body_alpha+downwash}
nominal_trim=trim(-.06,-3)
trim_cases=[trim(cm,alpha0)['tail_incidence_deg'] for cm,alpha0 in itertools.product((-.04,-.10),(-4,-2))]
rep['trim']={'assumed_total_CL':.6,'assumed_wing_Cm':-.06,'assumed_wing_alpha0_deg':-3,
             'nominal':nominal_trim,'built_tail_incidence_deg':T['incidence_deg'],
             'required_incidence_sensitivity_deg':[min(trim_cases),max(trim_cases)],
             'limitations':'Linear model only. Modified section polar, stall, fuselage moment and wing-tail interference are unmeasured.'}
rep['gates']['nominal_trim_incidence_within_1_degree']=abs(nominal_trim['tail_incidence_deg']-T['incidence_deg'])<=1
rep['passed']=all(rep['gates'].values())
(B/'balance.json').write_text(json.dumps(rep,indent=2,default=lambda x:x.item())+'\n')
print(json.dumps({k:v for k,v in rep.items() if k not in ('items','gcode_sha256')},indent=2,default=lambda x:x.item()))
raise SystemExit(0 if rep['passed'] else 2)
