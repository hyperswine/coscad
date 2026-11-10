"""Deposited filament mass/moments, including ears; excludes purge and retractions.
Supports the Bambu G90/M83, G17, G0/G1/G2/G3 dialect; rejects other active modes.
Arc centroid is integrated analytically. Object labels map to project IDs 2,4,...
"""
import json, math, re
from pathlib import Path
import numpy as np
import trimesh

def arc_centroid(start, end, ij, clockwise):
    centre = start + ij
    radius = np.linalg.norm(ij)
    assert radius > 0
    a = math.atan2(*(start-centre)[::-1]); b = math.atan2(*(end-centre)[::-1])
    sweep = (b-a) % (2*math.pi)
    if clockwise: sweep = -((a-b) % (2*math.pi))
    if abs(sweep) < 1e-10: sweep = -2*math.pi if clockwise else 2*math.pi
    assert abs(np.linalg.norm(end-centre)-radius) < 0.05, 'inconsistent arc radius'
    return centre + radius*np.array([(math.sin(a+sweep)-math.sin(a))/sweep,
                                    (math.cos(a)-math.cos(a+sweep))/sweep])

def read_job(base, job):
    folder = base/job
    layout = json.loads((folder/'layout.json').read_text())
    machine = json.loads((folder/'presets/machine.json').read_text())
    filament = json.loads((folder/'presets/filament.json').read_text())
    density = float(filament['filament_density'][0]); diameter = float(filament['filament_diameter'][0])
    factor = density/1000 * math.pi*(diameter/2)**2
    offset = np.array([float(v) for v in machine['extruder_offset'][0].split('x')])
    ids = {2*i+2: row['part'] for i,row in enumerate(layout)}
    accum = {p: np.zeros(4) for p in ids.values()}
    xy = np.zeros(2); obj=None; feature=''; relative_e=False; absolute_xyz=True; plane='G17'
    layer_z=layer_h=None
    for line in (folder/'slice/plate_1.gcode').read_text().splitlines():
        if line.startswith('; start printing object, unique label id:'):
            obj = ids[int(line.rsplit(':',1)[1])]
        if line.startswith('; stop printing object'): obj=None
        if line.startswith('; FEATURE:'): feature=line.split(':',1)[1].strip()
        if line.startswith('; Z_HEIGHT:'):
            layer_z=float(line.split(':')[1])
            if len(ids)==1: obj=next(iter(ids.values()))
        if line.startswith('; LAYER_HEIGHT:'): layer_h=float(line.split(':')[1])
        code=line.split(';')[0].strip()
        if not code: continue
        op=code.split()[0]
        if op in ('G90','G91'): absolute_xyz=op=='G90'
        if op in ('M82','M83'): relative_e=op=='M83'
        if op in ('G17','G18','G19'): plane=op
        if op not in ('G0','G1','G2','G3'): continue
        v={k:float(x) for k,x in re.findall(r'([XYEIJ])(-?\d*\.?\d+)',code)}
        end=np.array([v.get('X',xy[0]),v.get('Y',xy[1])]) if absolute_xyz else xy+np.array([v.get('X',0),v.get('Y',0)])
        if obj and feature not in ('Custom','Skirt') and v.get('E',0)>0 and (np.linalg.norm(end-xy)>1e-10 or op in ('G2','G3')):
            assert relative_e and absolute_xyz and plane=='G17', 'unsupported extrusion mode'
            assert layer_z is not None and layer_h is not None
            c=(xy+end)/2 if op in ('G0','G1') else arc_centroid(xy,end,np.array([v.get('I',0),v.get('J',0)]),op=='G2')
            m=v['E']*factor
            accum[obj] += np.r_[m,m*(c+offset),m*(layer_z-layer_h/2)]
        xy=end
    result={}
    for row in layout:
        part=row['part']; m=accum[part][0]; assert m>0
        c=accum[part][1:]/m
        bounds=np.array([trimesh.load_mesh(base/f'{part}.stl').bounds,trimesh.load_mesh(base/f'ears-{part}.stl').bounds])
        lo=bounds[:,0].min(0); hi=bounds[:,1].max(0)
        a=math.radians(row['rotation_deg']); rot=np.array([[math.cos(a),-math.sin(a)],[math.sin(a),math.cos(a)]])
        c[:2]=rot.T@(c[:2]-row['centre_mm'])+(lo[:2]+hi[:2])/2
        c[2]+=lo[2]
        result[part]={'mass_g':m,'print_cg_mm':c.tolist()}
    return result

if __name__=='__main__':
    import sys
    base=Path(sys.argv[1]) if len(sys.argv)>1 else Path(__file__).resolve().parent
    print(json.dumps({j:read_job(base,j) for j in ('a1-wing-right','p1s-wing-left','x1c-fuselage','a1-tail')},indent=2))
