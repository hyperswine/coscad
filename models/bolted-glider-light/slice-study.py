"""Slice centered, axis-aligned individual parts and integrate object extrusion."""
import json
import math
import re
import subprocess
import tempfile
import zipfile
from pathlib import Path

root = Path(__file__).resolve().parent
base = root.parent / 'bolted-glider-balanced'
tool = '/Applications/BambuStudio.app/Contents/MacOS/BambuStudio'
result = {}
for name in ['wing-right','wing-left','nose','fuselage']:
    job = base / 'x1c-fuselage'
    presets = job/'presets'
    points = [tuple(map(float,line.split()[1:])) for line in (root/f'{name}.stl').read_text().splitlines() if line.strip().startswith('vertex ')]
    angle = math.radians({"wing-right":1.5,"wing-left":88.5,"nose":0,"fuselage":45}[name])
    cs,sn = math.cos(angle),math.sin(angle)
    points = [(cs*x-sn*y,sn*x+cs*y,z) for x,y,z in points]
    offsets = [(min(p[k] for p in points)+max(p[k] for p in points))/2 for k in (0,1)]
    vertices = list(dict.fromkeys(points))
    ids = {p:i for i,p in enumerate(vertices)}
    vs = ''.join(f'<vertex x="{x-offsets[0]:.6f}" y="{y-offsets[1]:.6f}" z="{z:.6f}"/>' for x,y,z in vertices)
    ts = ''.join('<triangle v1="%d" v2="%d" v3="%d"/>' % tuple(ids[p] for p in points[i:i+3]) for i in range(0,len(points),3))
    with tempfile.TemporaryDirectory() as directory:
        out = Path(directory)
        with zipfile.ZipFile(job/'glider-project.3mf') as template, zipfile.ZipFile(out/'part.3mf','w') as pack:
            for entry in template.namelist():
                data = template.read(entry)
                if entry=='3D/3dmodel.model':
                    data = re.sub(b'<mesh>.*?</mesh>',f'<mesh><vertices>{vs}</vertices><triangles>{ts}</triangles></mesh>'.encode(),data,flags=re.S)
                    assert b'128.0000 128.0000 0' in data
                pack.writestr(entry,data)
        proc = subprocess.run([tool,'--load-settings',f'{presets}/machine.json;{presets}/process.json','--load-filaments',str(presets/'filament.json'),'--arrange','0','--orient','0','--slice','1','--outputdir',str(out),str(out/'part.3mf')],capture_output=True)
        if not (out/"plate_1.gcode").exists(): raise RuntimeError((name,proc.returncode,proc.stdout[-2000:],proc.stderr[-2000:]))
        lines = (out/'plate_1.gcode').read_text().splitlines()
        reported = float(next(l.split(':')[-1] for l in lines if l.startswith('; total filament weight')))
        weight = moment = 0.
        active = False
        x=y=z=0.
        for line in lines:
            if line.startswith('; OBJECT_ID:'): active=True
            if line.startswith('; FEATURE: Custom'): active=False
            if not line.startswith(('G0 ','G1 ','G2 ','G3 ')): continue
            fields = {k:float(v) for k,v in re.findall(r'([XYZE])(-?[\d.]+)',line.split(';')[0])}
            xx,yy,zz = fields.get('X',x),fields.get('Y',y),fields.get('Z',z)
            if active and (xx!=x or yy!=y) and fields.get('E',0)>0:
                e=fields['E']
                if 'wing' in name:
                    chord=cs*((x+xx)/2-128+offsets[0])+sn*((y+yy)/2-128+offsets[1])
                    aft=chord*math.cos(math.radians(2.5))+(z+zz)/2*math.sin(math.radians(2.5))
                else: aft=-sn*((x+xx)/2-128+offsets[0])+cs*((y+yy)/2-128+offsets[1])
                moment+=aft*e
                weight+=e
            x,y,z=xx,yy,zz
        density=float(json.loads((presets/'filament.json').read_text())['filament_density'][0])
        mass=weight*math.pi*.875**2*density/1000
        assert abs(mass-reported)<1,(name,mass,reported)
        result[name]={'mass_g':mass,'aft_mm':moment/weight,'slicer_total_g':reported}
print(json.dumps(result,indent=2))
(root/'wing-study-slices.json').write_text(json.dumps(result,indent=2)+'\n')
baseline=json.loads((base/'balance.json').read_text())
items=[(v['mass_g'],v['aft_mm']) for v in result.values()]
items.extend((v['mass_g'],v['cg_mm'][1]) for v in baseline['items'] if v['name'] not in ('wing-left','wing-right','fuselage'))
# Two covered faces, 10% allowance for curvature, seams and adhesive.
items.extend([(2*.04442*1.10*25,115),(5,-53)])
total=sum(m for m,y in items)
cg=sum(m*y for m,y in items)/total
limit=baseline['neutral_point_y_mm']-.15*baseline['mac_mm']
print(total,cg,(baseline['neutral_point_y_mm']-cg)/baseline['mac_mm'], 'aft ballast at 194:',total*(limit-cg)/(194-limit))
