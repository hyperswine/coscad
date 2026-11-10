"""Bambu relative-extrusion mass moments. Explicit assembly-frame transforms.
No printer communication. Requires numpy; importing this module reads no files.
"""
import hashlib
import json
import math
from pathlib import Path
import re
import numpy as np


def positive(value, name):
    if isinstance(value,bool):raise ValueError(f'{name} must be numeric, not boolean')
    value=float(value)
    if not math.isfinite(value) or value<=0: raise ValueError(f'{name} must be positive and finite')
    return value


def arc_centroid(start, end, ij, clockwise):
    start,end,ij=(np.asarray(v,dtype=float) for v in (start,end,ij))
    centre=start+ij; radius=np.linalg.norm(ij)
    if not np.isfinite([*start,*end,*ij]).all() or radius<=0: raise ValueError('invalid arc')
    a=math.atan2(*(start-centre)[::-1]); b=math.atan2(*(end-centre)[::-1])
    sweep=-((a-b)%(2*math.pi)) if clockwise else (b-a)%(2*math.pi)
    if abs(sweep)<1e-10: sweep=-2*math.pi if clockwise else 2*math.pi
    if abs(np.linalg.norm(end-centre)-radius)>.05: raise ValueError('inconsistent arc radius')
    return centre+radius*np.array([(math.sin(a+sweep)-math.sin(a))/sweep,(math.cos(a)-math.cos(a+sweep))/sweep])


def rigid_transform(value):
    m=np.asarray(value,dtype=float)
    if m.shape!=(4,4) or not np.isfinite(m).all() or not np.allclose(m[3],[0,0,0,1],rtol=0,atol=1e-10):
        raise ValueError('plate_to_assembly must be a finite affine 4x4 matrix')
    if not np.allclose(m[:3,:3].T@m[:3,:3],np.eye(3),rtol=0,atol=1e-8):
        raise ValueError('plate_to_assembly must preserve lengths (rotation/reflection and translation only)')
    return m


def deposited_moments(text, objects, density_g_cm3, filament_diameter_mm, offset_xy=(0,0)):
    """Return per-object grams and plate-frame centres, including modeled ears.

    Supports Bambu G90/M83, G17 XY arcs and the slicer's object/layer/feature
    annotations. Unknown active object IDs and unsupported deposition modes fail.
    Skirt, brim, support, purge and wipe features are not model material.
    """
    factor=positive(density_g_cm3,'density')/1000*math.pi*(positive(filament_diameter_mm,'diameter')/2)**2
    offset=np.asarray(offset_xy,dtype=float)
    if offset.shape!=(2,) or not np.isfinite(offset).all(): raise ValueError('invalid extruder offset')
    if not objects or len(set(objects.values()))!=len(objects) or not all(isinstance(n,str) and n for n in objects.values()):
        raise ValueError('object IDs and names must be nonempty and unique')
    accum={name:np.zeros(4) for name in objects.values()}
    xy=np.zeros(2); obj=None; feature=''; relative_e=False; absolute_xyz=True; plane='G17'; millimeters=True
    layer_z=layer_h=None; tools=set(); volumetric=False; flow=100.0
    accepted={'Outer wall','Inner wall','Overhang wall','Sparse infill','Internal solid infill','Top surface','Bottom surface','Bridge','Internal bridge','Gap infill','Floating vertical shell'}
    for lineno,line in enumerate(text.splitlines(),1):
        if line.startswith('; start printing object, unique label id:'):
            oid=int(line.rsplit(':',1)[1])
            if oid not in objects: raise ValueError(f'line {lineno}: unknown object ID {oid}')
            obj=objects[oid]
        if line.startswith('; stop printing object'): obj=None
        if line.startswith('; FEATURE:'): feature=line.split(':',1)[1].strip()
        if line.startswith('; Z_HEIGHT:'):
            layer_z=positive(line.split(':')[1],'layer Z')
            if len(objects)==1:obj=next(iter(objects.values()))
        if line.startswith('; LAYER_HEIGHT:'):layer_h=positive(line.split(':')[1],'layer height')
        code=line.split(';')[0].strip()
        if not code:continue
        words=code.split();op=words[0]
        if op in ('G90','G91'):absolute_xyz=op=='G90'
        if op in ('M82','M83'):relative_e=op=='M83'
        if op in ('G17','G18','G19'):plane=op
        if op in ('G20','G21'):millimeters=op=='G21'
        if op=='G92' and any(w[0] in 'XYZ' for w in words[1:]):
            raise ValueError(f'line {lineno}: coordinate resets are unsupported')
        if re.fullmatch(r'T[0-9]+',op) and int(op[1:])<255:tools.add(int(op[1:]))
        if op=='M221':
            # Bare S/R are Bambu soft-endstop save/restore, not a flow change.
            settings=[float(w[1:]) for w in words[1:] if w.startswith('S') and len(w)>1]
            if settings:flow=settings[-1]
        if op=='M200':
            ds=[float(w[1:]) for w in words[1:] if w.startswith('D')]
            if not ds:raise ValueError(f'line {lineno}: unknown volumetric extrusion setting')
            volumetric=ds[-1]!=0
        if op not in ('G0','G1','G2','G3'):
            if obj and feature in accepted and op.startswith('G') and any(w.startswith('E') for w in words[1:]) and op!='G92':
                raise ValueError(f'line {lineno}: unsupported extrusion command {op}')
            continue
        values={}
        for word in words[1:]:
            if word[0] in 'XYEIJRP':
                try:value=float(word[1:])
                except ValueError:raise ValueError(f'line {lineno}: malformed motion field {word}') from None
                if not math.isfinite(value):raise ValueError(f'line {lineno}: nonfinite motion')
                values[word[0]]=value
        end=np.array([values.get('X',xy[0]),values.get('Y',xy[1])]) if absolute_xyz else xy+np.array([values.get('X',0),values.get('Y',0)])
        deposition=values.get('E',0)>0 and (np.linalg.norm(end-xy)>1e-10 or op in ('G2','G3'))
        if obj and deposition:
            excluded=feature in ('Custom','Skirt') or feature.startswith(('Support','Brim','Wipe'))
            if not excluded:
                if feature not in accepted:raise ValueError(f'line {lineno}: unsupported model feature {feature!r}')
                if not(relative_e and absolute_xyz and millimeters and plane=='G17') or volumetric or len(tools)>1 or flow!=100:raise ValueError(f'line {lineno}: unsupported extrusion mode')
                if layer_z is None or layer_h is None or layer_h>layer_z+1e-8:raise ValueError(f'line {lineno}: missing/inconsistent layer annotation')
                if op in ('G2','G3') and ('R' in values or values.get('P',1)!=1):raise ValueError(f'line {lineno}: only single-turn IJ arcs supported')
                c=(xy+end)/2 if op in ('G0','G1') else arc_centroid(xy,end,[values.get('I',0),values.get('J',0)],op=='G2')
                mass=values['E']*factor
                accum[obj]+=np.r_[mass,mass*(c+offset),mass*(layer_z-layer_h/2)]
        elif deposition and feature in accepted and layer_z is not None:
            raise ValueError(f'line {lineno}: model extrusion without an object label')
        xy=end
    result={}
    for name,v in accum.items():
        if not np.isfinite(v).all() or v[0]<=0:raise ValueError(f'{name}: no finite deposited mass')
        result[name]={'mass_g':float(v[0]),'center_mm':(v[1:]/v[0]).tolist()}
    return result


def report(manifest, base):
    if manifest.get('schema')!='coscad.slice-mass/1':raise ValueError('expected coscad.slice-mass/1')
    if manifest.get('dialect')!='bambu-relative-e':raise ValueError('only bambu-relative-e is supported')
    components=[]; sources=[]; names=set()
    def add(row):
        name=row['name']
        if not isinstance(name,str) or not name or name in names:raise ValueError('component names must be nonempty and unique across all jobs and hardware')
        names.add(name); positive(row['mass_g'],'mass')
        c=np.asarray(row['center_mm'],dtype=float)
        if c.shape!=(3,) or not np.isfinite(c).all():raise ValueError('center_mm must contain three finite coordinates')
        components.append(row)
    for job in manifest.get('jobs',[]):
        path=(base/job['gcode']).resolve();data=path.read_bytes()
        rows=job['objects'];ids={r['id']:r['name'] for r in rows}
        if len(ids)!=len(rows) or not all(type(i) is int and i>=0 for i in ids):raise ValueError('object IDs must be unique nonnegative integers')
        moments=deposited_moments(data.decode('utf-8'),ids,job['density_g_cm3'],job['filament_diameter_mm'],job.get('extruder_offset_xy_mm',[0,0]))
        sources.append({'gcode':job['gcode'],'sha256':hashlib.sha256(data).hexdigest()})
        for row in rows:
            item=moments[row['name']];transform=rigid_transform(row['plate_to_assembly'])
            centre=(transform@np.r_[item['center_mm'],1])[:3].tolist()
            measured=row.get('measured_mass_g')
            mass=positive(measured,'measured mass') if measured is not None else item['mass_g']
            add({'name':row['name'],'kind':'measured' if measured is not None else 'sliced','mass_g':mass,'center_mm':centre,
                 'deposited_mass_g':item['mass_g'],'center_basis':'sliced-extrusion'})
    for row in manifest.get('hardware',[]):
        add({'name':row['name'],'kind':'hardware','mass_g':positive(row['mass_g'],'hardware mass'),'center_mm':row['center_mm']})
    if not components:raise ValueError('no components')
    total=sum(c['mass_g'] for c in components)
    ref=np.asarray(components[0]['center_mm'],dtype=float)
    centre=ref+sum((c['mass_g']*(np.asarray(c['center_mm'])-ref) for c in components),np.zeros(3))/total
    if not math.isfinite(total) or not np.isfinite(centre).all():raise ValueError('mass moment overflow')
    return {'schema':'coscad.mass-report/1','basis':'sliced-extrusion-plus-explicit-hardware','mass_g':total,'center_mm':centre.tolist(),
            'components':components,'sources':sources,'limitations':['Modeled mouse ears remain included; purge, support, brim and skirt excluded.',
            'Measured mass overrides retain sliced centres. Filament density, diameter and flow are estimates, not physical measurements.',
            'Rigid transforms are explicit; no automatic assembly placement, uncertainty bound, inertia or strength verdict.']}
