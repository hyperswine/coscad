#!/usr/bin/env python3
"""Pack actual projected outlines with explicit brim and support envelopes."""
from pathlib import Path
import json,random,itertools
import numpy as np
import trimesh
from shapely.geometry import MultiPoint,box
from shapely.affinity import translate,rotate
from shapely import intersects_xy
from scipy.signal import fftconvolve
HERE=Path(__file__).resolve().parent;SOURCE=HERE.parent
STEP=.5;N=512
SPECS={'hub':(8,[-1,-1,-1],1.5,[45,225]),'cap':(8,[0,0,1],1.5,[0]),'inner':(1,[0,0,-1],5,[0,45]),'strut':(8,[0,0,-1],5,[0]),'bar-print':(12,[-1,0,0],6,[0])}
shapes={};meshes={};options={}
for name,(count,down,brim,angles) in SPECS.items():
 mesh=trimesh.load_mesh(SOURCE/(name+'.stl'));mesh.apply_transform(trimesh.geometry.align_vectors(down,[0,0,-1]));mesh.apply_translation(-mesh.bounds[0]);meshes[name]=mesh
 hull=MultiPoint(mesh.vertices[:,:2]).convex_hull
 envelope=hull.buffer((6 if name=='inner' else brim)+.7,join_style=1)
 options[name]=[]
 for angle in angles:
  geom=rotate(envelope,angle,origin=(0,0));lo=np.array(geom.bounds[:2]);geom=translate(geom,-lo[0],-lo[1]);w,h=np.ceil(np.array(geom.bounds[2:])/STEP).astype(int)
  x,y=np.meshgrid((np.arange(w)+.5)*STEP,(np.arange(h)+.5)*STEP);mask=intersects_xy(geom,x,y)
  options[name].append((angle,lo,geom,mask))

def pack(order,seed=False):
 occ=np.ones((N,N),bool);occ[12:500,12:500]=False;occ[:56,:36]=True
 placed=[]
 if seed:
  for row in range(4):
   for col in range(2):
    choices=[]
    for angle,lo,geom,mask in options['hub']:
     h,w=mask.shape;x=12+col*124;y=12+row*122
     if y+h>N or x+w>N or np.any(occ[y:y+h,x:x+w]&mask):continue
     choices.append((angle,lo,geom,mask,x,y))
    if not choices:return placed
    angle,lo,geom,mask,x,y=choices[0];h,w=mask.shape;occ[y:y+h,x:x+w]|=mask
    placed.append({'name':'hub','angle':angle,'shift':(np.array([x,y])*STEP-lo).tolist(),'envelope':translate(geom,x*STEP,y*STEP),'brim_mm':SPECS['hub'][2]})
 for name in order:
  best=None
  for angle,lo,geom,mask in options[name]:
   h,w=mask.shape
   overlap=fftconvolve(occ.astype(float),mask[::-1,::-1],mode='valid')
   ys,xs=np.where(overlap<.1)
   if not len(xs):continue
   idx=np.argmin(ys*N+xs);x=int(xs[idx]);y=int(ys[idx]);score=(y+h,x,y)
   if best is None or score<best[0]:best=(score,angle,lo,geom,mask,x,y)
  if best is None:continue
  _,angle,lo,geom,mask,x,y=best;h,w=mask.shape;occ[y:y+h,x:x+w]|=mask
  placed.append({'name':name,'angle':angle,'shift':(np.array([x,y])*STEP-lo).tolist(),'envelope':translate(geom,x*STEP,y*STEP),'brim_mm':SPECS[name][2]})
 return placed

best=[]
orders=[['inner','hub','cap','strut','bar-print'],['hub','inner','cap','strut','bar-print'],['hub','cap','inner','strut','bar-print'],['cap','hub','inner','strut','bar-print']]
for types in orders:
 order=[name for name in types for _ in range(SPECS[name][0])]
 result=pack(order);print(types,len(result),flush=True)
 if len(result)>len(best):best=result
 if len(best)==37:break
if len(best)!=37:
 for types in [['inner','cap','strut','bar-print'],['cap','inner','strut','bar-print']]:
  order=[name for name in types for _ in range(SPECS[name][0])]
  result=pack(order,seed=True);print('seeded',types,len(result),flush=True)
  if len(result)>len(best):best=result
  if len(best)==37:break
if len(best)!=37:
 random.seed(281026)
 for attempt in range(24):
  big=['hub']*8+['cap']*8+['inner'];random.shuffle(big)
  result=pack(big+['strut']*8+['bar-print']*12)
  print('mixed',attempt,len(result),flush=True)
  if len(result)>len(best):best=result
  if len(best)==37:break
if len(best)!=37:raise SystemExit('No full single-plate layout: best '+str(len(best)))
items=[];all_meshes=[]
for i,entry in enumerate(best,1):
 name=entry['name'];m=meshes[name].copy();t=trimesh.transformations.rotation_matrix(np.deg2rad(entry['angle']),[0,0,1]);t[:2,3]=entry['shift'];m.apply_transform(t)
 assert m.bounds[0,2]>=-1e-6 and m.bounds[1,2]<=250
 m.export(HERE/(str(i)+'-'+name+'.stl'));all_meshes.append(m)
 items.append({k:v for k,v in entry.items() if k!='envelope'}|{'id':i,'file':str(i)+'-'+name+'.stl','bounds':m.bounds.tolist(),'envelope_wkt':entry['envelope'].wkt})
minimum_gap=999.
for a,b in itertools.combinations(best,2):
 actual_a=a['envelope'].buffer(-.7);actual_b=b['envelope'].buffer(-.7)
 assert actual_a.intersection(actual_b).area<.001,(a['name'],b['name'])
 gap=actual_a.distance(actual_b);minimum_gap=min(minimum_gap,gap)
 assert gap>=.5,(a['name'],b['name'],gap)
trimesh.util.concatenate(all_meshes).export(HERE/'full-bed.stl')
(HERE/'packing.json').write_text(json.dumps({'items':items,'grid_mm':STEP,'gap_allowance_mm':1.4,'inner_support_envelope_mm':6,'count':37,'minimum_brim_or_support_envelope_gap_mm':minimum_gap},indent=2)+'\n');print('PASS 37-part one-plate layout',flush=True)
