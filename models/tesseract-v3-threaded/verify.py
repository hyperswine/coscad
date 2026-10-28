#!/usr/bin/env python3
"""Mesh, screw-travel, closed-loop and insertion checks; never dispatches.

Uses compiler-resolved poses, not a second geometry generator. Sampled paths
are finite checks, not a proof over a continuous motion or a strength rating.
"""
import argparse,hashlib,itertools,json,re,subprocess
from pathlib import Path
import numpy as np
import trimesh
import manifold3d as md
HERE=Path(__file__).resolve().parent
TOL=0.01

def solid(mesh):
    s=md.Manifold(md.Mesh(np.asarray(mesh.vertices,dtype=np.float32),np.asarray(mesh.faces,dtype=np.uint32)))
    assert s.status()==md.Error.NoError,s.status()
    return s

def posed(s,t):return s.transform(t[:3,:])
def collision(a,b):return max(0.,(a^b).volume())
def main():
    ap=argparse.ArgumentParser();ap.add_argument('--coscad',required=True);ap.add_argument('--skip-build',action='store_true');args=ap.parse_args()
    tool=str(Path(args.coscad).resolve())
    names=['hub','cap','bar','strut','inner','coupon-socket','coupon-screw','male','female-tool']
    if not args.skip_build:
        subprocess.run(['python3',str(HERE/'export.py')],check=True)
        for name in names:subprocess.run([tool,'stl',str(HERE/(name+'.coscad')),'--backend=native'],check=True,stdout=subprocess.DEVNULL)
        subprocess.run([tool,'validate','--json',str(HERE/'tesseract.assemble')],check=True,stdout=subprocess.DEVNULL)
        subprocess.run([tool,str(HERE/'tesseract.assemble')],check=True,stdout=subprocess.DEVNULL)
    meshes={n:trimesh.load_mesh(HERE/(n+'.stl')) for n in names}
    stats={}
    for n,m in meshes.items():
        assert m.is_watertight and m.is_winding_consistent and len(m.split())==1 and m.volume>0,n
        stats[n]={'volume_mm3':float(m.volume),'components':1,'watertight':True,'bounds_mm':m.bounds.tolist(),'sha256':hashlib.sha256((HERE/(n+'.stl')).read_bytes()).hexdigest()}
    solids={n:solid(m) for n,m in meshes.items()}
    declarations=re.findall(r'^(\w+) ← (\S+)\.coscad', (HERE/'tesseract.assemble').read_text(),re.M)
    matrices=[np.asarray(json.loads(x),float) for x in re.findall(r'^  multmatrix\((.*)\) \{',(HERE/'tesseract_asm.scad').read_text(),re.M)]
    assert len(matrices)==len(declarations)==37
    parts={alias:(name,t,posed(solids[name],t)) for (alias,name),t in zip(declarations,matrices)}
    contacts=[];maximum=0
    for (a,(_,ta,sa)),(b,(_,tb,sb)) in itertools.combinations(parts.items(),2):
        vol=collision(sa,sb);maximum=max(maximum,vol)
        if vol>TOL:contacts.append([a,b,vol])
    print('assembly overlaps:',contacts,flush=True)
    assert not contacts,contacts
    # Cap removal: RH rotation and axial travel, against all assembled neighbors.
    _,cap_pose,_=parts['cap1'];neighbors=md.Manifold.batch_boolean([v[2] for k,v in parts.items() if k!='cap1'],md.OpType.Add)
    cap_peak=0
    for angle in np.linspace(0,1800,121):
        move=trimesh.transformations.rotation_matrix(np.deg2rad(angle),[0,0,1]);move[2,3]=3*angle/360
        cap_peak=max(cap_peak,collision(posed(solids['cap'],cap_pose@move),neighbors))
    print('cap screw sweep max:',cap_peak,flush=True);assert cap_peak<TOL
    wrong=trimesh.transformations.rotation_matrix(np.pi,[0,0,1])
    wrong_volume=collision(posed(solids['cap'],cap_pose@wrong),parts['hub1'][2]);assert wrong_volume>0.1,wrong_volume
    # Every bar enters both open hubs along its edge's outward face diagonal.
    entry_peak=0
    for i in range(1,13):
        open_parts=md.Manifold.batch_boolean([v[2] for k,v in parts.items() if not k.startswith('cap') and k!='bar'+str(i)],md.OpType.Add)
        _,pose,_=parts['bar'+str(i)]
        direction=pose[:3,3].copy();direction/=np.linalg.norm(direction)
        for distance in np.linspace(0,65,66):
            moved=pose.copy();moved[:3,3]+=direction*distance
            entry_peak=max(entry_peak,collision(posed(solids['bar'],moved),open_parts))
    print('bar entry max:',entry_peak,flush=True);assert entry_peak<TOL
    # Closed cap must retain the bar: an attempted outward pull collides.
    _,pose,_=parts['bar1'];d=pose[:3,3].copy();d/=np.linalg.norm(d);pulled=pose.copy();pulled[:3,3]+=d*4
    caps=md.Manifold.batch_boolean([v[2] for k,v in parts.items() if k.startswith('cap')],md.OpType.Add)
    retention=collision(posed(solids['bar'],pulled),caps);assert retention>0.1,retention
    # Same standalone RH interface in coupon, cap and diagonal struts.
    screw_peak=0
    for angle in np.linspace(0,1080,73):
        move=trimesh.transformations.rotation_matrix(np.deg2rad(angle),[0,0,1]);move[2,3]=angle*3/360
        screw_peak=max(screw_peak,collision(posed(solids['coupon-screw'],move),solids['coupon-socket']))
    assert screw_peak<TOL,screw_peak
    strut_peak=hub_peak=0
    _,strut_pose,_=parts['strut1'];_,hub_pose,_=parts['hub1']
    socket=strut_pose.copy();socket[:3,3]+=strut_pose[:3,2]*(40*3**.5-3-24)
    for angle in np.linspace(0,1080,73):
        move=trimesh.transformations.rotation_matrix(np.deg2rad(angle),[0,0,1]);move[2,3]=angle*3/360
        strut_peak=max(strut_peak,collision(posed(solids['strut'],strut_pose@move),parts['inner'][2]))
        hub_peak=max(hub_peak,collision(posed(solids['hub'],socket@move@np.linalg.inv(socket)@hub_pose),parts['strut1'][2]))
    assert strut_peak<TOL and hub_peak<TOL,(strut_peak,hub_peak)
    report={'parts':stats,'assembled_parts':37,'pair_checks':666,'max_assembly_interference_mm3':maximum,'cap_screw_poses':121,'cap_screw_max_mm3':cap_peak,'wrong_phase_control_mm3':wrong_volume,'bar_entry_samples':792,'bar_entry_max_mm3':entry_peak,'closed_cap_pull_control_mm3':retention,'coupon_screw_samples':73,'coupon_screw_max_mm3':screw_peak,'strut_unscrew_samples':73,'strut_unscrew_max_mm3':strut_peak,'hub_unscrew_samples':73,'hub_unscrew_max_mm3':hub_peak,'physical_fit':'not tested','sources_sha256':{p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in [HERE/'design.coscad',HERE/'tesseract.assemble']}}
    (HERE/'validation.json').write_text(json.dumps(report,indent=2)+'\n');print('PASS geometric assembly, thread motion and bar capture')
if __name__=='__main__':main()
