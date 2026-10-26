#!/usr/bin/env python3
"""Experimental STL -> Tet4 mesh, using pinned gmsh==4.15.2 and NumPy.

Preserves a checked surface contract: closed oriented mesh, topology, volume,
and bidirectional vertex/face-centre distance. Not an FEA solve or print command.
Based on the Gmsh t13 discrete-surface remeshing workflow.
"""
import argparse
from collections import defaultdict
import hashlib
import json
import math
import os
from pathlib import Path
import re
import sys
import subprocess
import tempfile
import gmsh
import numpy as np


def surface_topology(triangles):
    points, index = np.unique(triangles.reshape(-1, 3), axis=0, return_inverse=True)
    faces = index.reshape(-1, 3)
    edges = defaultdict(list)
    for i, (a, b, c) in enumerate(faces):
        for u, v in ((a, b), (b, c), (c, a)):
            edges[tuple(sorted((u, v)))].append((i, 1 if u<v else -1))
    if any(len(fs)!=2 or sum(s for _, s in fs)!=0 for fs in edges.values()):
        raise ValueError('surface is not closed and consistently oriented, or has coincident nonmanifold topology')
    adjacency = defaultdict(set)
    for pair in edges.values():
        a, b = pair[0][0], pair[1][0]
        adjacency[a].add(b); adjacency[b].add(a)
    unseen = set(range(len(faces))); components = 0
    while unseen:
        todo = [unseen.pop()]; components += 1
        while todo:
            neighbours = adjacency[todo.pop()] & unseen
            unseen -= neighbours; todo.extend(neighbours)
    return {'euler_characteristic': int(len(points)-len(edges)+len(faces)), 'surface_components': components}


def distance_to_surface(samples, triangles):
    """Exact point-triangle distance; samples are a finite diagnostic, not a Hausdorff proof."""
    maximum = 0.
    a, b, c = triangles[:, 0], triangles[:, 1], triangles[:, 2]
    u, v = b-a, c-a
    uu, uv, vv = (np.einsum('ij,ij->i', x, y) for x, y in ((u,u),(u,v),(v,v)))
    denom = uu*vv-uv*uv
    if np.any(denom<=0):
        raise ValueError('degenerate surface triangle')
    for start in range(0, len(samples), 64):
        p = samples[start:start+64, None, :]
        w = p-a
        wu, wv = np.einsum('ptj,tj->pt',w,u), np.einsum('ptj,tj->pt',w,v)
        s, t = (wu*vv-wv*uv)/denom, (wv*uu-wu*uv)/denom
        projected = a+s[...,None]*u+t[...,None]*v
        dist = np.sum((p-projected)**2,axis=2)
        dist[(s<0)|(t<0)|(s+t>1)] = np.inf
        for x, y in ((a,b),(b,c),(c,a)):
            d = y-x
            fraction = np.clip(np.einsum('ptj,tj->pt',p-x,d)/np.einsum('ij,ij->i',d,d),0,1)
            dist = np.minimum(dist,np.sum((p-(x+fraction[...,None]*d))**2,axis=2))
        maximum = max(maximum,float(np.sqrt(dist.min(axis=1).max())))
    return maximum


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('stl',type=Path)
    parser.add_argument('--output',required=True,type=Path)
    parser.add_argument('--size',required=True,type=float,help='target edge size in mm')
    parser.add_argument('--timeout',type=float,default=120,help='worker deadline in seconds (1..900)')
    parser.add_argument('--_worker',action='store_true',help=argparse.SUPPRESS)
    args = parser.parse_args()
    if not math.isfinite(args.timeout) or not 1<=args.timeout<=900:
        parser.error('timeout must be finite and in [1,900] seconds')
    if not args._worker:
        try:
            result = subprocess.run([sys.executable,str(Path(__file__).resolve()),*sys.argv[1:],'--_worker'],
                                    timeout=args.timeout)
        except subprocess.TimeoutExpired:
            parser.exit(1,'Error: volume meshing deadline exceeded; no result accepted\n')
        raise SystemExit(result.returncode)
    if gmsh.__version__!='4.15.2':
        parser.error('requires pinned gmsh==4.15.2')
    if not math.isfinite(args.size) or args.size<0.1:
        parser.error('size must be finite and at least 0.1 mm')
    if args.stl.stat().st_size>25_000_000:
        parser.error('STL exceeds 25 MB')
    content = args.stl.read_bytes()
    vertices = re.findall(r'^\s*vertex\s+(\S+)\s+(\S+)\s+(\S+)\s*$',content.decode('ascii'),re.M)
    source = np.array(vertices,dtype=float).reshape(-1,3,3)
    if not 0<len(source)<=50000 or not np.isfinite(source).all():
        parser.error('requires a finite ASCII STL with 1..50000 triangles')
    topology = surface_topology(source)
    lo, hi = source.min(axis=(0,1)), source.max(axis=(0,1))
    if np.prod((hi-lo)/args.size)>200000:
        parser.error('requested meshing density exceeds the experiment budget')
    source_volume = float(np.einsum('ij,ij->i',source[:,0],np.cross(source[:,1],source[:,2])).sum()/6)
    if source_volume<=0:
        parser.error('source winding has nonpositive volume')
    gmsh.initialize()
    try:
        gmsh.option.setNumber('General.Terminal',0)
        gmsh.option.setNumber('General.NumThreads',1)
        gmsh.option.setNumber('Mesh.MaxNumThreads3D',1)
        gmsh.option.setNumber('Mesh.RandomSeed',1)
        gmsh.merge(str(args.stl.resolve()))
        gmsh.model.mesh.classifySurfaces(math.radians(1),True,True,math.pi)
        gmsh.model.mesh.createGeometry()
        surfaces = [tag for dim, tag in gmsh.model.getEntities(2)]
        loop = gmsh.model.geo.addSurfaceLoop(surfaces)
        gmsh.model.geo.addVolume([loop])
        gmsh.model.geo.synchronize()
        gmsh.option.setNumber('Mesh.MeshSizeMin',args.size)
        gmsh.option.setNumber('Mesh.MeshSizeMax',args.size)
        gmsh.option.setNumber('Mesh.MeshSizeFromCurvature',0)
        gmsh.option.setNumber('Mesh.MeshSizeExtendFromBoundary',0)
        gmsh.model.mesh.generate(3)
        tags, xyz, _ = gmsh.model.mesh.getNodes()
        nodes = np.array(xyz).reshape(-1,3)
        indices = {int(tag):i for i, tag in enumerate(tags)}
        types, _, element_nodes = gmsh.model.mesh.getElements(3)
        if list(types)!=[4]:
            raise ValueError('expected only linear tetrahedra')
        tets = np.array([indices[int(t)] for t in element_nodes[0]]).reshape(-1,4)
    finally:
        gmsh.finalize()
    if len(tets)>30000 or len(nodes)>15000:
        raise ValueError('tetra mesh exceeds 30000 elements or 15000 nodes')
    used = np.unique(tets)
    remap = np.full(len(nodes),-1,dtype=int); remap[used] = np.arange(len(used))
    nodes, tets = nodes[used], remap[tets]
    x = nodes[tets]
    determinants = np.einsum('ij,ij->i',x[:,1]-x[:,0],np.cross(x[:,2]-x[:,0],x[:,3]-x[:,0]))
    reverse = determinants<0
    tets[reverse,1],tets[reverse,2] = tets[reverse,2].copy(),tets[reverse,1].copy()
    if np.any(determinants==0) or not np.isfinite(nodes).all():
        raise ValueError('degenerate/nonfinite tetrahedral mesh')
    faces = defaultdict(list)
    for index, (a,b,c,d) in enumerate(tets):
        for face in ((b,c,d),(a,d,c),(a,b,d),(a,c,b)):
            faces[tuple(sorted(face))].append((index,face))
    if any(len(v)>2 for v in faces.values()):
        raise ValueError('nonmanifold tetrahedral adjacency')
    adjacency = defaultdict(set)
    for pair in faces.values():
        if len(pair)==2:
            a,b = pair[0][0],pair[1][0]; adjacency[a].add(b); adjacency[b].add(a)
    reached, todo = {0}, [0]
    while todo:
        new = adjacency[todo.pop()]-reached; reached |= new; todo.extend(new)
    if len(reached)!=len(tets):
        raise ValueError('mesh has disconnected volumes; contact/joints require explicit modeling')
    boundary = np.array([v[0][1] for v in faces.values() if len(v)==1])
    triangles = nodes[boundary]
    if surface_topology(triangles)!=topology:
        raise ValueError('meshing changed surface topology; a hole or component may have disappeared')
    volume = float(np.abs(determinants).sum()/6)
    volume_error = abs(volume/source_volume-1)
    def samples(tris):
        return np.concatenate((np.unique(tris.reshape(-1,3),axis=0),tris.mean(axis=1)))
    deviation = max(distance_to_surface(samples(source),triangles),distance_to_surface(samples(triangles),source))
    tolerance = min(0.05,args.size*0.05)
    if volume_error>0.005 or deviation>tolerance:
        raise ValueError(f'mesh geometry not accepted: volume error={volume_error:.6g}, sampled deviation={deviation:.6g} mm (limit {tolerance})')
    receipt = {'schema':'coscad.fea-tet-mesh.v1','nodes_mm':nodes.tolist(),'tetrahedra':tets.tolist(),
               'provenance':{'gmsh_version':gmsh.__version__,'source_stl_sha256':hashlib.sha256(content).hexdigest(),
                             'target_size_mm':args.size,'surface_classification_angle_degrees':1,'source_volume_mm3':source_volume,'tetra_volume_mm3':volume,
                             'relative_volume_error':volume_error,'bidirectional_sampled_surface_error_mm':deviation,
                             'surface_check':'vertices and face centres; finite sampling, not exact Hausdorff proof',
                             'topology':topology,'reoriented_tetrahedra':int(reverse.sum())}}
    # Only replace a previous accepted mesh after all checks and serialization.
    serialized = json.dumps(receipt,separators=(',',':'),allow_nan=False)+'\n'
    temporary = None
    try:
        with tempfile.NamedTemporaryFile(mode='w',dir=args.output.parent,prefix='.fea-mesh-',delete=False) as file:
            temporary = file.name
            file.write(serialized)
            file.flush()
            os.fsync(file.fileno())
        os.replace(temporary,args.output)
    finally:
        if temporary and os.path.exists(temporary):
            os.unlink(temporary)
    print(json.dumps({'mesh':str(args.output),'nodes':len(nodes),'elements':len(tets),**receipt['provenance']},indent=2))


if __name__=='__main__':
    try:
        main()
    except (ValueError,RuntimeError,OSError) as error:
        print('Error:',error,file=sys.stderr)
        raise SystemExit(1)
