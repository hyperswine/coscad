#!/usr/bin/env python3
"""Quadratic solid FEA of the existing seam bracket under 1 N flange pressure.
Full back-plate clamp is an optimistic fixture, not a bolt/contact model.
Uses the pinned scikit-fem companion because the current CoScad tetra input
rejects loads sharing clamped nodes, which legitimately occurs at this edge.
"""
import argparse,hashlib,json
from pathlib import Path
import numpy as np
from skfem import MeshTet,ElementTetP2,ElementVector,Basis,FacetBasis,LinearForm,condense,solve
from skfem.models.elasticity import lame_parameters,linear_elasticity

def main():
    p=argparse.ArgumentParser();p.add_argument('mesh',type=Path);p.add_argument('--output',type=Path,required=True);a=p.parse_args()
    data=json.loads(a.mesh.read_text());mesh=MeshTet(np.array(data['nodes_mm']).T,np.array(data['tetrahedra']).T,sort_t=False)
    if mesh.t.shape[1]>20000:raise ValueError('study mesh limit exceeded')
    basis=Basis(mesh,ElementVector(ElementTetP2()),intorder=2)
    lam,mu=lame_parameters(2000.,.3);K=linear_elasticity(lam,mu).assemble(basis)
    boundary=mesh.boundary_facets();xyz=mesh.p[:,mesh.facets[:,boundary]]
    support=boundary[np.all(abs(xyz[0]+2.5)<1e-7,axis=0)]
    load=boundary[np.all(abs(xyz[2]-10)<1e-7,axis=0)]
    fb=FacetBasis(mesh,basis.elem,facets=load,intorder=2);area=float(fb.dx.sum())
    sb=FacetBasis(mesh,basis.elem,facets=support,intorder=2)
    if not len(support) or not len(load) or not np.allclose(fb.normals[2],1) or not np.allclose(sb.normals[0],-1):raise ValueError('surface selection failed')
    @LinearForm
    def traction(v,w):return -v[2]/area
    f=traction.assemble(fb);fixed=basis.get_dofs(facets=support).all();u=solve(*condense(K,f,D=fixed));r=K@u-f
    free=np.setdiff1d(np.arange(basis.N),fixed);res=float(np.linalg.norm(r[free])/np.linalg.norm(f[free]))
    reaction=np.zeros(3);moment=np.zeros(3);applied=np.zeros(3);applied_moment=np.zeros(3)
    for d,ids in enumerate(basis.split_indices()):
        constrained=np.intersect1d(ids,fixed);reaction[d]=r[constrained].sum();applied[d]=f[ids].sum()
        forces=np.zeros((len(constrained),3));forces[:,d]=r[constrained];moment+=np.cross(basis.doflocs[:,constrained].T,forces).sum(axis=0)
        forces=np.zeros((len(ids),3));forces[:,d]=f[ids];applied_moment+=np.cross(basis.doflocs[:,ids].T,forces).sum(axis=0)
    if res>1e-7 or np.linalg.norm(reaction+applied)>1e-6 or np.linalg.norm(moment+applied_moment)>1e-5:raise ValueError('residual/resultant failure')
    cb=Basis(mesh,basis.elem,quadrature=(np.full((3,1),.25),np.ones(1)))
    g=cb.interpolate(u).grad[:,:,:,0];strain=(g+g.transpose(1,0,2))/2
    stress=2*mu*strain+lam*np.einsum('iik->k',strain)[None,None,:]*np.eye(3)[:,:,None]
    dev=stress-np.eye(3)[:,:,None]*np.einsum('iik->k',stress)[None,None,:]/3
    vm=np.sqrt(1.5*np.einsum('ijk,ijk->k',dev,dev))
    vertex_u=u[basis.nodal_dofs].T
    result={'schema':'coscad.tank-bracket-unit-fea.v1','mesh_sha256':hashlib.sha256(a.mesh.read_bytes()).hexdigest(),'source_sha256':hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),'provenance':data['provenance'],
      'elements':int(mesh.t.shape[1]),'dofs':int(basis.N),'material':{'name':'PETG illustrative isotropic','young_mpa':2000,'poisson':.3},'total_force_n':[0,0,-1],'load_area_mm2':area,
      'load_average_displacement_mm':float(f@u),'max_vertex_displacement_mm':float(np.linalg.norm(vertex_u,axis=1).max()),'max_cell_centre_vm_mpa':float(vm.max()),'p95_cell_centre_vm_mpa':float(np.quantile(vm,.95)),
      'residual_relative':res,'reaction_n':reaction.tolist(),'force_balance_n':float(np.linalg.norm(reaction+applied)),'moment_balance_n_mm':float(np.linalg.norm(moment+applied_moment)),
      'scope':'Actual native seam-bracket mesh; 1 N uniform top-face pressure, fully clamped back plate excluding protruding key. Optimistic support fixture; no bolt preload, rail contact, slipping, thread/pull-out or PETG layer anisotropy. Stress peaks depend on mesh.'}
    a.output.write_text(json.dumps(result,indent=2,allow_nan=False)+'\n');print(json.dumps({k:result[k] for k in ['elements','dofs','load_average_displacement_mm','max_cell_centre_vm_mpa','residual_relative']}),flush=True)
if __name__=='__main__':main()
