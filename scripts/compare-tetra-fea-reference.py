#!/usr/bin/env python3
"""Independently verify a CoScad Tet4 solve using scikit-fem/SciPy.
Required pins are shared with the Hex8 comparison scripts. This verifies the
same mesh/discretization, not continuum accuracy or actual printed strength.
"""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import numpy as np
import scipy
import skfem
from skfem import MeshTet, ElementTetP1, ElementTetP2, ElementVector, Basis, FacetBasis, LinearForm, condense, solve
from skfem.models.elasticity import linear_elasticity, lame_parameters


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--exe',required=True)
    parser.add_argument('--order',type=int,choices=[1,2],default=1)
    parser.add_argument('--mesh',required=True,type=Path)
    parser.add_argument('--conditions',required=True,type=Path)
    args=parser.parse_args()
    mesh_bytes=args.mesh.read_bytes()
    data=json.loads(mesh_bytes)
    config=json.loads(args.conditions.read_text())
    actual=json.loads(subprocess.check_output([args.exe,'tetra2' if args.order==2 else 'tetra',str(args.mesh),str(args.conditions)],text=True,timeout=300))['result']
    points=np.array(data['nodes_mm']); cells=np.array(data['tetrahedra'])
    mesh=MeshTet(points.T,cells.T,sort_t=False)
    basis=Basis(mesh,ElementVector(ElementTetP2() if args.order==2 else ElementTetP1()),intorder=2)
    lam,mu=lame_parameters(config['young_mpa'],config['poisson'])
    matrix=linear_elasticity(lam,mu).assemble(basis)

    def select(region):
        lo,hi=np.array(region['bounds_mm']); normal=np.array(region['normal'])
        boundary=mesh.boundary_facets(); xyz=mesh.p[:,mesh.facets[:,boundary]]
        inside=((xyz>=lo[:,None,None]-1e-7)&(xyz<=hi[:,None,None]+1e-7)).all(axis=(0,1))
        selected=boundary[inside]
        face=FacetBasis(mesh,basis.elem,facets=selected,intorder=2)
        axis=np.flatnonzero(normal)[0]
        if not len(selected) or not np.allclose(face.normals[axis],normal[axis]):
            raise ValueError('reference face selection/normal mismatch')
        return selected,face

    support,_=select(config['support']); _,face=select(config['load'])
    total=np.array(config['force_n']); area=float(face.dx.sum())

    @LinearForm
    def traction(v,w):
        return sum(total[d]*v[d] for d in range(3))/area

    force=traction.assemble(face)
    fixed=basis.get_dofs(facets=support).all()
    u=solve(*condense(matrix,force,D=fixed))
    if not np.isfinite(u).all():
        raise ValueError('reference solve produced nonfinite values')
    residual=matrix@u-force
    reaction=np.zeros(3); moment=np.zeros(3)
    for d, indices in enumerate(basis.split_indices()):
        ids=np.intersect1d(fixed,indices)
        reaction[d]=residual[ids].sum()
        forces=np.zeros((len(ids),3)); forces[:,d]=residual[ids]
        moment+=np.cross(basis.doflocs[:,ids].T,forces).sum(axis=0)
    # Match physical DOF coordinates, since the two solvers number mid-edge
    # nodes independently for quadratic elements.
    component_indices=basis.split_indices()
    lookup=[{tuple(np.round(basis.doflocs[:,q],10)):u[q] for q in component_indices[d]} for d in range(3)]
    actual_u=np.array([x['displacement_mm'] for x in actual['nodes']])
    reference_u=np.array([[lookup[d][tuple(np.round(x['position_mm'],10))] for d in range(3)] for x in actual['nodes']])
    centre_basis=Basis(mesh,basis.elem,quadrature=(np.full((3,1),.25),np.ones(1)))
    gradients=centre_basis.interpolate(u).grad[:,:,:,0]
    strain=(gradients+gradients.transpose(1,0,2))/2
    sigma=2*mu*strain+lam*np.einsum('iik->k',strain)[None,None,:]*np.eye(3)[:,:,None]
    reference_s=sigma[[0,1,2,0,1,2],[0,1,2,1,2,0]].T
    actual_s=np.array([x['xx_yy_zz_xy_yz_zx_mpa'] for x in actual['cell_stresses']])
    if not np.allclose(np.array([x['centre_mm'] for x in actual['cell_stresses']]),points[cells].mean(axis=1),rtol=0,atol=1e-10):
        raise ValueError('unexpected output element order')
    errors={'all_displacements_relative_l2':float(np.linalg.norm(actual_u-reference_u)/np.linalg.norm(reference_u)),
            'all_cell_stresses_relative_l2':float(np.linalg.norm(actual_s-reference_s)/np.linalg.norm(reference_s)),
            'energy_relative':abs(actual['strain_energy_n_mm']/float(u@(matrix@u)/2)-1),
            'reaction_absolute_n':float(np.linalg.norm(np.array(actual['support_reaction_n'])-reaction)),
            'moment_absolute_n_mm':float(np.linalg.norm(np.array(actual['support_moment_n_mm'])-moment))}
    passed=all(np.isfinite(list(errors.values()))) and all(v<(1e-5 if k=='moment_absolute_n_mm' else 1e-6) for k,v in errors.items())
    result={k:v for k,v in actual.items() if k not in ('nodes','cell_stresses')}
    print(json.dumps({'schema':'coscad.tetra-fea-reference.v1','passed':bool(passed),
                      'versions':{'scikit_fem':skfem.__version__,'numpy':np.__version__,'scipy':scipy.__version__},
                      'element_order':args.order,'mesh_sha256':hashlib.sha256(mesh_bytes).hexdigest(),'conditions':config,
                      'provenance':data['provenance'],'load_area_mm2':area,'errors':errors,'result':result,
                      'scope':'Independent tetrahedral assembly, traction integration and direct solve on the same mesh; no physical strength claim.'},indent=2,allow_nan=False))
    raise SystemExit(0 if passed else 1)


if __name__=='__main__':
    main()
