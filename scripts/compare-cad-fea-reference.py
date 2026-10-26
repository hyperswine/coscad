#!/usr/bin/env python3
"""Independent assembly, traction integration and solve for CAD Hex8 receipts.
Uses the same pinned optional environment as compare-fea-reference.py.
The exported occupancy is checked against known fixture geometry independently;
this is not a validator of arbitrary CAD meshing or printed-material strength.
"""
import argparse
import json
import subprocess
from pathlib import Path
import numpy as np
import scipy
import skfem
from skfem import MeshHex, ElementHex1, ElementVector, Basis, FacetBasis, LinearForm, condense, solve
from skfem.models.elasticity import linear_elasticity, lame_parameters


def compare(exe, source, conditions, fixture):
    config = json.loads(Path(conditions).read_text())
    actual = json.loads(subprocess.check_output([exe, 'cad', source, conditions], text=True, timeout=120))
    grid, result = actual['grid'], actual['result']
    origin, size, counts = [np.array(grid[k]) for k in ('origin_mm', 'size_mm', 'cells')]
    mesh = MeshHex.init_tensor(*[np.linspace(o, o+d, n+1) for o, d, n in zip(origin, size, counts)])
    centres = mesh.p[:, mesh.t].mean(axis=1)
    if fixture == 'bracket':
        inside = ~((centres[0] > 10) & (centres[1] > 10))
        expected_volume = 1200.
    elif fixture == 'post':
        inside = np.ones(mesh.t.shape[1], dtype=bool)
        expected_volume = 1200.
    else:
        raise ValueError('unknown fixture')
    expected_cells = {tuple(x) for x in np.floor((centres[:, inside].T-origin)/(size/counts)).astype(int)}
    if expected_cells != {tuple(x) for x in grid['active_cells']}:
        raise ValueError('CAD occupancy differs from independent fixture geometry')
    mesh = mesh.remove_elements(np.flatnonzero(~inside))
    basis = Basis(mesh, ElementVector(ElementHex1()), intorder=3)
    lam, mu = lame_parameters(config['young_mpa'], config['poisson'])
    matrix = linear_elasticity(lam, mu).assemble(basis)

    def region_facets(region):
        lo, hi = np.array(region['bounds_mm'])
        normal = np.array(region['normal'])
        axis = np.flatnonzero(normal)[0]
        boundary = mesh.boundary_facets()
        coords = mesh.p[:, mesh.facets[:, boundary]]
        included = ((coords >= lo[:, None, None]-1e-9) & (coords <= hi[:, None, None]+1e-9)).all(axis=(0, 1))
        facets = boundary[included]
        fb = FacetBasis(mesh, basis.elem, facets=facets, intorder=3)
        if not np.allclose(fb.normals[axis], normal[axis]):
            raise ValueError('reference facet normal mismatch')
        return facets, fb

    support, _ = region_facets(config['support'])
    _, loaded = region_facets(config['load'])
    area = float(loaded.dx.sum())
    total = np.array(config['force_n'])

    @LinearForm
    def traction(v, w):
        return sum(total[d]*v[d] for d in range(3))/area

    force = traction.assemble(loaded)
    fixed = basis.get_dofs(facets=support).all()
    u = solve(*condense(matrix, force, D=fixed))
    residual = matrix @ u-force
    support_nodes = np.unique(mesh.facets[:, support])
    reactions = residual[basis.nodal_dofs[:, support_nodes]].T
    reaction = reactions.sum(axis=0)
    moment = np.cross(mesh.p[:, support_nodes].T, reactions).sum(axis=0)
    coords_to_index = {tuple(np.round(p, 9)): i for i, p in enumerate(mesh.p.T)}
    actual_u, reference_u = [], []
    for node in result['nodes']:
        i = coords_to_index[tuple(np.round(node['position_mm'], 9))]
        actual_u.extend(node['displacement_mm'])
        reference_u.extend(u[basis.nodal_dofs[:, i]])
    actual_u, reference_u = np.array(actual_u), np.array(reference_u)
    # Evaluate reference derivatives at each element centre, independently of
    # CoScad's differentiation/stress recovery implementation.
    centre_basis = Basis(mesh, basis.elem, quadrature=(np.full((3, 1), .5), np.ones(1)))
    grad = centre_basis.interpolate(u).grad[:, :, :, 0]
    strain = (grad+grad.transpose(1, 0, 2))/2
    stress = 2*mu*strain+lam*np.einsum('iik->k', strain)[None, None, :]*np.eye(3)[:, :, None]
    stresses = stress[[0, 1, 2, 0, 1, 2], [0, 1, 2, 1, 2, 0]].T
    centres = mesh.p[:, mesh.t].mean(axis=1).T
    stress_lookup = {tuple(np.round(p, 9)): s for p, s in zip(centres, stresses)}
    actual_s = np.array([s['xx_yy_zz_xy_yz_zx_mpa'] for s in result['cell_stresses']])
    ref_s = np.array([stress_lookup[tuple(np.round(s['centre_mm'], 9))] for s in result['cell_stresses']])
    errors = {
        'all_displacements_relative_l2': float(np.linalg.norm(actual_u-reference_u)/np.linalg.norm(reference_u)),
        'all_centre_stresses_relative_l2': float(np.linalg.norm(actual_s-ref_s)/np.linalg.norm(ref_s)),
        'energy_relative': abs(result['strain_energy_n_mm']/float(u @ (matrix @ u)/2)-1),
        'reaction_absolute_n': float(np.linalg.norm(np.array(result['support_reaction_n'])-reaction)),
        'moment_absolute_n_mm': float(np.linalg.norm(np.array(result['support_moment_n_mm'])-moment)),
        'volume_absolute_mm3': abs(float(basis.dx.sum())-expected_volume),
    }
    passed = all(np.isfinite(list(errors.values()))) and all(v < (1e-5 if k=='moment_absolute_n_mm' else 1e-6) for k, v in errors.items())
    return {'source': source, 'conditions': conditions, 'cells': grid['cells'], 'elements': result['elements'],
            'load_average_displacement_mm': result['load_average_displacement_mm'],
            'max_cell_centre_von_mises_mpa': result['max_cell_centre_von_mises_mpa'], 'errors': errors, 'passed': bool(passed)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--exe', required=True)
    args = parser.parse_args()
    cases = [compare(args.exe, 'experiments/cad/l-bracket.coscad', f'experiments/cad/l-bracket-{level}.json', 'bracket')
             for level in ('coarse', 'medium', 'fine')]
    cases.append(compare(args.exe, 'examples/assemble/constraints/post.coscad', 'experiments/cad/existing-post.json', 'post'))
    passed = all(case['passed'] for case in cases)
    print(json.dumps({'schema': 'coscad.cad-fea-reference.v1', 'passed': passed,
                      'versions': {'scikit_fem': skfem.__version__, 'numpy': np.__version__, 'scipy': scipy.__version__},
                      'scope': 'Independent occupancy, Hex8 assembly, surface traction, all nodal displacements and cell-centre stresses; not physical validation.',
                      'cases': cases}, indent=2, allow_nan=False))
    raise SystemExit(0 if passed else 1)


if __name__ == '__main__':
    main()
