#!/usr/bin/env python3
"""Independent same-discretization comparison against scikit-fem + SciPy.

Install in an isolated environment: scikit-fem==12.0.2 numpy==2.3.5 scipy==1.18.1.
Uses scikit-fem's element, quadrature, elasticity assembly and boundary traction
integration, and SciPy's direct solver. Does not reuse CoScad stiffness code.
This validates implementation agreement, not mesh convergence or real material.
"""
import argparse
import json
import subprocess
import numpy as np
import scipy
import skfem
from skfem import MeshHex, ElementHex1, ElementVector, Basis, FacetBasis, LinearForm, condense, solve
from skfem.models.elasticity import linear_elasticity, lame_parameters


def reference(mode, nx, ny, nz):
    mesh = MeshHex.init_tensor(np.linspace(0, 20, nx+1), np.linspace(0, 4, ny+1), np.linspace(0, 4, nz+1))
    element = ElementVector(ElementHex1())
    basis = Basis(mesh, element, intorder=3)
    face = FacetBasis(mesh, element, facets=mesh.facets_satisfying(lambda x: np.isclose(x[0], 20)), intorder=3)
    axis, sign, nu = (0, 1., 0.) if mode == 'axial' else (2, -1., .3)
    lam, mu = lame_parameters(2000., nu)
    matrix = linear_elasticity(lam, mu).assemble(basis)

    @LinearForm
    def traction(v, w):
        return sign * v[axis] / 16.

    force = traction.assemble(face)
    fixed = basis.get_dofs(lambda x: np.isclose(x[0], 0)).all()
    displacement = solve(*condense(matrix, force, D=fixed))
    if not all(np.isfinite(x).all() for x in (matrix.data, force, displacement)):
        raise ValueError('independent solver produced non-finite values')
    residual = matrix @ displacement - force
    left = np.where(np.isclose(mesh.p[0], 0))[0]
    reaction = residual[basis.nodal_dofs[:, left]].sum(axis=1)
    moment = np.cross(mesh.p[:, left].T, residual[basis.nodal_dofs[:, left]].T).sum(axis=0)
    # Unit TOTAL force makes work = displacement averaged using end traction.
    tip = float(force @ displacement)
    return {'tip_in_load_direction_mm': tip, 'reaction_n': reaction.tolist(),
            'moment_n_mm': moment.tolist(), 'energy_n_mm': float(displacement @ (matrix @ displacement) / 2),
            'degrees_of_freedom': int(basis.N)}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--exe', required=True)
    args = parser.parse_args()
    cases = []
    passed = True
    for mode, nx, ny, nz in [('axial', 4, 2, 2), ('bend', 10, 2, 2), ('bend', 20, 4, 4), ('bend', 30, 6, 6)]:
        actual = json.loads(subprocess.check_output([args.exe, 'beam', mode, str(nx), str(ny), str(nz)], text=True, timeout=60))
        independent = reference(mode, nx, ny, nz)
        axis, sign = (0, 1.) if mode == 'axial' else (2, -1.)
        tip = sign * actual['tip_displacement_mm'][axis]
        errors = {'displacement_relative': abs(tip/independent['tip_in_load_direction_mm']-1),
                  'energy_relative': abs(actual['strain_energy_n_mm']/independent['energy_n_mm']-1),
                  'reaction_absolute_n': float(np.linalg.norm(np.array(actual['support_reaction_n'])-independent['reaction_n'])),
                  'moment_absolute_n_mm': float(np.linalg.norm(np.array(actual['support_moment_n_mm'])-independent['moment_n_mm']))}
        ok = (errors['displacement_relative'] < 1e-7 and errors['energy_relative'] < 1e-7
              and errors['reaction_absolute_n'] < 1e-6 and errors['moment_absolute_n_mm'] < 1e-5)
        passed = passed and ok
        cases.append({'case': mode, 'cells': [nx, ny, nz], 'reference': independent, 'errors': errors, 'passed': ok})
    print(json.dumps({'schema': 'coscad.fea-reference.v1', 'versions': {'scikit_fem': skfem.__version__, 'numpy': np.__version__, 'scipy': scipy.__version__},
                      'scope': 'Independent assembly and direct solve of the same fully integrated Hex8 discretization. Not physical validation.',
                      'passed': passed, 'cases': cases}, indent=2, allow_nan=False))
    raise SystemExit(0 if passed else 1)


if __name__ == '__main__':
    main()
