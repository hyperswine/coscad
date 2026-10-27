#!/usr/bin/env python3
"""Independent check of the Hex20 voxel solver: the same 20 x 4 x 4 cantilever,
same cell size, same clamp and consistent end traction, assembled with scikit-fem's
20-node serendipity hexahedron and solved directly by SciPy. Compares the
traction-weighted tip displacement, reactions and strain energy.

  python scripts/compare-hex20-reference.py --exe $(cabal list-bin exe:coscad-experiments)

Requires the pinned optional environment (scikit-fem, numpy, scipy); it is a
validation tool, not part of the compiler.
"""
import argparse, json, subprocess
import numpy as np
from skfem import MeshHex, ElementHexS2, ElementVector, Basis, FacetBasis, LinearForm, condense, solve
from skfem.models.elasticity import linear_elasticity, lame_parameters


def reference(mode, ny):
    E, nu = 2000.0, (0.0 if mode == 'axial' else 0.3)
    h = 4.0 / ny
    nx = round(20 / h)
    mesh = MeshHex.init_tensor(np.linspace(0, 20, nx + 1), np.linspace(0, 4, ny + 1), np.linspace(0, 4, ny + 1))
    basis = Basis(mesh, ElementVector(ElementHexS2()), intorder=5)
    lam, mu = lame_parameters(E, nu)
    K = linear_elasticity(lam, mu).assemble(basis)
    end = FacetBasis(mesh, basis.elem, facets=mesh.facets_satisfying(lambda x: x[0] > 20 - 1e-9), intorder=5)
    force = np.array([1.0, 0, 0] if mode == 'axial' else [0, 0, -1.0]) / 16.0  # total 1 N over 16 mm^2

    @LinearForm
    def traction(v, w):
        return force[0] * v.value[0] + force[1] * v.value[1] + force[2] * v.value[2]
    f = traction.assemble(end)
    clamped = basis.get_dofs(facets=mesh.facets_satisfying(lambda x: x[0] < 1e-9)).all()
    u = solve(*condense(K, f, D=clamped))
    # traction-weighted average end displacement, per component
    comps = []
    for d in range(3):
        unit = np.zeros(3); unit[d] = 1.0 / 16.0

        @LinearForm
        def one(v, w, unit=unit):
            return unit[0] * v.value[0] + unit[1] * v.value[1] + unit[2] * v.value[2]
        comps.append(one.assemble(end))
    tip = np.array([float(np.dot(c, u)) for c in comps])
    reaction = (K @ u - f)[clamped]
    energy = 0.5 * float(u @ (K @ u))
    return {'tip': tip, 'reaction_total': float(np.sum(reaction[2::3]) if mode != 'axial' else np.sum(reaction[0::3])), 'energy': energy, 'dofs': int(K.shape[0])}


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--exe', required=True)
    ap.add_argument('--cells', type=int, nargs='+', default=[1, 2, 3])
    args = ap.parse_args()
    worst = 0.0
    for mode in ['axial', 'bend']:
        for ny in args.cells:
            actual = json.loads(subprocess.check_output([args.exe, 'hex20', mode, str(ny)], text=True, timeout=600))
            ref = reference(mode, ny)
            tip = np.array(actual['tip_displacement_mm'])
            err = float(np.linalg.norm(tip - ref['tip']) / np.linalg.norm(ref['tip']))
            e_err = abs(actual['strain_energy_n_mm'] - ref['energy']) / abs(ref['energy'])
            worst = max(worst, err, e_err)
            print(f"{mode} ny={ny}: coscad dofs {actual['degrees_of_freedom']} skfem dofs {ref['dofs']}  tip {tip.round(9).tolist()} vs {ref['tip'].round(9).tolist()}  rel {err:.2e}  energy rel {e_err:.2e}")
    print('worst relative difference', f'{worst:.2e}')
    if worst > 1e-6:
        raise SystemExit('Hex20 differs from the independent reference')


if __name__ == '__main__':
    main()
