#!/usr/bin/env python3
"""Compare bonded shelves with independent scikit-fem Hex20 / SciPy solves.

Runs symmetric and soft-right fixtures at 2,1,2/3 mm. Reports per-node
agreement, energy, support and interface wrenches, equilibrium, load sharing
and refinement. A peak-stress trend is recorded but never certified converged.
Uses experiments/requirements-fea.txt, not core compiler dependencies.
"""
import argparse
import hashlib
import importlib.metadata
import json
import platform
import subprocess
import tempfile
import time
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
from scipy.sparse import block_diag, coo_matrix
from scipy.sparse.linalg import spsolve
from skfem import MeshHex, ElementHexS2, ElementVector, Basis, FacetBasis, LinearForm
from skfem.models.elasticity import linear_elasticity, lame_parameters
from bonded_shelf import shelf_problem

# Acceptance thresholds, fixed before comparing results. Force and moment
# errors use the input force and force*assembly extent as reference scales.
REFERENCE_TOL = 1e-6
BALANCE_TOL = 1e-7
REFINEMENT_TOL = 0.05
ROOT = Path(__file__).resolve().parents[1]


def reference(problem):
    """Independent quadrature/assembly per body; explicit equality reduction.

    scikit-fem selects exterior facets and provides quadratic DOF locations,
    traction integration and stiffness. No CoScad matrix, element gradients,
    node ordering, displacement or reaction is used here.
    """
    bodies = {}
    size = 0
    for spec in problem['bodies']:
        origin = np.asarray(spec['origin_mm'], dtype=float)
        h = spec['cell_mm']
        mesh = MeshHex.init_tensor(*[origin[d]+h*np.arange(spec['counts'][d]+1)
                                    for d in range(3)])
        basis = Basis(mesh, ElementVector(ElementHexS2()), intorder=5)
        lam, mu = lame_parameters(spec['young_mpa'], spec['poisson'])
        matrix = linear_elasticity(lam, mu).assemble(basis).tocsr()
        components = np.empty(basis.N, dtype=int)
        for d, indices in enumerate(basis.split_indices()):
            components[indices] = d
        boundaries = mesh.boundary_facets()
        centres = mesh.p[:, mesh.facets[:, boundaries]].mean(axis=1).T
        facet_map = {tuple(np.round(c, 10)): int(f) for c, f in zip(centres, boundaries)}
        bodies[spec['name']] = dict(spec=spec, basis=basis, matrix=matrix,
                                  rhs=np.zeros(basis.N), offset=size,
                                  components=components, facets=facet_map)
        size += basis.N

    def facets(patch):
        body = bodies[patch['body']]
        spec = body['spec']
        selected = []
        for i, j, k, axis, sign in patch['faces']:
            local = np.array([i, j, k], dtype=float)+0.5
            local[axis] += sign*0.5
            centre = np.asarray(spec['origin_mm'])+spec['cell_mm']*local
            selected.append(body['facets'][tuple(np.round(centre, 10))])
        if len(set(selected)) != len(selected) or not selected:
            raise ValueError('empty/duplicate reference patch')
        return np.array(selected)

    def dofs(patch):
        return bodies[patch['body']]['basis'].get_dofs(facets=facets(patch)).all()

    parents = np.arange(size)

    def root(i):
        while parents[i] != i:
            parents[i] = parents[parents[i]]
            i = parents[i]
        return i

    for bond in problem['bonds']:
        mappings = []
        for patch in [bond['a'], bond['b']]:
            b = bodies[patch['body']]
            mappings.append({(tuple(np.round(b['basis'].doflocs[:, i], 10)), int(b['components'][i])):
                             int(b['offset']+i) for i in dofs(patch)})
        if mappings[0].keys() != mappings[1].keys():
            raise ValueError('nonmatching reference interface')
        for key, a in mappings[0].items():
            b = mappings[1][key]
            parents[root(b)] = root(a)

    for load in problem['loads']:
        patch = load['patch']; b = bodies[patch['body']]
        face_basis = FacetBasis(b['basis'].mesh, b['basis'].elem,
                               facets=facets(patch), intorder=5)
        area = float(np.sum(face_basis.dx))
        traction = np.asarray(load['force_n'])/area

        @LinearForm
        def load_form(v, w):
            return sum(traction[d]*v.value[d] for d in range(3))
        b['rhs'] += load_form.assemble(face_basis)

    # P explicitly maps reduced unknowns to separate body DOFs. Non-interface
    # coincident coordinates are not merged.
    _, ids = np.unique([root(i) for i in range(size)], return_inverse=True)
    projection = coo_matrix((np.ones(size), (np.arange(size), ids)),
                            shape=(size, int(ids.max())+1)).tocsr()
    matrix = block_diag([b['matrix'] for b in bodies.values()], format='csr')
    rhs = np.concatenate([b['rhs'] for b in bodies.values()])
    reduced = (projection.T @ matrix @ projection).tocsr()
    reduced_rhs = projection.T @ rhs
    fixed = np.unique(np.concatenate([ids[bodies[p['body']]['offset']+dofs(p)]
                                      for p in problem['supports']]))
    free = np.setdiff1d(np.arange(reduced.shape[0]), fixed)
    u = np.zeros(reduced.shape[0])
    u[free] = spsolve(reduced[free][:, free], reduced_rhs[free])
    full_u = projection @ u
    residual = np.linalg.norm((reduced @ u-reduced_rhs)[free])/np.linalg.norm(reduced_rhs[free])
    if not np.isfinite(full_u).all() or residual > 1e-8:
        raise ValueError('independent solve did not converge')

    def wrench(body, indices, forces):
        vectors = np.zeros((len(indices), 3))
        vectors[np.arange(len(indices)), body['components'][indices]] = forces[indices]
        coords = body['basis'].doflocs[:, indices].T
        return dict(force_n=vectors.sum(axis=0).tolist(),
                    moment_n_mm=np.cross(coords, vectors).sum(axis=0).tolist())

    nodes = {}; supports = {}; energies = {}
    for name, b in bodies.items():
        local = full_u[b['offset']:b['offset']+b['basis'].N]
        b['residual'] = b['matrix'] @ local-b['rhs']
        values = {}
        for i in range(b['basis'].N):
            key = tuple(np.round(b['basis'].doflocs[:, i], 10))
            values.setdefault(key, np.zeros(3))[b['components'][i]] = local[i]
        nodes[name] = values
        patches = [p for p in problem['supports'] if p['body']==name]
        indices = np.unique(np.concatenate([dofs(p) for p in patches])) if patches else np.array([],dtype=int)
        supports[name] = wrench(b, indices, b['residual'])
        energies[name] = float(local @ (b['matrix'] @ local)/2)
    interfaces = {}
    for bond in problem['bonds']:
        interfaces[bond['name']] = {side: wrench(bodies[bond[patch]['body']], dofs(bond[patch]),
                                               bodies[bond[patch]['body']]['residual'])
                                    for side, patch in [('on_a','a'),('on_b','b')]}
    return dict(nodes=nodes, supports=supports, interfaces=interfaces,
                energy=sum(energies.values()), body_energy=energies,
                residual=float(residual), dofs=reduced.shape[0])


def magnitude(wrench, field):
    return float(np.linalg.norm(wrench[field]))


def compare(problem, result, ref):
    force_scale = sum(np.linalg.norm(load['force_n']) for load in problem['loads'])
    # Absolute-position-independent characteristic size for moment errors.
    lows = np.array([b['origin_mm'] for b in problem['bodies']])
    highs = np.array([np.array(b['origin_mm'])+b['cell_mm']*np.array(b['counts']) for b in problem['bodies']])
    moment_scale = force_scale * float(np.max(highs.max(axis=0)-lows.min(axis=0)))
    disp_scale = max(np.linalg.norm(u) for ns in ref['nodes'].values() for u in ns.values())
    if {b['name'] for b in result['bodies']} != set(ref['nodes']):
        raise ValueError('body identities differ from reference')
    if {i['name'] for i in result['interfaces']} != set(ref['interfaces']):
        raise ValueError('interface identities differ from reference')
    node_error = max(np.linalg.norm(np.array(n['displacement_mm']) -
                                   ref['nodes'][b['name']][tuple(np.round(n['position_mm'],10))])
                     for b in result['bodies'] for n in b['nodes']) / disp_scale
    # Check identity/count coverage as well as values.
    for b in result['bodies']:
        if (len(b['nodes']) != len(ref['nodes'][b['name']]) or
                {tuple(np.round(n['position_mm'],10)) for n in b['nodes']} != set(ref['nodes'][b['name']])):
            raise ValueError('reference and CoScad node counts differ')
    force_errors = []; moment_errors = []

    def compare_wrench(actual, expected):
        force_errors.append(float(np.linalg.norm(np.array(actual['force_n'])-expected['force_n']))/force_scale)
        moment_errors.append(float(np.linalg.norm(np.array(actual['moment_n_mm'])-expected['moment_n_mm']))/moment_scale)

    for b in result['bodies']:
        compare_wrench(b['support'], ref['supports'][b['name']])
    for i in result['interfaces']:
        for side in ['on_a','on_b']:
            compare_wrench(i[side], ref['interfaces'][i['name']][side])
    balances = [result['equilibrium_residual']] + [i['balance_residual'] for i in result['interfaces']]
    metrics = dict(node_displacement_relative=float(node_error),
                   energy_relative=abs(result['strain_energy_n_mm']/ref['energy']-1),
                   body_energy_relative=max(abs(b['strain_energy_n_mm']/ref['body_energy'][b['name']]-1) for b in result['bodies']),
                   wrench_force_normalized=max(force_errors), wrench_moment_normalized=max(moment_errors),
                   force_balance_normalized=max(magnitude(w,'force_n')/force_scale for w in balances),
                   moment_balance_normalized=max(magnitude(w,'moment_n_mm')/moment_scale for w in balances))
    metrics['passed'] = (result['degrees_of_freedom']==ref['dofs'] and
                         all(v <= (BALANCE_TOL if 'balance' in k else REFERENCE_TOL) for k,v in metrics.items()))
    return metrics


def summarize(result):
    shelf = next(b for b in result['bodies'] if b['name']=='shelf')
    probe = next(n['displacement_mm'] for n in shelf['nodes'] if np.allclose(n['position_mm'], [6,6,4], atol=1e-10, rtol=0))
    supports = {b['name']:b['support'] for b in result['bodies'] if b['name']!='shelf'}
    interfaces = {i['name']:{s:i[s] for s in ['on_a','on_b']} for i in result['interfaces']}
    return dict(probe_displacement_mm=probe,
                max_displacement_mm=max(float(np.linalg.norm(n['displacement_mm'])) for b in result['bodies'] for n in b['nodes']),
                energy_n_mm=result['strain_energy_n_mm'], supports=supports, interfaces=interfaces,
                left_vertical_share=supports['left']['force_n'][2]/(-result['applied']['force_n'][2]),
                peak_von_mises_mpa=max(c['peak_von_mises_mpa'] for b in result['bodies'] for c in b['cells']),
                dofs=result['degrees_of_freedom'],iterations=result['iterations'],residual=result['free_residual_relative'])


def refinement_change(previous, current):
    """Signed-vector differences; near-zero quantities use absolute scales.

    Displacement relative to finest probe norm; interface force/moment changes
    relative to that interface's finest norm (floors 1e-6 N, 1e-6 N mm).
    """
    def change(a,b,floor):
        return float(np.linalg.norm(np.array(a)-b)/max(np.linalg.norm(b),floor))
    metrics = dict(probe_displacement=change(previous['probe_displacement_mm'],current['probe_displacement_mm'],1e-12))
    for name, sides in current['interfaces'].items():
        for field, floor in [('force_n',1e-6),('moment_n_mm',1e-6)]:
            metrics[name+'_'+field] = change(previous['interfaces'][name]['on_b'][field], sides['on_b'][field], floor)
    return dict(relative_changes=metrics, passed=all(v<REFINEMENT_TOL for v in metrics.values()))


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--exe', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    exe = args.exe.resolve()
    report = dict(schema='coscad.bonded-reference/1', utc=datetime.now(timezone.utc).isoformat(),
                  executable_sha256=sha(exe), python=platform.python_version(),
                  git_head=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
                  packages={p:importlib.metadata.version(p) for p in ['numpy','scipy','scikit-fem']},
                  thresholds=dict(reference=REFERENCE_TOL,balance=BALANCE_TOL,refinement=REFINEMENT_TOL),
                  source_sha256={p:sha(ROOT/p) for p in ['scripts/compare-bonded-reference.py','scripts/bonded_shelf.py',
                                                       'src/Coscad/Stress/Bonded.hs','src/Coscad/Stress/Hex20.hs','src/Coscad/Stress/Sparse.hs']},
                  cases=[], refinement=[], physical_validation=False, peak_stress_convergence_claimed=False)
    for label, modulus in [('symmetric',2000.0),('soft-right',500.0)]:
        summaries=[]
        for n in (1,2,3):
            problem=shelf_problem(n,modulus)
            with tempfile.TemporaryDirectory(prefix='coscad-shelf-') as directory:
                path=Path(directory)/'problem.json'; path.write_text(json.dumps(problem))
                started=time.perf_counter()
                run=subprocess.run([str(exe),'bonded',str(path)],capture_output=True,text=True,timeout=600,check=True)
                actual=json.loads(run.stdout)
                elapsed=time.perf_counter()-started
            ref=reference(problem)
            comparison=compare(problem,actual,ref)
            summary=summarize(actual); summaries.append(summary)
            share=summary['left_vertical_share']
            share_ok = abs(share-0.5)<1e-7 if label=='symmetric' else 0.5<share<1.0
            case=dict(case=label,refinement=n,cell_mm=2/n,elements=26*n**3,
                      problem_sha256=hashlib.sha256(json.dumps(problem,sort_keys=True).encode()).hexdigest(),
                      comparison=comparison,summary=summary,reference_residual=ref['residual'],
                      reference_energy_n_mm=ref['energy'],
                      reference_probe_displacement_mm=ref['nodes']['shelf'][(6.0,6.0,4.0)].tolist(),
                      reference_supports=ref['supports'],reference_interfaces=ref['interfaces'],
                      load_sharing_passed=share_ok,
                      coscad_wall_seconds=elapsed)
            report['cases'].append(case)
            print(f'{label} n={n}: left share {share:.6f}, probe z {summary["probe_displacement_mm"][2]:.9g} mm, '
                  f'node error {comparison["node_displacement_relative"]:.2e}, comparison {comparison["passed"]}',flush=True)
        steps=[refinement_change(summaries[i-1],summaries[i]) for i in (1,2)]
        report['refinement'].append(dict(case=label,coarse_to_medium=steps[0],medium_to_fine=steps[1]))
    # Promotion requires the finest adjacent pair to settle. Coarse-to-medium
    # changes are retained, not hidden, but need not pass the final threshold.
    report['passed'] = all(c['comparison']['passed'] and c['load_sharing_passed'] for c in report['cases']) and all(r['medium_to_fine']['passed'] for r in report['refinement'])
    args.output.parent.mkdir(parents=True,exist_ok=True)
    args.output.write_text(json.dumps(report,indent=2,allow_nan=False)+'\n')
    print('PASS' if report['passed'] else 'FAIL',args.output,flush=True)
    if not report['passed']:
        raise SystemExit(1)


if __name__=='__main__':
    main()
