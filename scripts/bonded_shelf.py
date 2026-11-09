"""Deterministic shelf fixtures for bonded FEA; lengths mm, forces N.

No scientific packages are needed to generate the JSON input. The reference
comparison imports this helper but assembles its own finite elements.
"""
import argparse
import json
from pathlib import Path


def shelf_problem(refinement=1, right_modulus=2000.0):
    """Shelf 12x6x2 on two 2x8x2 cantilever arms, clamped behind shelf.

    Each arm projects from y=-2 to y=6, with a bonded top patch y=0..6.
    Shelf occupies z=2..4; arms z=0..2, at x=0..2 and x=10..12.
    Refinements 1,2,3 use 26,208,702 elements, preserving exact geometry.
    """
    if refinement not in (1, 2, 3):
        raise ValueError('refinement must be 1, 2 or 3 (1000-cell experiment budget)')
    n = refinement
    h = 2.0 / n

    def body(name, origin, counts, modulus):
        return dict(name=name, origin_mm=origin, cell_mm=h, counts=counts,
                    young_mpa=modulus, poisson=0.3)

    def patch(name, faces):
        return dict(body=name, faces=faces)

    bonds = []
    for name, start in [('left', 0), ('right', 5*n)]:
        arm = patch(name, [[i, j, n-1, 2, 1]
                          for i in range(n) for j in range(n, 4*n)])
        shelf = patch('shelf', [[i, j, 0, 2, -1]
                                for i in range(start, start+n) for j in range(3*n)])
        bonds.append(dict(name=name+'_bond', a=arm, b=shelf))
    return dict(
        schema='coscad.bonded-problem/1',
        bodies=[body('shelf', [0, 0, 2], [6*n, 3*n, n], 2000.0),
                body('left', [0, -2, 0], [n, 4*n, n], 2000.0),
                body('right', [10, -2, 0], [n, 4*n, n], right_modulus)],
        bonds=bonds,
        supports=[patch(name, [[i, 0, k, 1, -1] for i in range(n) for k in range(n)])
                  for name in ('left', 'right')],
        loads=[dict(patch=patch('shelf', [[i, j, n-1, 2, 1]
                                        for i in range(6*n) for j in range(3*n)]),
                    force_n=[0, 0, -1])],
        tolerance=1e-10, iterations=20000)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output-dir', type=Path, required=True)
    args = parser.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    for name, modulus in [('shelf-symmetric', 2000.0), ('shelf-soft-right', 500.0)]:
        (args.output_dir / (name+'.json')).write_text(
            json.dumps(shelf_problem(right_modulus=modulus), indent=2)+'\n')
