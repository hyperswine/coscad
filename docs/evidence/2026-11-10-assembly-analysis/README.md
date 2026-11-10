# Local assembly analysis validation, 2026-11-10

- `cabal build exe:coscad test:coscad-test --offline --ghc-options=-Werror`: passed.
- Focused `coscad-test --experiments-only`, with the newly built CLI on PATH and
  `COSCAD_RENDER=0`: 477 passed, 0 failed.
- First full native run (`COSCAD_OPENSCAD=none`, default runtime workers):
  1,717 passed, 1 failed. The sole failure was the existing 1,200-definition parser
  timing assertion: 0.877 seconds against a 0.5-second threshold. No threshold was
  changed. An isolated reproduction with two runtime workers took 0.293 seconds.
- Final full native rerun, with the newly built CLI on PATH,
  `COSCAD_OPENSCAD=none GHCRTS=-N2`: **1,718 passed, 0 failed**, including the
  tightened ambiguous-geometry assertion. This also passed the unchanged parser
  timing assertion. No OpenSCAD oracle was run.
- `python3 scripts/check-docs.py` and `git diff --check`: passed.
- [Independent reference comparisons](reference.json): all six passed through the
  `.assemble` CLI bridge. This file records the executable, source and fixture
  SHA-256 hashes, package versions, reference errors and refinement results.

Independent comparison command (Python environment from
`experiments/requirements-fea.txt`):

```sh
python scripts/compare-bonded-reference.py --assembly-exe /path/to/built/coscad \
  --output /tmp/coscad-assembly-reference.json
```

These are local numerical/software checks. Hosted CI, the OpenSCAD geometry oracle,
physical correlation and peak-stress convergence are not claimed.
