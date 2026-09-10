# Examples index

- `examples/` — small single-part `.coscad` files (shapes, booleans,
  transforms, bezier, zoo of all primitives, and `loft` — numeric
  bindings plus lofted funnel and lug).
- `examples/topological/` — pipeline syntax and anchor-relative
  modelling: `pipeline_basics`, `topo_tour` (annotated), `tbracket`
  (the zero-translation seam bracket, verified identical to its
  coordinate-built twin).
- `examples/assemble/` — `.assemble` specs: `corner`/`frame`
  (recursion; `rail200`/`rail120` are plain-bar stand-ins for the
  2020 extrusions, referenced ×0), `spill` (multi-bed: the design stage
  reports plate overflow, `coscad next` spills to more beds), `autotest`
  (orientation search), and `bow3/` — the flagship three-part
  manufactured assembly.
- `examples-next/` — earlier manufacturing-era parts (bow2 generation,
  greaser tool with its manifest kept as a reference output), and
  `example_check.assemble` with parts `a`/`b`/`c` — a `coscad check`
  demo whose lid sits 0.2 mm above the blocks, under the declared 0.3 mm
  clearance.
- `examples/archive/` — historical debug/test scratch files.

Generated `.scad`/`.stl` are gitignored; regenerate with
`coscad <file>` / `coscad next <file>`. Every example (outside
`archive/`) is compiled, rendered, and compared against
`test/golden/` by `stack test`; see README "Tests".
