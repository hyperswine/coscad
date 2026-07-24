# Examples index

- `examples/` — small single-part `.coscad` files (shapes, booleans,
  transforms, bezier, zoo of all primitives).
- `examples/topological/` — pipeline syntax and anchor-relative
  modelling: `pipeline_basics`, `topo_tour` (annotated), `tbracket`
  (the zero-translation seam bracket, verified identical to its
  coordinate-built twin).
- `examples/assemble/` — `.assemble` specs: `corner`/`frame`
  (recursion), `spill` (multi-bed), `autotest` (orientation search),
  and `bow3/` — the flagship three-part manufactured assembly.
- `examples-next/` — earlier manufacturing-era parts (bow2 generation,
  greaser tool with its manifest kept as a reference output).
- `examples/archive/` — historical debug/test scratch files.

Generated `.scad`/`.stl` are gitignored; regenerate with
`coscad <file>` / `coscad next <file>`.
