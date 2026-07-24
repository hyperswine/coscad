# coscad

A concise glyph/pipeline CAD language in Haskell that compiles to OpenSCAD
(optionally BOSL2), with an assembly layer and a manufacturing stage that
emits print-ready per-bed STLs. Intelligence lives upstream in the compiler
— anchors, beziers, attachments, fit, orientation, and packing are all
computed here so the emitted OpenSCAD (and the downstream slicer) stay dumb.

```
.coscad  ──compile──▶  .scad ──openscad──▶ .stl        (one part)
.assemble ─coscad───▶  asm view + plate + manifest      (design stage)
.assemble ─coscad next▶ bedN.stl + manifest             (manufacturing)
```

## Quick start

```sh
stack build            # or: ghc --make app/Main.hs -isrc -o coscad
coscad part.coscad     # -> part.scad (render with OpenSCAD; BOSL2
                       #    checkout must sit next to the .scad)
coscad frame.assemble  # -> _asm.scad, _plate.scad, per-part scads, manifest
coscad next frame.assemble   # -> frame_bed1.stl ... + manifest
                             # (set COSCAD_OPENSCAD to a wrapper, e.g.
                             #  xvfb-run, on headless machines)
```

## Two syntaxes, one language

Glyph style and pipeline style are interchangeable and mix freely:

```
// glyphs
bracket = (plate ⊕ flange) ⊖ χ 10 (zcyl 2.75 12)

// pipelines + word aliases
bracket = plate
  |> add flange
  |> cutat top 2.5 0 -4 (zcyl 2.75 12)
```

A line starting with `|>` continues the previous definition.

## Feature map

- **Shapes**: plain OpenSCAD glyphs (`■ ● ◎ ▻ ▬ ⎏`), BOSL2 family
  (`▣ ◙ ⌭ ⌽ ⊚ ⏢ ◉ ⊿`, `xcyl/ycyl/zcyl`), centered word shapes
  (`cube box sphere cyl tube torus wedge`), 2D profiles + `⮕`/`extrude`,
  and `✎` — compiler-evaluated piecewise cubic bezier outlines.
- **Booleans**: `⊕ ⊖ ∩ ⇓ ⊞ ↯` and pipeline stages `add cut isect hull mink`.
- **Topological modelling**: bbox anchors (`top bot lft rt fwd bak ctr`,
  combos like `lft+fwd`), relational ops `⌖`/`at`, `⋈`/`on`, `cutat`
  with offsets, and `⚓`/`anchor` re-origining. See docs/TOPOLOGICAL.md.
- **Assemblies**: `.assemble` files declare physically separate parts
  (recursive references, counts, print orientation `▽`, free hints).
  Union = one solid; separate reference = separate object.
- **Manufacturing**: `coscad next` compiles each unique
  (part, orientation) variant once, searches FFF print orientation
  where undeclared, packs beds largest-first with spill, and emits
  per-bed STLs plus a variants+placements manifest for slice-once /
  stamp-many slicing. See docs/MANUFACTURING.md.

## Module map

```
src/Coscad/Shape.hs     AST + bezier evaluation (pure data)
src/Coscad/Geometry.hs  vectors, bboxes, anchors, attachment resolve
src/Coscad/Codegen.hs   OpenSCAD emission
src/Coscad/Dsl.hs       Haskell-embedded glyph DSL
src/Coscad/Parser.hs    .coscad parser (glyphs, words, pipelines)
src/Coscad/Assemble.hs  .assemble design stage
src/Coscad/Next.hs      manufacturing stage (orientation, beds, manifest)
src/Lib.hs              backwards-compatible re-export shim
app/Main.hs             CLI dispatch only
```

## Docs

- docs/LANGEXTENSION.md — full language reference
- docs/TOPOLOGICAL.md — anchors, pipelines, and the stable-datum rule
- docs/MANUFACTURING.md — .assemble and coscad next
- docs/EXAMPLES.md — index of the examples tree
- docs/SKILL.md — agent skill file (gotchas + verification workflow)

## Verification culture

Every nontrivial part in the examples was validated numerically, not
visually: trimesh watertightness + volume/bounds cross-checks, boolean
interference tests for fits and fastener paths (expect 0 mm³), and
ASCII occupancy rasters for 2D outlines. docs/SKILL.md describes the
loop; copy it for new parts.
