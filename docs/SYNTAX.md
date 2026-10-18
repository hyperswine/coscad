# CoScad syntax modes

A pragma on the first code line selects the vocabulary:

```
!simple    -- ASCII word set only
!glyph     -- glyph set only
(nothing)  -- legacy: both accepted (all existing files unchanged)
```

The pragma line is blanked, not stripped, so error line numbers stay right.

## Available everywhere (any mode)
- `|>` pipelines and all pipe-stage words (`x`, `y`, `z`, `rotx`, `move`,
  `scale`, `mirror`, `anchor`, `at`, `on`, `cutat`, `add`, `cut`, `isect`,
  `hull`, `mink`)
- `$` — Haskell-style loose application: every prefix form's shape argument
  may be `$ <rest of expression>`. Works after glyph transforms (`χ 5 $ a ⊕ b`),
  word transforms (`Translate.x 5 $ a * b`), and pipe stages (`|> cut $ ...`)
- lowercase primitive words: `xcyl ycyl zcyl cube box sphere cyl tube torus wedge`
- numeric bindings (`w = 20`, `r = w / 2`) and `loft z p z p ...` (see
  docs/LANGEXTENSION.md)
- anchor words: `top bot lft rt fwd bak ctr` (+ combos like `top+rt`)

## !simple vocabulary
Transforms (namespace style):
```
Translate (x, y, z) obj        Translate.x n obj   (.y .z likewise)
Rotate (x, y, z) obj           Rotate.z n obj      (tuple order: x then y then z)
Scale (x, y, z) obj            Scale.x n obj
Mirror (x, y, z) obj           Mirror.x obj        (no number)
Extrude h obj                  Anchor top obj
Loft z0 p0 z1 p1 ...           (2D profiles -> solid)
```
Combinators (prefix, two shape args; second may be `$`):
```
Hull a b    Union a b    Intersect a b    Minkowski a b    Offset n a
```
Operators: `*` union, `-` difference. Same flat left-assoc precedence as the
glyph operators: `a - b * c` is `(a - b) * c` — parenthesize when mixing.

Shapes: `Sphere r`, `Cube s`, `Box x y z`, `Cylinder r h`, `Cone r h`,
`Tube ro ri h`, `Torus rj rm`, `Wedge x y z`, `Prismoid x1 y1 x2 y2 h`,
`Circle r`, `Triangle r`, `Pentagon r`, `Bezier x y x y ...` (3k+1 points).
All centered (BOSL2 family). Note `Box` is centered, unlike legacy `▬`.

## Names and keywords

Names are resolved **by position**. Only the primitive shape words are
reserved; every other keyword is a keyword only in the place it is
recognised, and is an ordinary name everywhere else.

**Reserved** (cannot be a definition name or an `.assemble` part name):

| Mode | Reserved words |
|---|---|
| every mode | `cube box sphere cyl tube torus wedge xcyl ycyl zcyl loft` |
| `!simple` and legacy (no pragma) | also `Sphere Cube Box Cylinder Cone Tube Torus Wedge Prismoid Circle Triangle Pentagon Bezier Translate Rotate Scale Mirror Extrude Loft Anchor Hull Union Intersect Minkowski Offset` |

These are reserved because they start a shape wherever a shape may
appear. The parser tries shape words before names, so a binding called
`cyl` could never be referred to. `!glyph` files may use the capitalised
words as names (`Box = 2` is a number there). Glyphs (`●`, `▬`, …) are
symbols and never collide with names.

**Contextual** (usable as names; keywords only in these positions):

| Words | Recognised only |
|---|---|
| anchor words `top/up bot/dn/down rt/right lft/left fwd/front bak/back ctr/center`, combined with `+` | where a direction is expected: after `at`, `on`, `cutat`, `anchor` and `⚓`/`Anchor`; in `datum`/`place` points and `frame` normals; after `⌖` and `⋈`; `.assemble` `▽face`; and a `fastener` line's face word |
| stage words `x y z rotx roty rotz extrude move scale mirror anchor datum frame mate place grid loft at on cutat add cut isect hull mink` | directly after `\|>` |
| `.assemble` statement words `plate clearance fastener plan tolerance fixed mate` | at the start of an `.assemble` statement. Followed by `=` or `←`, the word is a definition or a part instead (`plate = box 5 15 20`) |

Named points and frames always carry a sigil (`@name`, `@inst.name`), so
they never compete with anchor words.

So these are all valid and mean what the renamed version would:

```
top = box 2 2 2
main = base ⌖ top top               // anchor top, then the shape top
x = 4
main = box 1 1 1 |> x x             // stage x, then the number x
top = 3
main = base |> at top top 0 0 c     // anchor top, offset x = 3 (legal, but hard to read)
center ← center3.coscad ×1          // an instance named like the ctr alias
```

Legal is not the same as clear: prefer names that do not double as
keywords where they would sit next to one.

**Adding words to the language.** A new anchor, stage or statement word
does not break anything, because those positions never hold a user's
name. A new primitive *shape* word (say a lowercase `cone` or `slot`)
must be reserved, so it is **a breaking change**: any file that binds
that name stops compiling. Add it to `reservedNames` in
`src/Coscad/Parser.hs` and note it as breaking in the CHANGELOG, or
choose a form that cannot collide (a glyph, or a capitalised `!simple`
word). The regression tests in `test/Tier/Keywords.hs` pin this rule.

## Known limits / notes
- Primitive shape words are reserved: you can't name a variable exactly `cyl`
  or (outside `!glyph`) `Translate`, but `cyl2` and `myTranslate` are fine.
  Every other keyword is contextual; see "Names and keywords" above.
- Errors are reported at `file:line:col` with the offending line quoted;
  a name that is not defined anywhere is an "undefined variable", names
  that depend on each other are a "circular dependency" (only the cycle
  members are listed), and 2D/3D mismatches are compile errors.
- `Rotate (x, y, z)` composes as Rz . Ry . Rx (OpenSCAD rotate([x,y,z]) order).
- `Translate (x,y,z)` emits one translate() vs three nested for χψζ chains —
  same geometry, cleaner scad.

# Assembly checking (`coscad check foo.assemble`)

`.assemble` files now take the same `!simple` / `!glyph` pragma (governs the
asm-side expressions; each referenced part file keeps its own pragma), plus:

```
clearance 0.15        -- required minimum gap in mm (0 = report contacts only)
tolerance 0.00001     -- how far a mating loop may miss and still close (mm [rotation])
```

`fixed` poses and `mate` `roll=`/`gap=` values take numeric atoms (literal,
binding, `-w`, `(w / 2)`); see docs/LANGEXTENSION.md, "Rigid assembly
constraints".

`coscad check` isolates each part inside the asm expression (all other parts
bound to Empty), renders real meshes, splits instances (a mirrored pair =
wing#1, wing#2), and for each AABB-close pair:
- exact overlap via OpenSCAD boolean on emitted polyhedra -> `OVERLAP` + volume
- CGAL instability on coplanar mating faces is detected (stderr+stdout scan),
  retried with swapped operands, else falls back to a grid-accelerated
  vertex-to-triangle distance verdict (annotated in the output)
- `contact` = touching mating faces (< 0.02mm); `WARN` = gap below clearance
Exit code 2 if any pair overlaps (the design has a problem), 0 otherwise;
1 if the check could not run (no `asm`, a part that fails to render). Distance verdicts are vertex-sampled: exact for
vertex-involved closest features; pathological edge-edge-only cases can read
slightly high.
