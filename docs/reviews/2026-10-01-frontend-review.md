# CoScad front-end review, 1 October 2026

A pass over the parser, the expression grammar and the `.assemble`
grammar, driven by about sixty small probe files run through
`coscad 1.1.0.0` (commit 3020e8c). Each finding quotes the input and what
came out. Nothing was changed as part of this review.

**Verdict.** The core holds: every error carries `file:line:col`, forward
references and cycles are handled, duplicate definitions are rejected,
2D/3D mismatches are compile errors, and names of numbers and shapes
share one namespace with clear errors both ways. The weaknesses fall into
three groups: inputs that compile to wrong geometry without a word, inputs
that are accepted and ignored, and error messages that point at the wrong
cause. Nothing is undefined in the sense of unspecified behaviour; several
things are under-specified in practice.

## 1. Compiles silently to something wrong

| Input | Output | Problem |
|---|---|---|
| `w = 10` / `main = box w - 2 3` | `cuboid([10, -2, 3])` | A bare `-` in argument position is unary, so `w - 2` is two arguments. Arithmetic without parentheses is quietly reinterpreted. The rule is documented; the spaced form reads as subtraction to anyone. |
| `r = 10 / 0` / `main = sphere r` | `sphere(Infinity)` | No numeric validation at codegen. |
| `a = 0 / 0` / `main = box a 1 1` | `cuboid([NaN, 1, 1])` | Same. |
| `main = box -5 1 1` | `cuboid([-5, 1, 1])` | Negative sizes pass through. |
| `... \|> scale 0 1 1 \|> at top ...` | degenerate solid, child placed on its box | Zero scale passes through. |
| `box 10 10 10 \|> at lft+rt 0 0 0 (box 1 1 1)` | child at the centre | Opposite anchors sum to a zero vector; accepted. |
| `... \|> at top+top ...` | same as `top` | Repeated anchors accepted. |
| `box 10 10 10 \|> cut (box 20 20 20 \|> move 0 0 10) \|> at top ...` | child floats at z 5.5 | A difference keeps the positive's box, so the anchor is on removed material. Documented, and the single most common surprise in this project (it is also what misled the planner twice). |
| `box 1 1 1 ⊖ box 5 5 5 \|> at top ...` | empty solid, child at z 1 | An empty result still has a box. |
| `box 10 10 10 \|> roty 45 \|> at top ...` | child at z 7.57 | Anchors sit on the axis-aligned box of the rotated part, 2 mm above the geometry here. Documented. |

## 2. Accepted and silently ignored

| Input | What happens | Problem |
|---|---|---|
| `fastener M5x10 bar blk#1 rt nutt=hex thru=4` | plan runs with a drop-in T-nut and no `through`, then reports "only -30 mm of thread reaches the nut" | Unknown fastener options are free hints and are dropped; the error names a consequence, not the typo. |
| `blk ← blk.coscad ×1 material=printd` | material defaults | Unknown hint values are ignored. |
| `blk ← blk.coscad ×3` with two placements in `asm` | `coscad next` packs three parts, `coscad plan` plans two | The declared count and the placements are never compared, and the two commands disagree. |
| `cube = box 1 1 1` / `main = cube` | definition accepted, use fails as the primitive ("expecting a number") | Reserved words are not rejected at the definition. |
| `main = ●15` | `sphere(15)` | Works, while SKILL.md says it does not; docs and parser disagree. |

## 3. Grammar decisions that are consistent but surprising

- `⊕ ⊖ ∩ ⇓ ⊞ ↯` form one flat left-associative level:
  `A ⊕ B ⊖ C ∩ D` is `((A ⊕ B) ⊖ C) ∩ D`. Readers with a maths or OpenSCAD
  background expect intersection to bind tighter.
- `|>` is the loosest operator and cannot be followed by a glyph
  operator: `box 10 10 10 |> move 1 2 3 ⊕ box 1 1 1` is a parse error,
  while `box ⊕ box |> move 1 2 3` moves the union. The message says
  "expecting |> or end of input", not "parenthesise the pipeline".
- `$` extends to the end of the line, pipes included:
  `|> cut $ a ⊕ b |> move 5 0 0` moves the cutter, not the part.
- Glyph transforms bind to the next primary only:
  `χ 5 box 1 1 1 ⊕ box 2 2 2` translates the first box alone.
- A line cannot end with an operator; continuation works only when the
  next line starts with one. The trailing-operator case fails with
  "expecting '='" because the next line is read as a new definition.
- `_x` is not a valid name (leading underscore); `a1`, `Ab`, `top`, `x`
  are, and `x = 5` does not collide with the `x` pipe stage.

## 4. Error messages that mislead

| Input | Message | Better |
|---|---|---|
| `k = 2 ^ 3` | "unexpected 2 ^ 3, expecting a shape ..." | An unparsable numeric binding falls through to the shape parser. Say `^` is not an operator. |
| `main = 5 \|> move 1 0 0` | "unexpected 5 \|> move, expecting a shape" | Say a pipeline needs a shape on the left. |
| `!simple` then `!glyph` | "unexpected '!' expecting end of input or letter" | Say one pragma only. |
| `main = box 1 2` | "expecting a number ... or digit" | `box` knows its arity: "box takes 3 numbers, got 2". |
| `move −1 0 0` (U+2212) | "unexpected '−'" | Name the Unicode minus; it is what PDFs and some keyboards produce. |
| `blk ← blk.coscad x2` | "unexpected 'x'" | Suggest `×`. |

## 5. What held up

Juxtaposed arguments with parenthesised arithmetic are unambiguous once
the rule is known; `at top (w / 2) 0 0` works. Sixty nested parentheses,
scientific notation (`1e1 1E-1 1.5e+1`), tabs, and comments inside
continued expressions all parse. `box 1 1 1|>move 1 0 0` without spaces
parses. Two-dimensional misuse (`⭘ 5 ⊕ box`, `cutat` with a 2D cutter,
`extrude` on a solid, `↯` on a solid, a one-profile `loft`) all give
specific messages naming the fix. Shape-as-number and number-as-shape
each give a one-line explanation. `!simple` rejects glyphs. Duplicate
definitions name the first site. Unknown instance references
(`blk#3`) and a clamped part that is not on the named face are reported
with the fastener's line.

## 6. Suggested order of fixes

1. Reject a `-` followed by whitespace in argument position (or require
   parentheses), so `w - 2` cannot mean two arguments.
2. Validate numbers at codegen: finite, sizes positive, scale non-zero.
3. Reject degenerate anchor combinations and reserved-word definitions.
4. Make fastener options and the hints the planner reads closed sets;
   error on unknown keys and values.
5. Have `next` and `plan` agree on instance counts; error when `×n`
   does not match the placements.
6. Warn when an anchor is taken on a face that a cut removed, or on an
   empty result. (The box model itself can stay; the surprise is in the
   silence.)
7. The message fixes in section 4, and the `●15` doc line.
