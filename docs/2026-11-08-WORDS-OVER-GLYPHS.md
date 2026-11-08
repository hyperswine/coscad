# Words over glyphs: whitepaper

Kind: whitepaper, direction. Written 8 November 2026 against main with the
architecture review of the same day already landed. It records what the
glyph system was for, what the editor and the docs tooling now do better,
the tax the dual vocabulary pays, and a pivot: the `!simple` word
vocabulary becomes the primary written form of CoScad. Nothing here is
implemented yet; each follow-up gets its own dated page.

## The original ideals

CoScad began in July 2025 as a Haskell library with "Unicode Glyph
Functions": one-character aliases (`■`, `◎`, `χ`) for the embedded shape
API (f4cedf9), because in Haskell source a primitive you use fifty times
a day earns a single keystroke. The file language grew around those
aliases (2025-07-14-LANGEXTENSION), and the glyphs carried three ideals
into it:

1. **Concision.** A model line is geometry, not ceremony: `◎ 29 4` reads
   faster than `cylinder(radius = 29, height = 4)`. The glyphs made dense
   construction lines short and visually distinct, closer to maths than
   to code.
2. **One meaning per symbol.** Every glyph is one concept, documented in
   one table, never overloaded the way `*` or `-` are in general-purpose
   languages.
3. **Readability at a glance.** `⊕ ⊖ ∩ ⇓` are the operations you already
   know from set notation; a line of them scans like a formula.

Around them stood the ideals that were never about glyphs at all: the
pipeline (`|>`, 2025-07-14), resolution by dependency rather than order,
anchors and stable datums for topology (2026-07-24), intelligence in the
compiler, and one vocabulary per file chosen by a pragma (2026-08-17-SYNTAX).
The README still opens with "a concise glyph/pipeline CAD language".

## What the ideals bought

The record shows what worked, and it is worth separating from the
notation it rode in on. The pipeline, the resolver, the refusal
discipline (every error names a fix), the wrapper features that compose
in one-line increments (datums, frames, joints), the native kernel as
the whole build (1.6.0.0), the planner on real meshes, and a stdlib that
dogfoods the user surface: all of these are the reason the language is
pleasant, and none of them is a glyph. The 2026-11-08 architecture
review's verdict was about exactly this split: the spine is clean; the
cost is concentrated at the leaves.

## What the tooling now does better

The IDE does the three jobs the glyphs were invented for, and it does
them with more information than a symbol can carry:

- **The editor renders the shapes inline.** `vscode/shape-visuals.js`
  plus `shape-icons.json` put a pre-rendered miniature beside every
  primitive in the buffer, sized to the font. A miniature shows the one
  thing a name cannot: the geometry, including the anchoring convention
  that distinguishes `box` from `▬`. Hover (hover-info.js) documents
  every glyph, word and keyword in place; the LSP completes them
  context-aware; the parser mirror behind all of it is regression-tested
  (139 checks).
- **Names are searchable; symbols are not.** Dash, the man page, grep,
  and this repository's own search all work on keywords: `xcyl`,
  `frustum`, `cutat` are findable, and none of `⌭ ⊚ ⎏` can even be
  typed into a search field without copy-paste.
- **Completion removes the concision argument.** With the editor
  offering `frustum` after two keystrokes, the difference between `⌭`
  and `frustum` at the keyboard is nil; without the editor, the glyph is
  not typeable at all on most keyboards, while the word is.

So the ideals migrate rather than die: concision becomes definitions
plus completion, one-meaning-per-symbol becomes one-name-per-concept in
a searchable table, and readability-at-a-glance becomes a rendered
miniature. The glyph's remaining edge is handwriting, chat messages and
column width in dense one-liners; that edge no longer justifies the
cost.

## The tax the dual vocabulary pays

Measured in the 2026-11-08 architecture review; restated here because
the pivot is the fix for it:

- Five hand-written choice lists in the grammar; a primitive is spelled
  up to three times (`sphere`: glyph, word, capitalized); operator
  labels, suggestion lists and reserved words are hand-kept in triplicate.
- Every primitive is a case in about 25 walkers; adding one is a silent
  13-module checklist, plus four editor files and five documentation
  surfaces (revolve: 34 files).
- The editor mirror is a 1,226-line hand-maintained reimplementation of
  the grammar, a textual contract with no guard.
- Unicode brings its own error classes: the U+2212 minus that PDFs
  produce (frontend review, section 4), and the χ/ψ/ζ family that has
  already been swapped once (SKILL.md's internal notes).

## The pivot: words as the primary written form

The word vocabulary is not a proposal; it already exists and is the
backbone of the language. The pipeline, every stage word, `$`, the
lowercase primitives, anchor words and numeric bindings work in all
three modes today. What changes is which form we write, teach and grow.

### Coverage today

| concept | words (all modes unless noted) |
|---|---|
| pipeline | `\|>` and all stages: `x y z rotx roty rotz extrude revolve move scale mirror anchor datum frame mate place grid loft at on cutat add cut isect hull mink` |
| booleans | `*` union, `-` difference; prefix `Union Intersect Hull Minkowski Offset` (!simple), stages `add cut isect hull mink` |
| transforms | `Translate/Rotate/Scale/Mirror` with `.x/.y/.z` (!simple) |
| centered solids | `cube box sphere cyl tube frustum torus wedge`, `xcyl ycyl zcyl`, `Prismoid` (!simple) |
| 2D | `Circle Triangle Pentagon Bezier` (!simple), `△ ⬠ ⭘ ✎` (glyph) |
| stdlib | `thread_male thread_female globe_stud globe_socket clearance_pin clearance_bore captive_bearing` |

The one real gap: the **bed-anchored family has no words**. `■ ▬ ◎ ▻ ⎏`
sit on the plane like printed parts do, and the words that sound like
them (`box`, `cyl`) deliberately name the centered BOSL2 forms. Note
`frustum r1 r2 h` (2026-11-08) is bed-anchored and word-only, so the
family already has one member spelled the intended way.

### Closing the gap

Give the five bed-anchored primitives words, with two constraints: no
collision with the centered words, and the anchoring visible in
documentation (the editor's miniatures show it for free). Two schemes:

- **Semantic** (recommended): names that say what sits where.
  `block x y z` (and one-argument `block s` for `■`), `pillar r h`,
  a bed-anchored cone (candidates: `spike`, `spire`; undecided),
  and `prism n r h`, which is already the name the Haskell DSL uses.
- **Systematic**: `obox ocyl ocone oprism`, "o" for at-the-origin
  OpenSCAD anchoring; predictable, but reads like a code, not a name.

Final names get their own dated page; `frustum` avoided this question
only because its centered twin does not exist.

### Grammar and editor consolidation

With words primary, `wordShape` becomes the single growth surface: a new
primitive lands as one table entry in the grammar, one entry in the
editor, one row in the reference tables. The glyph lists freeze: no new
glyphs, no new glyph documentation, legacy maintenance only. That is the
architectural payoff of the pivot — the add-a-primitive checklist
shrinks from five spelling lists to one, and the JS mirror's growth
surface shrinks with it. (The deeper consolidation — one data-driven
table driving both — stays on the architecture review's list; this
pivot makes it cheaper, not harder.)

### A words renderer for the corpus

The compiler holds the AST, so a `coscad words file.coscad` renderer
that pretty-prints a shape tree back as words is cheap, and a round-trip
test (`words` output reparses to the same tree) guards it. Existing
models, examples and manual listings can then be translated mechanically
where wanted; files are not rewritten wholesale — legacy mode parses
both forever — but anything touched for a print or a doc gets rendered
words as it passes through.

### Documentation order

All new pages, examples, editor snippets and templates are written
words-first, with the glyph in parentheses where the existing tables
pair them (`pillar r h` (◎)). `SKILL.md` gains an authoring rule: agents
write words — they paste text, and unicode input is friction for them
for the same reason it is for keyboards. The man page, already the Dash
source, leads with words in its examples.

### What stays non-ASCII

`.assemble` line syntax keeps `←` (part), `×` (count) and `▽`
(orientation), and `@`/`#` for datums and instances. They are few,
line-structural, hover-documented, and not primitive spellings; word
spellings (`part <-`, `x3`) can come later if they ever annoy.

## Challenges and risks

- **Two spellings forever.** The compat promise means glyph support is
  never removed. The mitigation is freezing: frozen lists stop growing,
  so the duplication stops compounding.
- **The corpus stays glyphy.** Models and evidence pages are records of
  what was printed; translating them is optional and mechanical when
  wanted, never a migration gate.
- **Naming collisions.** `block` and `pillar` are ordinary words; the
  keyword-wins-over-identifier rule (the `cube` precedent) already
  governs them, and `frustum` just joined that list without incident.
- **A third way of writing.** If new words land but docs keep leading
  with glyphs, the language fragments. The documentation-order rule
  above is the guard, and it applies to this whitepaper's own examples.
- **Muscle memory.** The owner's own models are the hardest corpus;
  the words renderer exists precisely so nothing needs retyping.

## What does not change

Notation only. Words and glyphs build the same `Shape` tree, so
geometry, goldens, plans, manifests, stress reports and the pipeline
semantics are untouched. The ideals that made the language good —
pipeline, dependency-ordered resolution, refusals that name a fix,
topology on stable datums, intelligence in the compiler — were never
glyph properties and are all kept. Concision survives as definitions:
the corpus already writes `plate = ◎ 29 4`, and `plate = pillar 29 4`
is the same sentence with a searchable headword.

## Suggested order of work

1. Name the bed-anchored words (dated page), add them to `wordShape`,
   the editor mirror and the reference tables, with the render sweep
   and round-trip tests.
2. `coscad words` renderer + round-trip test.
3. Flip new documentation, examples, snippets and the editor template
   to words-first; add the SKILL.md authoring rule.
4. Freeze the glyph lists in code and in 2026-08-17-SYNTAX.md (new
   primitives are words only, `frustum` as the precedent).
5. Optional, only if missed: `&` as the ASCII intersection operator,
   sugar over `Intersect`/`isect`, which already exist.

## Bed-anchored words and the renderer, 8 November 2026

The naming question resolved to the semantic scheme, and the words landed
with the tool that forced them: `coscad words`. `block x y z` (■ ▬),
`pillar r h` (◎), `spike r h` (▻), `prism n r h` (⎏) and `poly x0 y0 ...`
join `frustum` in the all-modes word list, each reserved like its
siblings and documented everywhere the family is. The renderer
(`coscad words file [-o out]`, `Coscad.Words`) turns a whole file back
as word syntax: definitions keep their lines, numbers stay numbers,
transforms become pipeline stages, and the round-trip suite reparses and
meshes every case. What cannot be spelled in words yet is an honest
error naming the construct: chamfered or rounded BOSL2 primitives,
arbitrary-axis rotations and matrix transforms, mating frames, tags, and
profiles beyond Triangle/Pentagon/Circle. Those are the spellings to add
as the corpus asks for them, one table entry each.
