# CoScad diagnostic audit, 11 October 2026

Follow-up: the [12 October fixes](2026-10-12-DIAGNOSTIC-FIXES.md) close all 15
remaining asserted gaps and add geometry and grammar validation. The results
below describe the original audit and its first increment.

Ran **419 CLI probes**: 169 curated cases (including 17 valid controls) and
250 deterministic single-character mutations of four small valid programs.
The corpus covers shape syntax, numeric/shape resolution, modes, Unicode and
encoding, continuations, anchors, lofts, assembly references and counts,
planner/fastener options, CLI misuse, and bounded nesting/long tokens.

The most valuable findings were **silent acceptance and wrong-cause errors**.
For example, a huge print count became zero, `at=oops` silently chose fastener
positions, and an empty definition consumed the next definition. A small first
increment fixes empty definitions, literal count overflow, planner options,
plan-line boundaries, and the CLI's UTF-8 failure message. Fastener metadata and
geometry validation still need work.

## Evidence and reproduction

- [Corpus and runner notes](../test/diagnostics/README.md)
- [Curated inputs](../test/diagnostics/cases.json)
- [Runner](../scripts/probe-diagnostics.py)
- [Before: every input and actual response](evidence/2026-10-11-diagnostic-data/before.json)
- [After: every input and actual response](evidence/2026-10-11-diagnostic-data/after.json)

Baseline: executable from `3247fe4`, before this diagnostic increment. The
existing bow model changes in the working tree are unrelated; every probe uses
its own tiny fixtures. Both runs used the same inputs. Baseline expectations
were subsequently evaluated against the final message assertions, without
rerunning or changing the recorded responses.

Each invocation uses its own temporary directory, a three-second timeout, and
one GHC runtime capability. No render, slicer, printer, or network command runs.
The stress cases stop at 256 nested parentheses, a 512-character identifier,
and a 512-digit literal. This is a reproducible finite audit, not exhaustive
fuzzing or a claim that arbitrary resource exhaustion is prevented.

| Outcome | Before | After |
|---|---:|---:|
| Successful exit | 109 | 92 |
| Rejected with a normal diagnostic | 309 | 327 |
| Raw runtime exception | 1 | 0 |
| Timeout | 0 | 0 |
| Unmet rejection/message expectations | 36 | 15 |

All **17 valid controls pass** in both runs. The mutations produce 211
rejections and 39 successful exits; an edit can preserve valid syntax, so those
39 successes are observations, not 39 bugs. The 15 remaining unmet expectations
are known validation gaps listed below. Exploratory geometry cases have no
blanket reject expectation because some boundaries, such as negative scale,
are useful and intentional.

## Improvements implemented in this pass

| Input | Actual baseline response | New response |
|---|---|---|
| `a =` followed by `main = ■ 10` | Error at line 2: `undefined variable 'main'`, with a malformed source excerpt | Error at line 1: `missing expression after '=' in 'a'; write a = <shape or number>` |
| `a ← part.coscad ×18446744073709551616` | Exit 0, assembled SCAD and manifest, no packed plate: the machine `Int` count wrapped to zero | Error at the literal: `part/fastener count is too large; maximum is 9223372036854775807` on this 64-bit host |
| `plan beam=oops`, `beam=0`, `beam=NaN`, `beam=Infinity`, fractional/overflowing beam or invalid `astar` | Exit 0 and a plan in these fixtures | Error at the option: expected a positive whole number, with an example; overflow names the bound |
| `plan rest=bto` | `no states left`, followed by planner-stuck/no-rest-face messages | `plan rest=bto: unknown rest face 'bto'; choose top, bot, lft, rt, fwd, or bak (aliases allowed)` |
| `plan rest=bot,tpo` | Exit 0: silently dropped `tpo` | Error naming the invalid face |
| `plan flip=-10`, `flip=oops`, `flip=NaN` | Accepted; negative flip produced a negative plan cost | Expected a finite, non-negative cost weight, with an example |
| `plan beem=30` or `plan ×5` | Accepted and ignored | Unknown plan option / plan takes `key=value` options |
| Invalid UTF-8 byte `FF` | `coscad: case.coscad: withFile: invalid argument (cannot decode byte sequence starting from 255)` | `Error: case.coscad: source is not valid UTF-8; save .coscad and .assemble files as UTF-8` |

Planner options are now confined to their statement line, so `plan` followed
by `asm = a` does not swallow `asm` as an option. Duplicate options on the same
`plan` line are rejected. Free manufacturing hints on parts remain supported,
as do `×0` premade parts, valid rest-face aliases, zero cost weights, and
fractional cost weights.

Validation: **`COSCAD_RENDER=0 stack test`: 229 passed, 0 failed**, including
24 added checks. All example SCAD snapshots still match. The final 419-probe
run has no raw exceptions or timeouts. The render/geometry tier was deliberately
not run: several inputs ask for absurd geometry, including an enormous `$fn`.

## Highest-value remaining work

| Reproducer / corpus ID | Observed response after this pass | Suggested diagnostic |
|---|---|---|
| `fastener M4x20 b a top nut=drpin` (`fastener-nut-typo`) | Exit 0; unknown nut value defaults to DropIn | `unknown nut kind 'drpin'; did you mean 'dropin'? Choose dropin, slidein, hex, or none` |
| `fastener peg b a top at=oops` (`fastener-at-nonnumeric`) | Exit 0; falls back to evenly spaced positions | `at= expects comma-separated finite distances in mm; got 'oops'` |
| `at=NaN`, `inset=NaN` | Exit 0 | Name the option and require finite distances before geometry/search |
| `torque=oops`, `torque=-2`, `through=oops` on a valid longer screw | Exit 0; malformed numbers default, negative torque is accepted | Name the option, units, allowed range, and offending value |
| `head=buton` | Exit 0; unsupported head is used in text and treated like a button head for clearance | `unknown head 'buton'; choose button or socket` |
| `fastener M0x0 b a top` (`screw-zero`) | Exit 0; treated as a peg because its length is zero | `screw diameter and length must be positive; use 'peg' for a peg joint` |
| `fastener peg b a top ×0` | Exit 0; creates one joint | `fastener count must be at least 1; remove the declaration to omit the joint` |
| Declare `a ← part.coscad` twice (`duplicate-part`) | Exit 0; packs two parts while the name table collapses the duplicate | Duplicate part declaration, naming both locations, like duplicate shape definitions |
| `plate 1e999 220 6`, `plate 220 220 -6` | Exit 0 | Width/depth must be finite and positive; margin finite and non-negative, leaving usable space |
| `clearance -1`, `clearance 1e999` | Exit 0 | `clearance must be a finite, non-negative distance in mm` |

Those rows account for the **15 remaining asserted gaps**. Further exploratory
probes show invalid part metadata can also be accepted (`mass=oops`,
`material=stel`, `ends=blockd,open`), or surface later as a misleading search
failure (`mass=NaN` reports tipping/stuck). Validate keys the planner consumes;
keep arbitrary manufacturing hints such as `seam=rear`. Duplicate keys across
multiple plan statements and duplicate part hints also still need a policy.

Count validation currently protects each **literal** before conversion to
`Int`. Recursive count multiplication and resource limits on huge but
representable counts still need separate checks; this pass does not establish
a safe maximum manufacturing quantity.

## Shape/grammar messages to improve next

| Input | Actual response | Better message |
|---|---|---|
| `main = box 10 20` | Generic `expecting a number ... or digit` at EOF | `box needs width, depth and height; missing height. Example: box 10 20 5` |
| `main = ■ 10 |> mov 1 2 3` | A long expected-stage list and `unexpected "mov 1 2"` | `unknown pipeline stage 'mov'; did you mean 'move'? Usage: |> move x y z` |
| `main = ■ 10 |> at top 1 2 (● 3)` | `expecting '$', '+', or a shape ...` at `1` | `at takes either a child shape, or three offsets followed by a child; only two offsets were given` |
| `!simpel` | `unexpected '!'; expecting end of input or letter` | `unknown syntax mode '!simpel'; choose !simple or !glyph as the first directive` |
| `main = cuube 10` | `undefined variable 'cuube'` | Preserve undefined-name error; suggest `cube` when a close keyword or binding exists |
| `a ← part.coscad seam=` | Caret at `seam`, with generic expected-token list | `missing value after 'seam='` at the value position |
| `main = χ −2 (■ 5)` | `unexpected '−'` | `Unicode minus U+2212 is not a numeric sign; replace it with '-'` |
| `coscad next` | `expected a .coscad file, got next` | `next needs one .assemble file. Usage: coscad next model.assemble` |

Successful compilation is also too permissive for some geometry. These were
compiled, **not rendered**:

- Negative radius/box dimensions, inverted tube radii, excessive rounding,
  fewer than three prism sides, zero scale and a zero mirror normal pass.
- `⎏ 3.5 5 10` silently emits `$fn = 4`.
- `⎏ 1e100 5 10` emits `$fn = 9223372036854775807` on this host. This is a
  particularly bad value to pass to a renderer; validate integral side counts
  and decide a practical resource bound before generating SCAD.
- Descending/equal loft heights pass; specify whether to reject, normalize or
  explicitly support them rather than assuming that compilation proves a solid.
- Negative scale remains a legitimate reflection. Zero chamfer/rounding disables
  a feature, and zero inner tube radius may be meaningful. Validate parameters
  by operation rather than applying a universal positivity rule.

## Error-message design

Prefer a short problem statement, the relevant parameter/units, and one local
repair. Keep `file:line:column` and the source caret. For example:

```text
model.coscad:4:17:
4 | main = box 10 20
  |                 ^
box is missing its height argument
Usage: box width depth height
Example: box 10 20 5
```

Use a nearest-name suggestion only when confidence is high; preserve the
original unknown name. Do not automatically repair the program or silently
change geometry. Distinguish invalid input from a valid assembly that has no
feasible sequence; invalid options should fail before search starts.

The next useful increment is strict fastener/consumed-metadata validation,
then parameter-aware shape errors and typo hints. Stable diagnostic codes and
a structured output mode would let the LSP test error kind/range independently
of prose. Keep precedence, bounding-box attachment semantics and print-versus-
placement counts as explicit design decisions, as discussed in the
[earlier frontend review](2026-10-01-FRONTEND-REVIEW.md).
