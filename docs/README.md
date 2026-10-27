# docs/: a chronological record

Kind: index. Every document in this directory is named
`YYYY-MM-DD-NAME.md`, dated by the day it was first written, so a listing of
the directory is the timeline of the project's thinking. A page says what was
true or intended when it was written; a later page may supersede it without
the earlier one being rewritten. When two pages disagree, the later date wins
unless the earlier page is one of the living references below.

`SKILL.md` is the one exception to the naming: it is the agent skill file
(gotchas and the verification workflow), and agent tools expect that name.
Raw data a page cites lives under `evidence/`, named after the page's date and
topic. `design/` holds reference PDFs.

## Adding a page

Name it with today's date and add one line to the end of the table below. Do
not rename a page when it changes: its date is when it began, and later
changes go in dated sections inside it. Pages here before 2026-10-24 keep the
date of their first commit; the reviews keep the date already in their name.

## Living references

These are kept current rather than superseded. The compiler's `--help` points
at the first three.

| page | what it holds |
|---|---|
| [2025-07-14-LANGEXTENSION](2025-07-14-LANGEXTENSION.md) | the full CoScad language reference |
| [2026-07-24-TOPOLOGICAL](2026-07-24-TOPOLOGICAL.md) | anchors, pipelines and the stable-datum rule |
| [2026-07-24-MANUFACTURING](2026-07-24-MANUFACTURING.md) | `.assemble` files and `coscad next` |
| [2025-08-11-EXAMPLES](2025-08-11-EXAMPLES.md) | index of the examples tree |
| [2026-08-17-SYNTAX](2026-08-17-SYNTAX.md) | the glyph and simple syntax modes |
| [2026-09-25-PLAN](2026-09-25-PLAN.md) | fasteners, material hints and `coscad plan` |
| [2026-10-15-PIPELINE-LOCK](2026-10-15-PIPELINE-LOCK.md) | pipeline pins, deliberate upgrades and recovery |
| [SKILL](SKILL.md) | the agent skill file |

## The record, oldest first

| date | page | what it is |
|---|---|---|
| 2026-10-01 | [FRONTEND-REVIEW](2026-10-01-FRONTEND-REVIEW.md) | review of the front end: parser, docs and diagnostics disagreements |
| 2026-10-01 | [LESSONS](2026-10-01-LESSONS.md) | lessons from the first prints |
| 2026-10-11 | [DIAGNOSTIC-AUDIT](2026-10-11-DIAGNOSTIC-AUDIT.md) | diagnostic audit: every error input and its response; data in `evidence/2026-10-11-diagnostic-data/` |
| 2026-10-11 | [ORBIT-TOWER](2026-10-11-ORBIT-TOWER.md) | hand-written assembly, handling and print guide for the Orbit Tower |
| 2026-10-12 | [DIAGNOSTIC-FIXES](2026-10-12-DIAGNOSTIC-FIXES.md) | the diagnostic fixes and the 443-probe CLI run; data in `evidence/2026-10-12-diagnostic-data/` |
| 2026-10-12 | [TOPOLOGICAL-FEATURES](2026-10-12-TOPOLOGICAL-FEATURES.md) | the first topological language features |
| 2026-10-12 | [TOPOLOGICAL-MIGRATION](2026-10-12-TOPOLOGICAL-MIGRATION.md) | migrating the examples to topological placement: initial findings |
| 2026-10-12 | [CUBE-CONSTRAINTS](2026-10-12-CUBE-CONSTRAINTS.md) | converting the 2020 cube to constraints |
| 2026-10-12 | [MATING-FRAMES](2026-10-12-MATING-FRAMES.md) | oriented mating frames and rigid assembly constraints |
| 2026-10-12 | [ASSEMBLY-MIGRATIONS](2026-10-12-ASSEMBLY-MIGRATIONS.md) | verifying the assembly frame migrations |
| 2026-10-12 | [FRAMED-FASTENERS](2026-10-12-FRAMED-FASTENERS.md) | frame-based fasteners and instruction repairs |
| 2026-10-13 | [TESSERACT](2026-10-13-TESSERACT.md) | the tesseract with captured joints |
| 2026-10-13 | [TESSERACT-INTERLOCK](2026-10-13-TESSERACT-INTERLOCK.md) | a slimmer, fastener-free tesseract |
| 2026-10-17 | [FEA-AND-BACKEND](2026-10-17-FEA-AND-BACKEND.md) | intent: `coscad stress` and a native geometry backend |
| 2026-10-26 | [FEA-BACKEND-EXPERIMENTS](2026-10-26-FEA-BACKEND-EXPERIMENTS.md) | independently checked CAD/beam FEA, pinned native CSG, acceptance evidence and remaining promotion gates |
| 2026-10-26 | [FEA-NATIVE-QUICKSTART](2026-10-26-FEA-NATIVE-QUICKSTART.md) | build and try native geometry and FEA on existing brackets and struts |
| 2026-10-27 | [STRESS-COMMAND](2026-10-27-STRESS-COMMAND.md) | `coscad stress`: support/load/stress lines, Haskell voxel Hex20 solver, material table, checks |
