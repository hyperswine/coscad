# Diagnostic probes

`cases.json` contains 212 curated cases, including 30 valid controls, malformed
inputs, and exploratory geometry/metadata cases. `scripts/probe-diagnostics.py` adds 250 deterministic
single-character edits (seed `20261011`) and invokes the actual CLI in a fresh
temporary directory for each case. It never invokes rendering or printer commands.
Each process has a three-second timeout; logs returned in the JSON are capped at
16 KiB per stream. The stress cases stop at 256 nested parentheses, 512-character
identifiers and 512-digit numeric literals. This is a bounded diagnostic audit,
not an exhaustive fuzzer or a denial-of-service test.

```sh
stack build
python3 scripts/probe-diagnostics.py \
  --coscad "$(stack path --local-install-root)/bin/coscad" \
  --output /tmp/coscad-diagnostics.json
```

- `accept`: documented valid control, expected to exit successfully.
- `reject`: required rejection. `messages` optionally checks
  that the diagnostic explains the actual problem rather than a later symptom.
- `observe`: exploratory input. Acceptance alone does not indicate a bug; a
  character edit may still be valid, and negative scale is useful reflection.

The command returns **1 if any required rejection/message is missing**, or if
a process times out or emits a detected runtime exception. The expanded corpus
passes all 462 probes with oriented mating frames and assembly constraints.
The 19 new cases include 16 required rejections that also assert no output
artifacts were generated. Earlier results are in the
[follow-up fixes](../../docs/2026-10-12-DIAGNOSTIC-FIXES.md).
Run it alongside `stack test`; it is not yet wired into CI. The
[original audit](../../docs/2026-10-11-DIAGNOSTIC-AUDIT.md) preserves the
earlier failures and before/after responses.

Reports include input, fixture files, arguments, expected outcome, exit code,
stdout, stderr, duration and generated filenames. Temporary paths are normalized.
The runtime-exception classification is a heuristic; inspect the actual response.
