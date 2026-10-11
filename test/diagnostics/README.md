# Diagnostic probes

`cases.json` contains 193 curated cases, including 27 valid controls, malformed
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
passes all 443 probes after the [follow-up fixes](../../docs/reviews/2026-10-12-diagnostic-fixes.md).
Run it alongside `stack test`; it is not yet wired into CI. The
[original audit](../../docs/reviews/2026-10-11-diagnostic-audit.md) preserves the
earlier failures and before/after responses.

Reports include input, fixture files, arguments, expected outcome, exit code,
stdout, stderr, duration and generated filenames. Temporary paths are normalized.
The runtime-exception classification is a heuristic; inspect the actual response.
