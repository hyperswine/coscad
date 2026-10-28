# Tesseract

The current version has 12 mm split corner hubs, 7 mm bars and integral
captured joints, with no screws or nuts. See [design, fit and assembly
instructions](../../../docs/2026-10-13-TESSERACT.md).

## Reported print result

On 28 October 2026 the owner confirmed that this previous, fastener-free
version **printed well on the A1**. The actual print date and exact sliced job
were not supplied in this observation. See [the outcome record](physical-outcome-a1-reported-20261028.json).
This is a useful comparison with the newer threaded tesseract's initial
one-bed failures; it does not identify why those newer attempts failed.

`tesseract.assemble` includes the removable build support and works with
`coscad next`, `coscad plan` and `coscad site`. `finished.assemble` shows the
37-piece final object. Use [the adhesion retry](adhesion-retry/README.md)
before a full set. It includes a broader cap foot and one supported full-length
bar, with an explicit A1 brim, speed and cooling profile. The original
three-piece coupon failed on the A1; that historical coupon failure is retained
separately from the owner-reported successful print above.

The previous 19 mm bolted design is preserved in `bolted/`.
