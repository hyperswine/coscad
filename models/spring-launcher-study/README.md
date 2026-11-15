# Spring launcher study (2026-11-15)

How fast can a spring launcher throw a small glider? This started from the
captive spring demonstrator (`../captive-spring-demo/three-bed`). Its PLA
spiral stores about 32 uJ over 10 mm travel, which is about 0.05 m/s. Wound
to 15 MPa it would store 3.6 mJ, still under 1 m/s.

Run `python3 study.py`. Every number below comes from that script, using
typical published material values. None of it has been measured on our
prints.

## Part 1: the ceiling for printed springs

Energy per gram = eta * sigma^2 / (2 E rho).

- **sigma (stress):** "repeatable" is many shots with short holds. "Near
  limit" is close to breaking or taking a set.
- **eta (geometry factor):**
  - 1 for pure tension, or an ideal thin-flange beam in pure bending.
  - 1/3 for a rectangular section at constant stress. A tapered bow limb and
    a spiral are both in this class.
  - 1/9 for a straight cantilever with an end load.

| Material | Tapered limb J/g, repeatable .. near limit | Top speed, no payload |
|---|---|---|
| PLA | 0.015 .. 0.047 | 17 m/s |
| PETG | 0.025 .. 0.077 | 21 m/s |
| PC | 0.054 .. 0.151 | 30 m/s |
| Nylon (PA12) | 0.064 .. 0.207 | 35 m/s |
| Music wire, helical body at 0.45 Sut | 0.40 | |

Low stiffness matters as much as strength. Nylon at near-limit stress gets
within a factor of two of steel per gram. Plain PLA is the worst of the four.

Flexing mass needed in a tapered limb, assuming an 8 g carriage:

| Target | PLA | PETG | PC | Nylon |
|---|---|---|---|---|
| 10 g glider @ 8 m/s | 123 .. 16 g | 40 .. 9 g | 13 .. 4 g | 11 .. 3 g |
| 20 g glider @ 12 m/s | impossible .. 87 g | 2 kg .. 38 g | 66 .. 16 g | 51 .. 11 g |
| 160 g bolted glider @ 10 m/s | impossible .. 277 g | 1 kg .. 140 g | 223 .. 63 g | 178 .. 44 g |

"Impossible" means that at repeatable stress, PLA cannot accelerate even its
own moving mass to that speed.

Caveats:
- Layer lines, voids and notches lower the real allowable stress.
- PLA and nylon creep if held drawn.
- Nylon's stiffness changes with moisture.
- Brittle PLA limbs that fail near their limit throw shards.
- A coupon test is needed before trusting the "near limit" column: draw-force
  curve, repeated shots, and set after a hold.

## Part 2: steel extension spring, pull-back carriage

A close-wound music wire extension spring. It has initial tension, so it
starts storing energy as soon as it is pulled. Assumptions:
- Machine hooks.
- Shigley's static limits: body torsion 0.45 Sut, hook bending 0.75 Sut,
  hook torsion 0.40 Sut.
- Stroke <= 150 mm, free length <= 160 mm, OD <= 16 mm.
- 8 g carriage.
- 70% of the stored energy goes into the moving parts.

| Target | Lightest spring found |
|---|---|
| 10 g @ 8 m/s (0.88 J) | 0.5 mm wire, 4.0 mm OD, 114 mm long, 143 mm stroke, 11 N, 3.7 g |
| 20 g @ 12 m/s (3.5 J) | 1.2 mm wire, 9.0 mm OD, 118 mm long, 102 mm stroke, 60 N, 19 g |
| 160 g @ 10 m/s (12 J) | 2 in parallel: 1.4 mm wire, 10.5 mm OD, 154 mm long, 133 mm stroke, 80 N each |

The hook twist limits almost every design. Two kinds of end take load
without a bent hook, and either would let the body run to its own limit:
- Screw-in plugs.
- Loop ends over a printed spool.

Speeds for a stock-like spring: 1.0 mm wire, 10 mm OD, 60 coils, 77 mm free
length, 121 mm stroke, 31 N at full draw, 2.1 J, 10.8 g.

| Glider | Speed | Climb with no drag |
|---|---|---|
| 5 g | 13.2 m/s | 8.9 m |
| 10 g | 11.6 m/s | 6.9 m |
| 20 g | 9.6 m/s | 4.7 m |
| 50 g | 6.9 m/s | 2.4 m |
| 160 g | 4.1 m/s (2 springs 5.8, 4 springs 8.0) | 0.9 m |

Design notes:
- **Separation:** the carriage stops at the spring's free length and the
  glider flies off a pin or hook.
- **End stop:** the carriage arrives with about 0.5 J at the stop on every
  shot (8 g at 11.6 m/s), so the stop needs a TPU bumper.
- **Latch:** the latch must hold the full draw force (31 N for the example).
- **Not checked:** spring surge, meaning whether the coils can follow a carriage
  this fast; rail friction; release losses.
