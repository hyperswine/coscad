# Long tweezers (2026-11-28)

310 mm PLA tweezers for picking things up about 25 cm away and for pinching a cloth around a lead
screw. Printed flat along the bed diagonal (241 mm square footprint), so the arms flex in the bed
plane and the bending stress runs along the extruded lines.

![design](preview.png)

- **Spring** (root to 100 mm): 4.7 mm wide, 12 mm tall, sets the closing force.
- **Beam** (100 to 285 mm): Warren truss tapering 14 to 7 mm, stiff so finger force reaches the tips.
- **Pads** 104-164 mm, ribbed. **Jaws** 285-310 mm with interleaved teeth; 26 mm apart at rest.
- Arms are unstressed when open (no creep at rest).

`sol design.sol` writes the models, `sol build.sol --printer x1c|a1|p1s|x2d` slices,
`sol dispatch.sol --printer P --dry-run` checks the printer.

## FEA (one arm, unit loads, superposed: `analysis.sol`, results in `fea/operating-points.txt`)

| pinch at jaws | finger force | peak von Mises | safety (50 MPa) |
|---|---|---|---|
| 0 N (touching) | 2.9 N | 7.2 MPa | 6.9 |
| 3 N | 12.0 N | 11.1 MPa | 4.5 |
| 5 N | 18.1 N | 13.8 MPa | 3.6 |

Each extra newton on the pads adds 0.33 N at the jaw. Sideways (z) flex is 3.1 mm per N at the jaw
(5.0 mm/N for the earlier 10 mm tall arms, `fea/h10/`). Linear small-strain model: fatigue, creep and
large deflection are not modelled.
