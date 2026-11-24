# Ribbed Mk3 wing study

The wing has a 6 mm thick root carried through the fuselage, tapering to 3 mm by span station 20 mm. Dimensions here are construction dimensions before the final 1.035 scale. Subtracting trapezoidal closed bays leaves 0.60 mm upper and lower skins, spanwise webs at chord fractions in tenths, and chordwise ribs between bays. The front and rear fuselage cavities and the swept tail come from the balanced Mk3.

Run `sol build.sol` here, then render `glider.coscad` with CoScad. The A1 package was sliced for **standard PLA**, two walls, 8% lightning and tree supports. The matched reference is the balanced Mk3 lightning slice. The LW-PLA figures use mass scaling, not a calibrated LW-PLA printer preset.

| Quantity | Original | Ribbed |
|---|---:|---:|
| Modeled void fraction of complete right wing before union with body | 16.6% | 31.7% |
| Standard PLA airborne mass from model extrusion | 44.88 g | 46.21 g |
| Total plate consumption | 78.41 g | 79.25 g |
| Cantilever deflection | 0.441 mm | 0.449 mm |
| Peak von Mises stress | 1.83 MPa | 1.97 MPa |

The FEA compares a 90 mm wing segment clamped at span station 6 mm with a total 1 N vertical end load at station 96 mm. It uses nominal solid PLA and 1.25 mm cells. The grid is coarser than a skin, so results screen overall bending and do not qualify local skin buckling, the wing-body joint, or foamed material.

Use `sol flight.sol glider.stl PATH_TO_PLATE_GCODE` to recompute the flight report with the project's centered diagonal placement. It integrates model extrusion including arcs, then uses the existing Mk3 wing and tail model to estimate balance, static margin and tail trim. Airspeed assumes sea-level air density and a lift coefficient of 0.6. Uniform mass reductions leave the calculated CG fixed. The manufacturer's quoted maximum reduction is 50%; actual LW-PLA mass depends on foaming and flow. No drag polar or measured foam modulus is available for this revision.

[3D META LW-PLA specifications and FAQ](https://www.3dmeta.com.au/products/light-weight-pla-3d-printing-filament)
