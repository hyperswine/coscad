# PLA bending calibration coupon

Print flat in PLA. The P1S plate uses 0.20 mm layers and enough solid layers to fill the 4 mm thickness. The rectangular beam is nominally 100 × 12 × 4 mm.

1. Measure the actual width and thickness of the beam. Mark 20 mm and 95 mm from the same end.
2. Clamp the first 20 mm between flat jaws. Keep the printed face horizontal and the 4 mm dimension vertical. The load mark should be 75 mm beyond the jaw edge.
3. Measure unloaded height at the load mark. Hang 102 g total at that mark, including the sling, for approximately 1.00 N. Measure loaded height at the same point after five seconds. Remove the load and check that the beam returns. Repeat three times.
4. With dimensions in mm and force in newtons, compute I = width × thickness³ / 12 and E = force × span³ / (3 × I × deflection). E is in MPa. For nominal dimensions, E ≈ 2197 / deflection_mm. A deflection of 0.88 mm implies about 2.5 GPa.

This measures effective flat-print bending stiffness for the P1S recipe and loaded spool. It does not measure failure strength or through-layer stiffness. Check for clamp movement if the three measurements disagree.
