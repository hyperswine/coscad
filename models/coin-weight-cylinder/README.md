# Coin weight cylinder

Pure CoScad model: 50 mm tall, nominal diameter 30 mm.
Top opening: 25.6 x 3.6 mm, 26 mm deep, for a coin up to 25 mm diameter and 3 mm thick.
The pocket is open and has no retaining lid; insert the coin after printing.
Print upright with the flat bottom on the build plate; supports are unnecessary.

`coin-weight-cylinder.coscad` is the editable source.
`coin-weight-cylinder.stl` is the validated closed mesh.
`coin-weight-cylinder_bed1.gcode.3mf` is sliced for Bambu P1S, 0.4 mm nozzle, PLA Basic, textured PEI, 0.20 mm layers.
Slicer estimate: 30m08s including preparation, 13.75 g PLA.

Rebuild: `stack exec coscad -- stl models/coin-weight-cylinder/coin-weight-cylinder.coscad`
