# Hand-driven 200 mm carriage

Print base on X1C, rack/carriage on P1S, and controls (integral thumbwheel/pinion, guide keeper, axle cap) on A1. Use the machine-specific thumbwheel.gcode.3mf under each slice directory.

Set the rack slot over the two guide posts with the teeth towards the axle. With the carriage centred, lower the pinion teeth-first onto the axle and turn slightly until the teeth mesh. Seat the rectangular keeper and round cap with their blind sockets facing down. The seated keeper clears the rack by 0.4 mm and the cap clears the thumbwheel by 0.3 mm nominally. The sockets have 0.02 mm nominal diametral clearance; check their retention and free running on the actual print.

A 224 mm closed slot runs against two 4 mm posts spaced 20 mm apart, giving 224 − 20 − 4 = 200 mm nominal travel. At either stop the pinion is still 17.81 mm inboard of the outermost tooth centre. The module-1.5, 20-tooth gear advances the rack by 94.25 mm per turn. Allow a clear 440 mm longitudinal envelope. Mark the rack position against the stationary base at one stop, turn by hand to the other, and measure the difference. Printed tolerances affect the actual stroke.
