# Twin SG90 lead-screw lift

The two moving threads carry and guide the platform on axes 40 mm apart. The platform has two threaded holes. Two rear frame posts sit 3 mm behind its swept envelope and hold the press-fit top cap. The cap journals both screw tips and provides the upper stop. Short pads on the base provide the lower stop.

Assembled height is 138.2 mm. Each screw is 100.7 mm overall with 91 mm of active thread and a 20 mm lead. Both screws have the same hand. Both servos face the same way, with their cable ends toward the rear. The screw hub uses the earlier 4.95 mm shaft bore. The 12 mm thick platform uses 0.35 mm nominal radial thread clearance.

Center both servos independently, then disconnect power. Thread both screws into the platform by hand until their hubs are level. Lower the assembly onto the two centered shafts, keeping the platform level near the middle of the frame. Seat both hubs with a small gap above their servo turrets. Align both journals and press the cap fully onto the rear keys. The journal shoulders should clear the cap underside.

Start unloaded with matched commands of ±15° (about 1.7 mm end to end). Check for binding or tilt and adjust the individual servo centers if necessary before increasing the sweep. Each 90° of rotation gives 5 mm of travel. The 74.2 mm stop spacing available to the platform requires 3.71 turns to traverse. A positional SG90 operates over a small portion of that range, selected by the platform's starting height. Equal commands alone do not ensure identical shaft angles under load.

Run `sol build.sol` here to regenerate the CoScad plate, then `coscad stl plate.coscad` to render the five pieces.
