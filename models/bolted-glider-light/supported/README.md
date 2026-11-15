# Supported upper skin for the plain tube prototype

The owner reported that all three original prototype beds printed. The upper wing skin formed only partially. The owner considered the wings workable. Inadequate support is the working hypothesis.

This revision keeps the shell wing outline, screw stations, main spars and root mating faces. Narrow chordwise cells are divided by 0.50 mm webs, staggered by ±0.65 mm between rib stations. Nominal upper skin increases from 0.26 to 0.42 mm. Cavity endpoints are laid out at a nominal pitch of at most 7 mm before staggering. The bottom skin remains nominally 0.20 mm. Use build.py to regenerate the CoScad geometry.

Standard PLA, A1 right wing and P1S left wing. Bridge directions in the saved machine profiles are 2 and 89 degrees respectively. The finished slice packages and per-printer dispatch records are in the two job directories. Run check-bridges.py to compare labelled bridge moves in the original and revised toolpaths. A single move can cross multiple support webs. Physical top-skin continuity and spar bonding still require inspection.

Before adding trim, weigh the assembled replacement wings and recheck the CG with the original tube and nose hardware.
