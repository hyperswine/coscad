# Spiral spring launcher (2026-11-16)

A follow-up to the captive spring demonstrator (`../captive-spring-demo`): a
printed PLA spiral (clock) spring drives a pinion, and the pinion drives a rack
carriage that throws the 10 g glider from `../pla-bow-launcher`. It winds about
**1.04 turns** over a 180 mm pull, where the demonstrator wound 0.04 turns
over 10 mm.

**Result (model only, not printed or tested):**
- **Speed:** 8.6 m/s for the 10 g glider at 24.8 MPa peak strip stress. That
  is the same class as the PLA bow launcher. It already includes a 0.85
  efficiency deduction.
- **Energy:** 1.51 J stored.
- **Draw:** 16.8 N at full draw.

## Why it doesn't coil more

The spring spins as it releases. Its inertia, seen at the carriage through a
pinion of pitch radius R, acts like a mass of J/R² riding on the carriage.
Winding more turns over the same stroke means a smaller R, and that effective
mass grows quickly. Best designs from `study.sol` (strip ≤ 75 g, 180 mm
stroke):

| Turns wound | Best speed | Spring + pinion felt at the rack |
|---|---|---|
| 0.5–1.0 | 8.5 m/s | 13 g |
| 1.0–1.5 | 8.6 m/s | 14 g (this design) |
| 1.5–2.0 | 6.5 m/s | 32 g |
| 2.0–3.0 | 3.6 m/s | 111 g |

So the design keeps the spiral compact and puts a large, light pinion on it:
- **Spiral:** 7 coils inside 70 mm, from a thick (2.0 mm), tall (28 mm) strip.
- **Pinion:** a spoked module-1.25, 44-tooth ring.

## Files

- `study.sol`: sizing model and the trade table above (about 1 minute).
- `build.sol`: writes every part and the three assemblies:
  - `launcher.assemble`: full draw, stopper in, glider loaded.
  - `launcher-end.assemble`: end of the stroke, with the last rack tooth at
    the pinion.
  - `launcher-coast.assemble`: the carriage at the end wall, glider 100 mm on.
- The glider parts come from `../pla-bow-launcher`.

Run both scripts from the repository root or this folder, e.g.
`"$SOL" models/spiral-launcher/build.sol`.

## Design

| Part | What it is |
|---|---|
| `spring` | Counter-clockwise spiral, 2.0 × 28 mm strip, 924 mm long, centreline r 8–34 mm. Hub with a D boss for the pinion. Outer clamp block bolted to the base. 74 g solid (strip 64 g). |
| `pinion` | Spoked ring gear, module 1.25, 44 teeth, R 27.5, 4 mm face, keyed to the hub, 4.6 g |
| `carriage` | Rack bar 220 mm long with lightening windows. Glider mast, groove cap and M3×16 screw pin as on the bow carriage, raised to z 45 so the glider clears the pinion, nut and rail. Stopper notch. 10.2 g + screw. |
| `rail-rear`, `rail-front` | Open channel above the spring, with an outer wall and a hold-down lip. The rear piece has the overdraw stop and the stopper guide; the front piece has the end wall (put foam tape on it). |
| `stopper` | Plate that drops through the guide into the carriage notch and a floor pocket, so it is held on both sides. |
| `base` | Plate with the M5 axle hole and hex pocket, two M4 clamp bolt holes, the hub washer and three rail legs |
| `foot` ×2 | Feet at the far ends of the rail |

How it runs:
- **Winding:** pulling the carriage back (−x) turns the pinion
  counter-clockwise, which tightens the spiral.
- **Release:** at the end of the stroke the rack's last tooth leaves the
  pinion. The spring and pinion free-spin instead of being stopped through the
  teeth. The carriage coasts about 5 mm into the end wall, and the glider
  slides forward off the pin.

**To launch:**
1. Pull the carriage back to the rear stop.
2. Slide the stopper down its slot.
3. Load the glider.
4. Lift the stopper.

**Hardware:**
- M5 axle bolt (from below) with a washer and nut on top
- 2 × M4 bolts for the spring clamp
- 7 × M3 screws: rail into legs and feet
- M3×16 glider pin

## Checks done

- **Renders:** every part renders with the native backend.
- **Interference:** `coscad check` finds 0 overlaps at the end of the stroke,
  with the glider loaded.
- **Mesh:** with the carriage at -137, -91, -46, -12 and -2 mm, and the pinion
  turned to match, the rack-pinion clearance is 0.14 mm at every position.
- **The one overlap at full draw and coast** is a modelling artefact. There
  the pinion is turned (375° and -6°) but the spring is drawn in its rest
  pose, so its D boss no longer lines up with the pinion's D hole. In reality
  the hub turns with the pinion; a wound spiral can't be drawn as a rigid part.
- **Stopper:** it sits in the carriage notch with 0.2 mm each side at full
  draw.
- **Overdraw stop:** 1 mm behind the carriage.

## Not checked

- **The spring model:** it uses pure moment and assumes the wound coils
  tighten evenly (1.2 mm wound gap). The clamp and hub ends carry stress
  concentrations, coils may rub, and stiffness depends on the real PLA (as on
  the bow launcher).
- **Inertia:** the effective mass assumes each strip element turns by
  θ(L−s)/L about the arbor.
- **Re-engaging the rack:** after a launch the pinion stops at an arbitrary
  angle, and pulling the carriage back has to find the mesh. The teeth have no
  lead-in chamfer yet.
- **Rail lip:** the lip is a 3.3 mm overhang when printed. It has 0.6 mm
  clearance over the carriage to allow for droop.
- **Rail stiffness:** the rear rail cantilevers past its leg to a foot, with
  the 16.8 N stopper load at its end.
