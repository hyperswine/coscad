# `coscad flight`: a lightweight flight model

Status: implemented 2026-11-22. A vortex-lattice model of all the lifting
surfaces together, giving static longitudinal stability, trim and a glide
estimate. It is in the Haskell core and adds no dependencies.

## Why

The glider work estimated the neutral point by hand. It weighted the wing and
tail centres by Helmbold lift slopes and an assumed downwash gradient
(`bolted-glider-balanced/README.md`), and assumed a trim lift coefficient of
0.6. A vortex lattice derives the lift slope, the downwash at the tail, the
neutral point and trim from the planform itself, and it shows how the lift is
spread along the span. That matters for a swept wing, whose outer panels
stall first.

## Spec

coscad.flight/1 JSON uses the frame x aft (downstream), y right, z up, in mm.
`surfaces` is a list. The first surface is the reference for area, span and MAC. Each surface has:

- `sections`: `{"le": [x, y, z], "chord": c, "incidence_deg": i}`, ordered
  along the span. Positive incidence raises the leading edge. Dihedral, sweep
  and taper come from the leading-edge points.
- `mirror`: reflects the surface across y = 0, for a wing or stabiliser pair.
- `zero_lift_deg`: stands in for camber; the panels are solved at incidence minus this angle.
- `cm0`: the section pitching moment.
- `cd0`: the profile drag.
- `cl_max`: used for the stall check.

The spec also gives mass and CG, either `mass_g` with `cg_mm`, or
`mass_components` (a coscad.mass-components/1 file in the same frame). Other
keys: `parasite_drag_area_mm2` for the fuselage and hardware (CD times area),
`rho_kg_m3` (default 1.225), `chordwise_panels` (default 6) and `panel_mm`, the
target spanwise width (default span / 64). Unknown keys are rejected, and at
most 4000 panels are allowed.

## Method

Each panel carries a horseshoe vortex, after Katz and Plotkin, chapter 12:
- the bound vortex lies on the panel's quarter chord;
- the collocation point lies at three quarters;
- the trailing legs run straight aft along +x.

Within each segment between sections, the strips use cosine spacing. A dense LU
factorisation with partial pivoting solves the system for two freestreams, one
along x and one along z. Every coefficient is therefore linear in angle of
attack:

- **Lift and moment:** Kutta-Joukowski forces on the bound vortices, with
  pitching moment about the CG (positive nose up). Each surface's `cm0` is added
  over its horizontal strips.
- **Neutral point:** `x_np = x_cg - MAC * Cm_alpha / CL_alpha`. Static margin
  is `(x_np - x_cg) / MAC`.
- **Trim:** `Cm = 0` gives alpha and CL. Speed is
  `sqrt(2 m g / (rho S CL))`.
- **Drag:** induced drag from the Trefftz plane, plus each surface's cd0
  weighted by its area, plus the parasite area. The report gives the glide ratio,
  glide angle and sink rate.
- **Span efficiency:** `CL_alpha^2 / (pi AR CDi_alpha)` for the loading that
  angle of attack adds. Measured against the wing's span, a wing-and-tail layout can read
  slightly above 1.
- **Worst section:** the local section `cl = 2 G / c` at trim against each
  surface's `cl_max`. Above 1 means that part of the span stalls before the
  aircraft trims.

The verdict is `no lift`, `unstable` (Cm_alpha >= 0), `no positive trim`,
`stalls at trim` or `ok`. Every verdict except `ok` exits 2.

## Checks against theory

| Case | Panels | CL_alpha /rad | Neutral point | Span efficiency |
| --- | --- | --- | --- | --- |
| Rectangle AR 6 | 384 / 720 / 2000 | 4.26 / 4.24 / 4.23 | 0.240 / 0.239 / 0.239 c | 1.002 / 0.994 / 0.990 |
| Ellipse AR 6 | 684 / 996 / 2340 | 4.41 / 4.40 / 4.40 | 0.241 / 0.241 / 0.240 MAC | 1.008 / 1.005 / 1.003 |

For reference, vortex-lattice results for an AR 6 rectangle are about
4.2-4.3 per radian. Jones' lifting-surface estimate for the AR 6 ellipse is
4.53, about 3% above the elliptic result. Span efficiency converges slowly
downward with spanwise refinement, so treat it as good to about 2%.

A rectangular wing with a tail of volume 0.55 puts the neutral point at
49 mm on a 100 mm chord. The hand estimate is about 48 mm:
`25 + 0.55 * 100 * (a_t / a_w) * (1 - de/da)`.

## The balanced glider

`models/bolted-glider-balanced/flight.json` takes its planform from
`design.json` (the seven wing stations, sweep, 6 degree dihedral, 2.5 degree
incidence, and the stabiliser at -5.5 degrees). It uses the same airfoil
assumptions as the earlier estimate: zero-lift angle -3 degrees, cm0 -0.06.
Profile drag is 0.025 on the wing and 0.02 on the tail, with 350 mm^2 of
parasite area. Mass is 218.4 g with the CG at 90.5 mm.

| | Vortex lattice | Earlier hand estimate |
| --- | --- | --- |
| Neutral point | 101.1 mm | 105.1 mm |
| Static margin | 10.7% | 14.7% |
| Wing lift slope | 4.03 /rad (on the wing area) | 3.56 /rad (Helmbold) |
| Trim | CL 0.90 at 9.5 degrees, 9.4 m/s | assumed CL 0.6, 11.5 m/s |
| Glide | about 9.6 : 1, sink 0.98 m/s | not estimated |
| Worst section | cl 1.04 at y = 156 mm (swept outer panel) | not estimated |

Going from 352 to 3264 panels moves the neutral point by 0.08 mm and trim CL
by under 0.01. The model says the glider is stable, with a smaller margin than
estimated, and that at -5.5 degrees of tail incidence it trims slow. Its outer
panels then reach their assumed cl_max first. Less tail-down incidence, or
washout at the tips, would raise the trim speed and unload the tips. No flight
test is recorded yet (`physical-outcome-20261111.json`: not flight tested), so
these are predictions.

## Limits

The model is inviscid and linear. It does not model thickness, fuselage lift,
viscous lift loss, wake roll-up or deflection, Reynolds-number effects, stall
progression, sideslip or dynamic stability. Camber enters only through
`zero_lift_deg` and `cm0`, and the airfoil coefficients are assumptions
unless measured.
