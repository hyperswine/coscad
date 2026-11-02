# Printable connections

The first CoScad standard-library collection lives in `Coscad.Stdlib` and expands
into ordinary native CoScad solids. All constructors below work directly in
`.coscad` files in legacy, `!simple` and `!glyph` modes; no import/include, Python
generator, OpenSCAD or filesystem library search is required. They are compiled
into the executable, and participate in validation, STL/image rendering,
assembly/plan/site geometry and the compiler-backed LSP like other solids.
This is a constructor library, not a new general-purpose module/function syntax.

## Thread pairs

```text
thread_male   rootRadius majorRadius pitch length rootWidth crestWidth facets
thread_female rootRadius majorRadius pitch length rootWidth crestWidth facets radialClearance axialClearance
```

All lengths are millimetres, both radii are **radii, not diameters**, and profile
widths are full axial widths. Clearances independently enlarge the female
profile radially and on **each** axial flank. They do not scale pitch or length.
Facets is an integer 12..192 per revolution. Geometry expansion is bounded to
4096 facets; oversized/invalid requests fail before allocating the helix.
Cleared root width must remain less than pitch. Positive finite dimensions are
required (root radius >0.4, length >0.8, root width >crest width >0.04).

Both are right-handed, with common phase z=pitch*angle/360, primarily in
`z=-length..0`. The female is a **positive cutter**, not a nut: subtract it from
a suitably thick host, open on z=0. It extends 0.3 mm beyond either end and has
a 0.8 mm entry mouth. The male has a 0.4 mm core chamfer and a 0.6 mm shorter
thread-ridge envelope. These lead-in details deliberately preserve Tesseract III.
Rotate both or mirror both together when changing axis/handedness.

Tesseract III defaults:

```coscad
male = thread_male 4.05 5 3 8 2.2 0.2 24
cutter = thread_female 4.05 5 3 8 2.2 0.2 24 0.35 0.18
socket = (zcyl 8 10 |> anchor top) |> cut cutter
main = male
```

The Haskell API uses a named `ThreadSpec` record and returns `Either String Shape`:
`threadMale tesseractThread`, `threadFemale tesseractThread 0.35 0.18`.
Change record fields for other sizes rather than globally scaling mating shapes.
The defaults derive from the successful physical thread-fit coupon. The later
supported diagonal sockets still had printer-dependent defects; this is not a
claim that every orientation or printer will print these threads well.

## Globe's distinct coarse joint

```text
globe_stud   rootRadius majorRadius pitch length
globe_socket rootRadius majorRadius pitch length radialClearance axialClearance
```

The globe uses a different point-hull thread recipe, retained separately rather
than approximated by the tesseract's. Successful defaults are `10 12 6 12`,
with socket clearances `0.4 0.25`. Both grow upward from z=0. The stud's last
3 mm taper; the socket cutter includes an entry chamfer, 1 mm end allowance and
a conical roof above engagement. **Allow room for that roof in the host**:
default cutter reaches z=25.41 mm, although engagement is only 12 mm. A short
coupon may deliberately leave it open, as in the original successful fit test.
Root radius must exceed 0.9, length must exceed 3; the cleared profile must fit
within a pitch. Resolution is the original 48 facets per turn.

The owner's two-part globe had successful thread fit and captive globe/gimbal
clearance with bed-only tree supports and thicker walls. This joint recipe
extracts the stand connection; it does not generate continents or an entire
self-supporting globe. The original print records and geometry remain unchanged.

## Other connection mechanisms

| Constructor | Parameters | Coordinates and meaning |
|---|---|---|
| `clearance_pin` | shaftRadius shaftLength flangeRadius flangeHeight | Bottom flange at z=0; shaft extends to flangeHeight+shaftLength. Flange must exceed shaft radius. |
| `clearance_bore` | shaftRadius length radialClearance | Positive cutter at z=0..length; radius is shaftRadius+clearance. Subtract from the host. |
| `captive_bearing` | shaftRadius span stopRadius stopHeight radialGap axialGap sleeveWall | Stationary axle with two retaining stops, plus a separate captive sleeve between them. Bottom is z=0. |

The bearing requires positive gaps/wall thickness, span >2*axialGap, and stops
wider than the sleeve exterior. It contains **two disconnected printed solids**
by design, with a shared axis and no modeled interference. Its dimensions are
not a proven physical fit; check overhangs and keep supports out of the moving
gap. It is a reusable print-in-place mechanism, not a promise of support-free
printing. Snap fits, bayonets, hinges and dovetails can be added as independently
validated recipes later, rather than labelling untested mechanisms as proven.

## Run the examples

```sh
export PKG_CONFIG_PATH="$PWD/.native/manifold-3.5.2/install/lib/pkgconfig"
cabal build exe:coscad
COSCAD=$(cabal list-bin exe:coscad)
"$COSCAD" stl stdlib/examples/thread-fit.coscad -o /tmp/thread-fit.stl
"$COSCAD" stl stdlib/examples/globe-joint.coscad -o /tmp/globe-joint.stl
"$COSCAD" stl stdlib/examples/pin-fit.coscad -o /tmp/pin-fit.stl
"$COSCAD" stl stdlib/examples/captive-bearing.coscad -o /tmp/captive-bearing.stl
"$COSCAD" stl stdlib/examples/tesseract-iii.coscad -o /tmp/tesseract-hub.stl
```

The last example uses the existing tesseract design with its long thread
construction replaced by two constructor calls; its `main` is the corner hub,
as in the original source. Historical model exports and sliced/printed packages
are preserved. The examples do not dispatch printers.

Tests compare reference geometry and exercise a correctly phased screw sweep,
a deliberately misphased collision, parameter variations and invalid requests.
These checks do not replace a fit coupon, slicer/support review, or physical
strength/wear testing after changing a parameter, material or printer.

Focused connection regression tests: `cabal test coscad-test --test-options=--stdlib-only`.
