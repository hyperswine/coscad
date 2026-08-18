// ============================================================
// Seam bracket for 2020 T-slot rail + thin PETG panels
// ------------------------------------------------------------
// One bracket bolts to the SIDE slot of a 2020 rail (M5 + T-nut,
// horizontal). Its top flange sits flush with the rail top face
// and presents a surface the panel screws down onto (M3).
// The part is symmetric in Y, so the SAME part serves both the
// left and right side of the rail (rotate 180°).
//
// Panel convention: panel edge sits at the rail face; panel hole
// is `panel_hole_x` in from the panel edge (default 10 mm, same
// inset as the perimeter holes that hit rail-top slot centerlines,
// so all panels stay identical/interchangeable).
//
// Hardware:
//   - Rail side: M5 x 12 socket-cap screw + drop-in T-nut,
//     driven with a 4 mm hex key through the access tunnel.
//   - Panel side: M3 x 8 thread-forming into the 2.8 mm pilot
//     (or set m3_pilot = 4.0 and use an M3 heat-set insert).
//
// Print: lie the bracket on its flat side (Y face down).
// No supports needed; layers end up in the strong orientation.
// ============================================================

include <BOSL2/std.scad>

/* [Rail] */
rail = 20; // 2020 face width / height
slot_z = 10; // side-slot centerline height (2020 = 10)

/* [Bracket body] */
width = 15; // length along the rail (Y)
plate_t = 5; // back plate thickness (X)
flange_len = 16; // flange reach from rail face (must be > panel_hole_x + ~4)
flange_t = 5; // flange thickness; M3 threads into this
gusset_drop = 15; // vertical extent of the triangle below the flange

/* [Fasteners] */
m5_clear = 5.4; // M5 clearance hole
tunnel_d = 9; // access bore for M5 socket head + 4mm hex key
panel_hole_x = 10; // M3 position, measured from rail face
m3_pilot = 2.8; // 2.8 = thread-forming M3, 4.0 = heat-set insert

/* [Slot key (anti-rotation nub)] */
key_on = true;
key_h = 5.8; // 2020 slot mouth is ~6.0-6.2; leave clearance
key_proud = 1.0; // how far it pokes into the slot mouth

/* [Render] */
// "part" = single printable bracket, "assembly" = rail + 2 brackets + panels
mode = "assembly"; // ["part", "assembly"]

$fn = 48;

// ------------------------------------------------------------
module bracket() {
  difference() {
    union() {
      // back plate against the rail side face (x = 0 plane)
      translate([plate_t / 2, 0, rail / 2])
        cuboid([plate_t, width, rail]);

      // top flange, flush with rail top
      translate([flange_len / 2, 0, rail - flange_t / 2])
        cuboid([flange_len, width, flange_t]);

      // triangular gusset under the flange (nudged 0.2 up into the
      // flange so the union is a clean manifold, not coplanar)
      translate([0, width / 2, 0])
        rotate([90, 0, 0])
          linear_extrude(width)
            polygon(
              [
                [1, rail - flange_t + 0.2],
                [flange_len, rail - flange_t + 0.2],
                [1, rail - flange_t - gusset_drop + 1],
              ]
            );

      // anti-rotation key into the slot mouth (overlapped into plate)
      if (key_on)
        translate([-key_proud / 2 + 0.1, 0, slot_z])
          cuboid([key_proud + 0.2, width, key_h]);
    }

    // M5 clearance hole into the side-slot T-nut
    translate([plate_t / 2, 0, slot_z])
      xcyl(l=plate_t + 2 * key_proud + 4, d=m5_clear);

    // driver/head access tunnel through the gusset
    translate([plate_t + (flange_len + 10 - plate_t) / 2, 0, slot_z])
      xcyl(l=flange_len + 10 - plate_t, d=tunnel_d);

    // M3 pilot through the flange (extended well past the flange
    // underside so it overlaps the tunnel void cleanly rather than
    // touching it tangentially, which makes a non-manifold contact)
    translate([panel_hole_x, 0, rail - (flange_t + 4) / 2 + 0.5])
      zcyl(l=flange_t + 5, d=m3_pilot);
  }
}

// ------------------------------------------------------------
// Crude 2020 profile, just for the assembly preview
module rail2020(len = 60) {
  difference() {
    translate([0, 0, rail / 2]) cuboid([rail, len, rail]);
    // slot mouths on all four faces
    for (a = [0:3])
      rotate([0, a * 90, 0])
        translate([0, 0, rail / 2])
          translate([0, 0, rail / 2])
            cuboid([6.2, len + 2, 4], anchor=TOP);
    // center bore
    translate([0, 0, rail / 2]) ycyl(l=len + 2, d=4.2);
  }
}

module panel(sz = 40, t = 2.5) {
  color("skyblue", 0.75)
    translate([sz / 2, 0, rail + t / 2])
      cuboid([sz, 50, t]);
}

module assembly() {
  color("silver") translate([0, 0, 0]) rail2020();
  // right-side bracket
  color("orange") translate([rail / 2, 0, 0]) bracket();
  // left-side bracket = same part rotated 180 about Z
  color("orangered") rotate([0, 0, 180]) translate([rail / 2, 0, 0]) bracket();
  // the two panels, edges at the rail faces
  translate([rail / 2, 0, 0]) panel();
  rotate([0, 0, 180]) translate([rail / 2, 0, 0]) panel();
}

// ------------------------------------------------------------
if (mode == "part")
  bracket();
else
  assembly();
