// rack_pinion.scad
// PLA rack-and-pinion linear drive, NEMA17 stepper driven.
// Parts: pinion (D-bore + setscrew hub), rack (with mounting base),
//        motor bracket (NEMA17 face mount).
//
//   openscad rack_pinion.scad                          # plated preview
//   openscad -D 'view="assembly"' rack_pinion.scad     # assembled preview
//   openscad-stl rack_pinion.scad pinion               # export one part
//   openscad-stl rack_pinion.scad --all                # export everything

include <BOSL2/std.scad>
include <BOSL2/gears.scad>
include <make_parts.scad>

/* ---------------- Parameters ---------------- */

$fn = 64;

// Gearing — module 2, 20 deg PA: chunky teeth that survive in PLA.
GEAR_MOD = 2;
PINION_TEETH = 18;
RACK_TEETH = 25; // rack length = RACK_TEETH * PI * GEAR_MOD ~= 157mm
PRESSURE_ANGLE = 20;
BACKLASH = 0.15; // print-tolerance allowance
GEAR_THICK = 8; // face width of pinion and rack

PITCH_R = pitch_radius(mod=GEAR_MOD, teeth=PINION_TEETH); // = 18mm

// NEMA17 shaft: 5mm dia, flat milled to 4.5mm
SHAFT_D = 5.0;
BORE_CLR = 0.2; // diametral clearance for the bore
FLAT_POS = 2.0 + BORE_CLR / 2; // flat plane distance from shaft center

HUB_D = 16;
HUB_H = 8;

// Rack base (flange under the teeth, with mounting holes)
RACK_BASE_H = 6; // solid height below tooth roots
RACK_HOLE_D = 3.4; // M3 clearance
RACK_HOLE_N = 3;

// NEMA17 face: 42mm body, 31mm hole square, 22mm pilot boss
BRACKET_T = 5;
NEMA_HOLE_SP = 31;
NEMA_HOLE_D = 3.4;
NEMA_PILOT_D = 22.5;
BRACKET_W = 46;

/* ---------------- Parts ---------------- */

// D-profile bore for the NEMA17 shaft, extruded along Z, centered.
module d_bore(h) {
  linear_extrude(height=h, center=true, convexity=4)
    difference() {
      circle(d=SHAFT_D + BORE_CLR);
      right(FLAT_POS) square([SHAFT_D, SHAFT_D + 1], anchor=LEFT);
    }
}

module pinion() {
  difference() {
    union() {
      spur_gear(
        mod=GEAR_MOD,
        teeth=PINION_TEETH,
        thickness=GEAR_THICK,
        pressure_angle=PRESSURE_ANGLE,
        backlash=BACKLASH
      );
      // Setscrew hub on top
      up(GEAR_THICK / 2) cyl(d=HUB_D, h=HUB_H, anchor=BOTTOM);
    }
    d_bore(GEAR_THICK + 2 * HUB_H + 2);
    // M3 setscrew hole through the hub (tap or heat-set from outside)
    up(GEAR_THICK / 2 + HUB_H / 2) ycyl(d=2.8, h=HUB_D + 2);
  }
}

module rack_part() {
  rack_len = RACK_TEETH * circular_pitch(mod=GEAR_MOD);
  union() {
    rack(
      mod=GEAR_MOD,
      teeth=RACK_TEETH,
      thickness=GEAR_THICK,
      bottom=GEAR_THICK, // pitch line (X axis) down to rack bottom
      pressure_angle=PRESSURE_ANGLE,
      backlash=BACKLASH
    );
    // Mounting base flange below the rack body, with M3 holes
    down(GEAR_THICK)
      difference() {
        cuboid([rack_len, GEAR_THICK + 8, RACK_BASE_H], anchor=TOP);
        xcopies(n=RACK_HOLE_N, l=rack_len - 20)
          cyl(d=RACK_HOLE_D, h=RACK_BASE_H + 2);
      }
  }
}

module motor_bracket() {
  plate_h = 60;
  motor_z = 40; // shaft axis height above bracket base
  difference() {
    union() {
      // Vertical motor plate
      cuboid([BRACKET_W, BRACKET_T, plate_h], anchor=BOTTOM + FWD);
      // Base flange
      cuboid([BRACKET_W, 30, BRACKET_T], anchor=BOTTOM + BACK);
      // Side gussets
      xcopies(spacing=BRACKET_W - 4)
        prismoid(
          size1=[4, 25], size2=[4, 0], h=25,
          shift=[0, 12.5], anchor=BOTTOM + BACK
        );
    }
    // NEMA17 pilot + bolt pattern, through the vertical plate
    up(motor_z) ycyl(d=NEMA_PILOT_D, h=3 * BRACKET_T);
    up(motor_z)
      grid_copies(spacing=NEMA_HOLE_SP, n=[2, 1])
        zcopies(spacing=NEMA_HOLE_SP, n=2)
          ycyl(d=NEMA_HOLE_D, h=3 * BRACKET_T);
    // Base slots for M4 mounting bolts
    fwd(15) xcopies(spacing=BRACKET_W - 16)
        cyl(d=4.4, h=3 * BRACKET_T);
  }
}

/* ---------------- Output ---------------- */

// Assembly: rack lies teeth-up along X with its pitch line at Z=0 (its
// native orientation), pinion axis along Y at Z = pitch radius, spun half
// a tooth about its own axis so the teeth interleave.
make_parts(
  names=["pinion", "rack", "bracket"],
  spacing=110,
  asm=[
    [[0, 0, PITCH_R], [90, 0, 180 / PINION_TEETH]], // pinion
    [[0, 0, 0], [0, 0, 0]], // rack
    [[0, GEAR_THICK / 2 + HUB_H + 2, PITCH_R - 40], [0, 0, 0]], // bracket
  ]
) {
  pinion();
  rack_part();
  motor_bracket();
}

// openscad-st1 rack_pinion.scad --list &8 openscad-st1 rack_pinion.scad --all
