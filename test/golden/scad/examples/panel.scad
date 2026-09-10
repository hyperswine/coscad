include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          cuboid([200, 200, 2.5], rounding = 1);
          translate([90, 0, 0]) {
            translate([0, 90, 0]) {
              zcyl(r = 2.75, l = 8);
            }
          }
        }
        translate([-90, 0, 0]) {
          translate([0, 90, 0]) {
            zcyl(r = 2.75, l = 8);
          }
        }
      }
      translate([90, 0, 0]) {
        translate([0, -90, 0]) {
          zcyl(r = 2.75, l = 8);
        }
      }
    }
    translate([-90, 0, 0]) {
      translate([0, -90, 0]) {
        zcyl(r = 2.75, l = 8);
      }
    }
  }
  zcyl(r = 2.75, l = 8);
}
$fn = 50;