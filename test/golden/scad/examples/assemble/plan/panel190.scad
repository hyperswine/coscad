include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          difference() {
            cuboid([190, 190, 3]);
            translate([90, -40, 0]) {
              zcyl(r = 2.75, l = 10);
            }
          }
          translate([90, 40, 0]) {
            zcyl(r = 2.75, l = 10);
          }
        }
        translate([-90, -40, 0]) {
          zcyl(r = 2.75, l = 10);
        }
      }
      translate([-90, 40, 0]) {
        zcyl(r = 2.75, l = 10);
      }
    }
    translate([0, -90, 0]) {
      zcyl(r = 2.75, l = 10);
    }
  }
  translate([0, 90, 0]) {
    zcyl(r = 2.75, l = 10);
  }
}
$fn = 50;