include <BOSL2/std.scad>

scale([1.5, 1.5, 1]) {
  difference() {
    difference() {
      difference() {
        difference() {
          union() {
            union() {
              union() {
                translate([0, 13, 0.7]) {
                  cuboid([21, 4.5, 1.4]);
                }
                translate([0, 0.5, 0.7]) {
                  cuboid([5, 25, 1.4]);
                }
              }
              translate([0, 0, 0.7]) {
                zcyl(r = 4.4, l = 1.4);
              }
            }
            translate([0, -12, 0.7]) {
              cuboid([6, 5, 1.4]);
            }
          }
          translate([0, 0, 0.5]) {
            zcyl(r = 3.1, l = 3);
          }
        }
        translate([0, -12, 0.5]) {
          zcyl(r = 1.65, l = 3);
        }
      }
      translate([-8, 13, 0.5]) {
        zcyl(r = 1.41, l = 3);
      }
    }
    translate([8, 13, 0.5]) {
      zcyl(r = 1.41, l = 3);
    }
  }
}
$fn = 50;