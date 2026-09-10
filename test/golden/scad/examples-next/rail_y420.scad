include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          difference() {
            difference() {
              difference() {
                difference() {
                  cuboid([20, 420, 20]);
                  translate([10, 0, 0]) {
                    cuboid([3.6, 422, 6.4]);
                  }
                }
                translate([-10, 0, 0]) {
                  cuboid([3.6, 422, 6.4]);
                }
              }
              translate([0, 0, 10]) {
                cuboid([6.4, 422, 3.6]);
              }
            }
            translate([0, 0, -10]) {
              cuboid([6.4, 422, 3.6]);
            }
          }
          translate([6.8, 0, 0]) {
            cuboid([2.8, 422, 9.5]);
          }
        }
        translate([-6.8, 0, 0]) {
          cuboid([2.8, 422, 9.5]);
        }
      }
      translate([0, 0, 6.8]) {
        cuboid([9.5, 422, 2.8]);
      }
    }
    translate([0, 0, -6.8]) {
      cuboid([9.5, 422, 2.8]);
    }
  }
  ycyl(r = 2.1, l = 422);
}
$fn = 50;