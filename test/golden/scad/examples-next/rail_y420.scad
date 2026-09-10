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
                  cuboid([20.0, 420.0, 20.0]);
                  translate([10.0, 0.0, 0.0]) {
                    cuboid([3.6, 422.0, 6.4]);
                  }
                }
                translate([-10.0, 0.0, 0.0]) {
                  cuboid([3.6, 422.0, 6.4]);
                }
              }
              translate([0.0, 0.0, 10.0]) {
                cuboid([6.4, 422.0, 3.6]);
              }
            }
            translate([0.0, 0.0, -10.0]) {
              cuboid([6.4, 422.0, 3.6]);
            }
          }
          translate([6.8, 0.0, 0.0]) {
            cuboid([2.8, 422.0, 9.5]);
          }
        }
        translate([-6.8, 0.0, 0.0]) {
          cuboid([2.8, 422.0, 9.5]);
        }
      }
      translate([0.0, 0.0, 6.8]) {
        cuboid([9.5, 422.0, 2.8]);
      }
    }
    translate([0.0, 0.0, -6.8]) {
      cuboid([9.5, 422.0, 2.8]);
    }
  }
  translate([0.0, 0.0, 0.0]) {
    ycyl(r = 2.1, l = 422.0);
  }
}
$fn = 50;