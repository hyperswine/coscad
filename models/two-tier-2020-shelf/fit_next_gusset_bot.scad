include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          union() {
            union() {
              translate([32.5, 0, 0]) {
                cuboid([45, 20, 6]);
              }
              translate([0, -32.5, 0]) {
                cuboid([20, 45, 6]);
              }
            }
            hull() {
              hull() {
                translate([10, -10, 0]) {
                  cuboid([1, 1, 6]);
                }
                translate([42, -10, 0]) {
                  cuboid([1, 1, 6]);
                }
              }
              translate([10, -42, 0]) {
                cuboid([1, 1, 6]);
              }
            }
          }
          cuboid([20.4, 20.4, 10]);
        }
        translate([25, 0, 0]) {
          zcyl(r = 2.8, l = 10);
        }
      }
      translate([45, 0, 0]) {
        zcyl(r = 2.8, l = 10);
      }
    }
    translate([0, -25, 0]) {
      zcyl(r = 2.8, l = 10);
    }
  }
  translate([0, -45, 0]) {
    zcyl(r = 2.8, l = 10);
  }
}
$fn = 50;