include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      union() {
        union() {
          union() {
            translate([2.5, 0, 0]) {
              translate([0, 0, 10]) {
                cuboid([5, 15, 20]);
              }
            }
            translate([8, 0, 0]) {
              translate([0, 0, 17.5]) {
                cuboid([16, 15, 5]);
              }
            }
          }
          translate([8.5, 0, 0]) {
            translate([0, 0, 8.1]) {
              mirror([0, 0, 1]) {
                rotate([0, 0, -90]) {
                  wedge([15, 15, 14.2], anchor = CENTER);
                }
              }
            }
          }
        }
        translate([-0.5, 0, 0]) {
          translate([0, 0, 10]) {
            cuboid([1, 15, 5.8]);
          }
        }
      }
      translate([2.5, 0, 0]) {
        translate([0, 0, 10]) {
          xcyl(r = 2.7, l = 12);
        }
      }
    }
    translate([15.5, 0, 0]) {
      translate([0, 0, 10]) {
        xcyl(r = 5, l = 21);
      }
    }
  }
  translate([10, 0, 0]) {
    translate([0, 0, 16]) {
      zcyl(r = 1.4, l = 10);
    }
  }
}
$fn = 50;