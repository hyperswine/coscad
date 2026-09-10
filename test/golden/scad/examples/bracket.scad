include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      union() {
        union() {
          union() {
            translate([2.5, 0, 0]) {
              translate([0, 0, 10.0]) {
                cuboid([5.0, 15.0, 20.0]);
              }
            }
            translate([8.0, 0, 0]) {
              translate([0, 0, 17.5]) {
                cuboid([16.0, 15.0, 5.0]);
              }
            }
          }
          translate([8.5, 0, 0]) {
            translate([0, 0, 8.1]) {
              mirror([0.0, 0.0, 1.0]) {
                rotate([0, 0, -90.0]) {
                  wedge([15.0, 15.0, 14.2], anchor = CENTER);
                }
              }
            }
          }
        }
        translate([-0.5, 0, 0]) {
          translate([0, 0, 10.0]) {
            cuboid([1.0, 15.0, 5.8]);
          }
        }
      }
      translate([2.5, 0, 0]) {
        translate([0, 0, 10.0]) {
          xcyl(r = 2.7, l = 12.0);
        }
      }
    }
    translate([15.5, 0, 0]) {
      translate([0, 0, 10.0]) {
        xcyl(r = 5.0, l = 21.0);
      }
    }
  }
  translate([10.0, 0, 0]) {
    translate([0, 0, 16.0]) {
      zcyl(r = 1.4, l = 10.0);
    }
  }
}
$fn = 50;