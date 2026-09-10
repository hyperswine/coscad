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
                  difference() {
                    cuboid([110.0, 30.0, 14.0]);
                    translate([42.0, 0.0, 3.9]) {
                      cuboid([27.5, 28.5, 8.2]);
                    }
                  }
                  translate([-42.0, 0.0, 3.9]) {
                    cuboid([27.5, 28.5, 8.2]);
                  }
                }
                translate([42.0, 8.0, 0.0]) {
                  zcyl(r = 2.15, l = 20.0);
                }
              }
              translate([42.0, -8.0, 0.0]) {
                zcyl(r = 2.15, l = 20.0);
              }
            }
            translate([-42.0, 8.0, 0.0]) {
              zcyl(r = 2.15, l = 20.0);
            }
          }
          translate([-42.0, -8.0, 0.0]) {
            zcyl(r = 2.15, l = 20.0);
          }
        }
        translate([42.0, 8.0, -7.1000000000000005]) {
          cylinder(h = 3.4, r = 4.25, $fn = 6);
        }
      }
      translate([42.0, -8.0, -7.1000000000000005]) {
        cylinder(h = 3.4, r = 4.25, $fn = 6);
      }
    }
    translate([-42.0, 8.0, -7.1000000000000005]) {
      cylinder(h = 3.4, r = 4.25, $fn = 6);
    }
  }
  translate([-42.0, -8.0, -7.1000000000000005]) {
    cylinder(h = 3.4, r = 4.25, $fn = 6);
  }
}
$fn = 50;