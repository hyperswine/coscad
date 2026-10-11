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
                    difference() {
                      cuboid([110, 36, 14]);
                      translate([42, 0, 3.9]) {
                        cuboid([27.5, 28.5, 8.2]);
                      }
                    }
                    translate([-42, 0, 3.9]) {
                      cuboid([27.5, 28.5, 8.2]);
                    }
                  }
                  translate([42, 8, 0]) {
                    zcyl(r = 2.15, l = 20);
                  }
                }
                translate([42, -8, 0]) {
                  zcyl(r = 2.15, l = 20);
                }
              }
              translate([-42, 8, 0]) {
                zcyl(r = 2.15, l = 20);
              }
            }
            translate([-42, -8, 0]) {
              zcyl(r = 2.15, l = 20);
            }
          }
          translate([42, 8, -7.1]) {
            cylinder(h = 3.4, r = 4.25, $fn = 6);
          }
        }
        translate([42, -8, -7.1]) {
          cylinder(h = 3.4, r = 4.25, $fn = 6);
        }
      }
      translate([-42, 8, -7.1]) {
        cylinder(h = 3.4, r = 4.25, $fn = 6);
      }
    }
    translate([-42, -8, -7.1]) {
      cylinder(h = 3.4, r = 4.25, $fn = 6);
    }
  }
  hull() {
    translate([-13, 0, 6.9]) {
      zcyl(r = 7, l = 2.2);
    }
    translate([13, 0, 6.9]) {
      zcyl(r = 7, l = 2.2);
    }
  }
}
$fn = 50;