include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          difference() {
            translate([0, 6.0, 0]) {
              translate([0, 0, 7.0]) {
                cuboid([110.0, 30.0, 14.0], rounding = 2.0);
              }
            }
            translate([42.0, 0, 0]) {
              translate([0, 6.0, 0]) {
                translate([0, 0, 10.9]) {
                  cuboid([27.5, 28.5, 8.2]);
                }
              }
            }
          }
          translate([-42.0, 0, 0]) {
            translate([0, 6.0, 0]) {
              translate([0, 0, 10.9]) {
                cuboid([27.5, 28.5, 8.2]);
              }
            }
          }
        }
        translate([42.0, 0, 0]) {
          translate([0, -2.0, 0]) {
            translate([0, 0, 3.6]) {
              zcyl(r = 2.8, l = 6.6);
            }
          }
        }
      }
      translate([42.0, 0, 0]) {
        translate([0, 14.0, 0]) {
          translate([0, 0, 3.6]) {
            zcyl(r = 2.8, l = 6.6);
          }
        }
      }
    }
    translate([-42.0, 0, 0]) {
      translate([0, -2.0, 0]) {
        translate([0, 0, 3.6]) {
          zcyl(r = 2.8, l = 6.6);
        }
      }
    }
  }
  translate([-42.0, 0, 0]) {
    translate([0, 14.0, 0]) {
      translate([0, 0, 3.6]) {
        zcyl(r = 2.8, l = 6.6);
      }
    }
  }
}
$fn = 50;