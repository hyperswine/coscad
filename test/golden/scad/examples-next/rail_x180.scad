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
                  cuboid([180, 20, 20]);
                  translate([0, -10, 0]) {
                    cuboid([182, 3.6, 6.4]);
                  }
                }
                translate([0, 10, 0]) {
                  cuboid([182, 3.6, 6.4]);
                }
              }
              translate([0, 0, 10]) {
                cuboid([182, 6.4, 3.6]);
              }
            }
            translate([0, 0, -10]) {
              cuboid([182, 6.4, 3.6]);
            }
          }
          translate([0, 6.8, 0]) {
            cuboid([182, 2.8, 9.5]);
          }
        }
        translate([0, -6.8, 0]) {
          cuboid([182, 2.8, 9.5]);
        }
      }
      translate([0, 0, 6.8]) {
        cuboid([182, 9.5, 2.8]);
      }
    }
    translate([0, 0, -6.8]) {
      cuboid([182, 9.5, 2.8]);
    }
  }
  xcyl(r = 2.1, l = 182);
}
$fn = 50;