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
                  cuboid([180.0, 20.0, 20.0]);
                  translate([0.0, -10.0, 0.0]) {
                    cuboid([182.0, 3.6, 6.4]);
                  }
                }
                translate([0.0, 10.0, 0.0]) {
                  cuboid([182.0, 3.6, 6.4]);
                }
              }
              translate([0.0, 0.0, 10.0]) {
                cuboid([182.0, 6.4, 3.6]);
              }
            }
            translate([0.0, 0.0, -10.0]) {
              cuboid([182.0, 6.4, 3.6]);
            }
          }
          translate([0.0, 6.8, 0.0]) {
            cuboid([182.0, 2.8, 9.5]);
          }
        }
        translate([0.0, -6.8, 0.0]) {
          cuboid([182.0, 2.8, 9.5]);
        }
      }
      translate([0.0, 0.0, 6.8]) {
        cuboid([182.0, 9.5, 2.8]);
      }
    }
    translate([0.0, 0.0, -6.8]) {
      cuboid([182.0, 9.5, 2.8]);
    }
  }
  translate([0.0, 0.0, 0.0]) {
    xcyl(r = 2.1, l = 182.0);
  }
}
$fn = 50;