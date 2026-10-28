include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          cuboid([50, 20, 20]);
          union() {
            union() {
              translate([0, 0, 9.95]) {
                cuboid([52, 7.4, 3.1]);
              }
              union() {
                translate([0, 0, 5.3]) {
                  prismoid(size1 = [52, 7.2], size2 = [52, 11.2], h = 2, anchor = CENTER);
                }
                translate([0, 0, 7.4]) {
                  cuboid([52, 11.2, 2.22]);
                }
              }
            }
            translate([0, 0, 6]) {
              cuboid([52, 5.6, 5]);
            }
          }
        }
        rotate([90, 0, 0]) {
          union() {
            union() {
              translate([0, 0, 9.95]) {
                cuboid([52, 7.4, 3.1]);
              }
              union() {
                translate([0, 0, 5.3]) {
                  prismoid(size1 = [52, 7.2], size2 = [52, 11.2], h = 2, anchor = CENTER);
                }
                translate([0, 0, 7.4]) {
                  cuboid([52, 11.2, 2.22]);
                }
              }
            }
            translate([0, 0, 6]) {
              cuboid([52, 5.6, 5]);
            }
          }
        }
      }
      rotate([180, 0, 0]) {
        union() {
          union() {
            translate([0, 0, 9.95]) {
              cuboid([52, 7.4, 3.1]);
            }
            union() {
              translate([0, 0, 5.3]) {
                prismoid(size1 = [52, 7.2], size2 = [52, 11.2], h = 2, anchor = CENTER);
              }
              translate([0, 0, 7.4]) {
                cuboid([52, 11.2, 2.22]);
              }
            }
          }
          translate([0, 0, 6]) {
            cuboid([52, 5.6, 5]);
          }
        }
      }
    }
    rotate([270, 0, 0]) {
      union() {
        union() {
          translate([0, 0, 9.95]) {
            cuboid([52, 7.4, 3.1]);
          }
          union() {
            translate([0, 0, 5.3]) {
              prismoid(size1 = [52, 7.2], size2 = [52, 11.2], h = 2, anchor = CENTER);
            }
            translate([0, 0, 7.4]) {
              cuboid([52, 11.2, 2.22]);
            }
          }
        }
        translate([0, 0, 6]) {
          cuboid([52, 5.6, 5]);
        }
      }
    }
  }
  xcyl(r = 2.1, l = 52);
}
$fn = 50;