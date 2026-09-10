include <BOSL2/std.scad>

union() {
  union() {
    difference() {
      union() {
        union() {
          translate([17, 0, 0]) {
            translate([0, 0, 2.5]) {
              cuboid([34, 16, 5], chamfer = 0.5);
            }
          }
          translate([15.3, 0, 0]) {
            translate([0, 0, -0.6]) {
              prismoid(size1 = [2.2, 5.4], size2 = [3, 5.8], h = 1.8, anchor = CENTER);
            }
          }
        }
        translate([28.7, 0, 0]) {
          translate([0, 0, -0.6]) {
            prismoid(size1 = [2.2, 5.4], size2 = [3, 5.8], h = 1.8, anchor = CENTER);
          }
        }
      }
      translate([22, 0, 0]) {
        translate([0, 0, 2.5]) {
          zcyl(r = 2.75, l = 12);
        }
      }
    }
    rotate([0, 0, 90]) {
      difference() {
        union() {
          union() {
            translate([17, 0, 0]) {
              translate([0, 0, 2.5]) {
                cuboid([34, 16, 5], chamfer = 0.5);
              }
            }
            translate([15.3, 0, 0]) {
              translate([0, 0, -0.6]) {
                prismoid(size1 = [2.2, 5.4], size2 = [3, 5.8], h = 1.8, anchor = CENTER);
              }
            }
          }
          translate([28.7, 0, 0]) {
            translate([0, 0, -0.6]) {
              prismoid(size1 = [2.2, 5.4], size2 = [3, 5.8], h = 1.8, anchor = CENTER);
            }
          }
        }
        translate([22, 0, 0]) {
          translate([0, 0, 2.5]) {
            zcyl(r = 2.75, l = 12);
          }
        }
      }
    }
  }
  translate([0, 0, 2.5]) {
    cyl(r = 9, h = 5, chamfer = 0.5);
  }
}
$fn = 50;