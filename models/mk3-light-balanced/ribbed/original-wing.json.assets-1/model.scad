include <BOSL2/std.scad>

intersection() {
  difference() {
    hull() {
      hull() {
        translate([3, 0, 0]) {
          translate([0, -40, 0]) {
            translate([0, 0, -4]) {
              zcyl(r = 1.5, l = 3);
            }
          }
        }
        translate([3, 0, 0]) {
          translate([0, 45, 0]) {
            translate([0, 0, -4]) {
              zcyl(r = 1.5, l = 3);
            }
          }
        }
      }
      translate([113, 0, 0]) {
        translate([0, 45, 0]) {
          translate([0, 0, 7]) {
            zcyl(r = 1.5, l = 3);
          }
        }
      }
    }
    union() {
      union() {
        hull() {
          hull() {
            translate([12, -27, -3.1]) {
              zcyl(r = 0.45, l = 1.25);
            }
            translate([12, 44, -3.1]) {
              zcyl(r = 0.45, l = 1.25);
            }
          }
          translate([46, 44, 0.3]) {
            zcyl(r = 0.45, l = 1.25);
          }
        }
        hull() {
          hull() {
            translate([49, 4, 0.6]) {
              zcyl(r = 0.45, l = 1.25);
            }
            translate([49, 44, 0.6]) {
              zcyl(r = 0.45, l = 1.25);
            }
          }
          translate([76, 44, 3.3]) {
            zcyl(r = 0.45, l = 1.25);
          }
        }
      }
      hull() {
        hull() {
          translate([79, 26, 3.6]) {
            zcyl(r = 0.45, l = 1.25);
          }
          translate([79, 44, 3.6]) {
            zcyl(r = 0.45, l = 1.25);
          }
        }
        translate([102, 44, 5.9]) {
          zcyl(r = 0.45, l = 1.25);
        }
      }
    }
  }
  translate([51, 0, 0]) {
    cuboid([90, 120, 40]);
  }
}
$fn = 50;