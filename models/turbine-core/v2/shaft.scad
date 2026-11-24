include <BOSL2/std.scad>

union() {
  difference() {
    union() {
      union() {
        union() {
          translate([0, 0, 81]) {
            zcyl(r = 4, l = 162);
          }
          translate([0, 0, 44]) {
            zcyl(r = 5, l = 12);
          }
        }
        translate([0, 0, 82]) {
          zcyl(r = 5, l = 12);
        }
      }
      union() {
        hull() {
          translate([0, 0, 146.01]) {
            zcyl(r = 4, l = 2.0e-2);
          }
          translate([0, 0, 148.51]) {
            zcyl(r = 6.5, l = 2.0e-2);
          }
        }
        translate([0, 0, 155.25]) {
          zcyl(r = 6.5, l = 13.5);
        }
      }
    }
    union() {
      translate([0, 0, 157.5]) {
        zcyl(r = 2.475, l = 9.2);
      }
      hull() {
        translate([0, 0, 162.05]) {
          zcyl(r = 2.875, l = 2.0e-2);
        }
        translate([0, 0, 161.55]) {
          zcyl(r = 2.475, l = 2.0e-2);
        }
      }
    }
  }
  union() {
    union() {
      union() {
        rotate([0, 0, 0]) {
          translate([8, 0, 0]) {
            translate([0, 0, 0.1]) {
              zcyl(r = 4, l = 0.2);
            }
          }
        }
        rotate([0, 0, 90]) {
          translate([8, 0, 0]) {
            translate([0, 0, 0.1]) {
              zcyl(r = 4, l = 0.2);
            }
          }
        }
      }
      rotate([0, 0, 180]) {
        translate([8, 0, 0]) {
          translate([0, 0, 0.1]) {
            zcyl(r = 4, l = 0.2);
          }
        }
      }
    }
    rotate([0, 0, 270]) {
      translate([8, 0, 0]) {
        translate([0, 0, 0.1]) {
          zcyl(r = 4, l = 0.2);
        }
      }
    }
  }
}
$fn = 50;