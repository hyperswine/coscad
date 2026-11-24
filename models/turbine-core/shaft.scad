include <BOSL2/std.scad>

union() {
  union() {
    union() {
      translate([0, 0, 65]) {
        zcyl(r = 4, l = 130);
      }
      translate([0, 0, 12]) {
        zcyl(r = 5, l = 12);
      }
    }
    translate([0, 0, 108]) {
      zcyl(r = 5, l = 12);
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