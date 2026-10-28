include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          cuboid([230, 20, 20]);
          union() {
            union() {
              translate([0, 0, 9.95]) {
                cuboid([232, 6.4, 3.1]);
              }
              translate([0, 0, 7]) {
                cuboid([232, 10, 3]);
              }
            }
            translate([0, 0, 6]) {
              cuboid([232, 5.6, 5]);
            }
          }
        }
        rotate([90, 0, 0]) {
          union() {
            union() {
              translate([0, 0, 9.95]) {
                cuboid([232, 6.4, 3.1]);
              }
              translate([0, 0, 7]) {
                cuboid([232, 10, 3]);
              }
            }
            translate([0, 0, 6]) {
              cuboid([232, 5.6, 5]);
            }
          }
        }
      }
      rotate([180, 0, 0]) {
        union() {
          union() {
            translate([0, 0, 9.95]) {
              cuboid([232, 6.4, 3.1]);
            }
            translate([0, 0, 7]) {
              cuboid([232, 10, 3]);
            }
          }
          translate([0, 0, 6]) {
            cuboid([232, 5.6, 5]);
          }
        }
      }
    }
    rotate([270, 0, 0]) {
      union() {
        union() {
          translate([0, 0, 9.95]) {
            cuboid([232, 6.4, 3.1]);
          }
          translate([0, 0, 7]) {
            cuboid([232, 10, 3]);
          }
        }
        translate([0, 0, 6]) {
          cuboid([232, 5.6, 5]);
        }
      }
    }
  }
  xcyl(r = 2.1, l = 232);
}
$fn = 50;