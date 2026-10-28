include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          cuboid([160, 20, 20]);
          union() {
            union() {
              translate([0, 0, 9.95]) {
                cuboid([162, 6.4, 3.1]);
              }
              translate([0, 0, 7]) {
                cuboid([162, 10, 3]);
              }
            }
            translate([0, 0, 6]) {
              cuboid([162, 5.6, 5]);
            }
          }
        }
        rotate([90, 0, 0]) {
          union() {
            union() {
              translate([0, 0, 9.95]) {
                cuboid([162, 6.4, 3.1]);
              }
              translate([0, 0, 7]) {
                cuboid([162, 10, 3]);
              }
            }
            translate([0, 0, 6]) {
              cuboid([162, 5.6, 5]);
            }
          }
        }
      }
      rotate([180, 0, 0]) {
        union() {
          union() {
            translate([0, 0, 9.95]) {
              cuboid([162, 6.4, 3.1]);
            }
            translate([0, 0, 7]) {
              cuboid([162, 10, 3]);
            }
          }
          translate([0, 0, 6]) {
            cuboid([162, 5.6, 5]);
          }
        }
      }
    }
    rotate([270, 0, 0]) {
      union() {
        union() {
          translate([0, 0, 9.95]) {
            cuboid([162, 6.4, 3.1]);
          }
          translate([0, 0, 7]) {
            cuboid([162, 10, 3]);
          }
        }
        translate([0, 0, 6]) {
          cuboid([162, 5.6, 5]);
        }
      }
    }
  }
  xcyl(r = 2.1, l = 162);
}
$fn = 50;