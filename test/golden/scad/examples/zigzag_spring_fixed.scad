union() {
  union() {
    union() {
      union() {
        union() {
          difference() {
            cube([4.0, 5.0, 2.0]);
            translate([2.0, 0, 0]) {
              translate([0, 2.5, 0]) {
                linear_extrude(height = 2.0) {
                  circle(r = 1.0, $fn = 100);
                }
              }
            }
          }
          difference() {
            translate([16.0, 0, 0]) {
              cube([4.0, 5.0, 2.0]);
            }
            translate([18.0, 0, 0]) {
              translate([0, 2.5, 0]) {
                linear_extrude(height = 2.0) {
                  circle(r = 1.0, $fn = 100);
                }
              }
            }
          }
        }
        translate([4.0, 0, 0]) {
          translate([0, 3.5, 0]) {
            cube([3.0, 1.0, 2.0]);
          }
        }
      }
      translate([7.0, 0, 0]) {
        translate([0, 0.5, 0]) {
          cube([3.0, 1.0, 2.0]);
        }
      }
    }
    translate([10.0, 0, 0]) {
      translate([0, 3.5, 0]) {
        cube([3.0, 1.0, 2.0]);
      }
    }
  }
  translate([13.0, 0, 0]) {
    translate([0, 0.5, 0]) {
      cube([3.0, 1.0, 2.0]);
    }
  }
}
$fn = 50;