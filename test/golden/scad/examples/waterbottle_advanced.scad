difference() {
  difference() {
    union() {
      union() {
        union() {
          linear_extrude(height = 5) {
            offset(r = 2) {
              circle(r = 24, $fn = 100);
            }
          }
          linear_extrude(height = 70) {
            offset(r = 2) {
              circle(r = 22, $fn = 100);
            }
          }
        }
        translate([0, 0, 70]) {
          linear_extrude(height = 25) {
            circle(r = 8, $fn = 100);
          }
        }
      }
      translate([0, 0, 95]) {
        linear_extrude(height = 12) {
          offset(r = 2) {
            circle(r = 10, $fn = 100);
          }
        }
      }
    }
    translate([0, 0, 2]) {
      linear_extrude(height = 68) {
        circle(r = 19, $fn = 100);
      }
    }
  }
  translate([0, 0, 72]) {
    linear_extrude(height = 23) {
      circle(r = 6, $fn = 100);
    }
  }
}
$fn = 50;