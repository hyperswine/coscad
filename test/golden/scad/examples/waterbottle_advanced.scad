difference() {
  difference() {
    union() {
      union() {
        union() {
          linear_extrude(height = 5.0) {
            offset(r = 2.0) {
              circle(r = 24.0, $fn = 100);
            }
          }
          linear_extrude(height = 70.0) {
            offset(r = 2.0) {
              circle(r = 22.0, $fn = 100);
            }
          }
        }
        translate([0, 0, 70.0]) {
          linear_extrude(height = 25.0) {
            circle(r = 8.0, $fn = 100);
          }
        }
      }
      translate([0, 0, 95.0]) {
        linear_extrude(height = 12.0) {
          offset(r = 2.0) {
            circle(r = 10.0, $fn = 100);
          }
        }
      }
    }
    translate([0, 0, 2.0]) {
      linear_extrude(height = 68.0) {
        circle(r = 19.0, $fn = 100);
      }
    }
  }
  translate([0, 0, 72.0]) {
    linear_extrude(height = 23.0) {
      circle(r = 6.0, $fn = 100);
    }
  }
}
$fn = 50;