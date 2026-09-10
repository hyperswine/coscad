union() {
  difference() {
    linear_extrude(height = 2.0) {
      circle(r = 10.0, $fn = 100);
    }
    linear_extrude(height = 2.0) {
      circle(r = 5.0, $fn = 100);
    }
  }
  union() {
    union() {
      union() {
        union() {
          union() {
            union() {
              union() {
                translate([10.0, 0, 0]) {
                  linear_extrude(height = 2.0) {
                    circle(r = 2.0, $fn = 3);
                  }
                }
                translate([7.07, 0, 0]) {
                  translate([0, 7.07, 0]) {
                    rotate([0, 0, 45.0]) {
                      linear_extrude(height = 2.0) {
                        circle(r = 2.0, $fn = 3);
                      }
                    }
                  }
                }
              }
              translate([0, 10.0, 0]) {
                rotate([0, 0, 90.0]) {
                  linear_extrude(height = 2.0) {
                    circle(r = 2.0, $fn = 3);
                  }
                }
              }
            }
            translate([-7.07, 0, 0]) {
              translate([0, 7.07, 0]) {
                rotate([0, 0, 135.0]) {
                  linear_extrude(height = 2.0) {
                    circle(r = 2.0, $fn = 3);
                  }
                }
              }
            }
          }
          translate([-10.0, 0, 0]) {
            rotate([0, 0, 180.0]) {
              linear_extrude(height = 2.0) {
                circle(r = 2.0, $fn = 3);
              }
            }
          }
        }
        translate([-7.07, 0, 0]) {
          translate([0, -7.07, 0]) {
            rotate([0, 0, 225.0]) {
              linear_extrude(height = 2.0) {
                circle(r = 2.0, $fn = 3);
              }
            }
          }
        }
      }
      translate([0, -10.0, 0]) {
        rotate([0, 0, 270.0]) {
          linear_extrude(height = 2.0) {
            circle(r = 2.0, $fn = 3);
          }
        }
      }
    }
    translate([7.07, 0, 0]) {
      translate([0, -7.07, 0]) {
        rotate([0, 0, 315.0]) {
          linear_extrude(height = 2.0) {
            circle(r = 2.0, $fn = 3);
          }
        }
      }
    }
  }
}
$fn = 50;