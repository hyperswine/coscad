union() {
  union() {
    union() {
      translate([-28, 0, 0]) {
        rotate_extrude(angle = 360, $fn = 64) {
          intersection() {
            circle(r = 4, $fn = 100);
            translate([0, -4]) square([4, 8]);
          }
        }
      }
      translate([-16, 0, 0]) {
        rotate_extrude(angle = 360, $fn = 64) {
          translate([4, 0, 0]) {
            circle(r = 3, $fn = 3);
          }
        }
      }
    }
    rotate_extrude(angle = 360, $fn = 64) {
      translate([6, 0, 0]) {
        circle(r = 2, $fn = 100);
      }
    }
  }
  translate([16, 0, 0]) {
    rotate_extrude(angle = 90, $fn = 64) {
      translate([3, 0, 0]) {
        circle(r = 2, $fn = 100);
      }
    }
  }
}
$fn = 50;