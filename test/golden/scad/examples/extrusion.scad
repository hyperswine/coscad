union() {
  union() {
    translate([-20, 0, 0]) {
      linear_extrude(height = 15) {
        circle(r = 8, $fn = 3);
      }
    }
    translate([0, 0, 0]) {
      linear_extrude(height = 12) {
        circle(r = 6, $fn = 5);
      }
    }
  }
  translate([20, 0, 0]) {
    linear_extrude(height = 10) {
      circle(r = 4, $fn = 100);
    }
  }
}
$fn = 50;