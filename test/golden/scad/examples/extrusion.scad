union() {
  union() {
    translate([-20.0, 0, 0]) {
      linear_extrude(height = 15.0) {
        circle(r = 8.0, $fn = 3);
      }
    }
    translate([0.0, 0, 0]) {
      linear_extrude(height = 12.0) {
        circle(r = 6.0, $fn = 5);
      }
    }
  }
  translate([20.0, 0, 0]) {
    linear_extrude(height = 10.0) {
      circle(r = 4.0, $fn = 100);
    }
  }
}
$fn = 50;