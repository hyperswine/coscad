union() {
  translate([-25.0, 0, 0]) {
    linear_extrude(height = 10.0) {
      circle(r = 15.0, $fn = 3);
    }
  }
  translate([25.0, 0, 0]) {
    linear_extrude(height = 10.0) {
      offset(r = 3.0) {
        circle(r = 15.0, $fn = 3);
      }
    }
  }
}
$fn = 50;