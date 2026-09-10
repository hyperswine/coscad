union() {
  translate([-25, 0, 0]) {
    linear_extrude(height = 10) {
      circle(r = 15, $fn = 3);
    }
  }
  translate([25, 0, 0]) {
    linear_extrude(height = 10) {
      offset(r = 3) {
        circle(r = 15, $fn = 3);
      }
    }
  }
}
$fn = 50;