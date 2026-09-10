difference() {
  difference() {
    linear_extrude(height = 80.0) {
      circle(r = 25.0, $fn = 100);
    }
    translate([0, 0, 2.0]) {
      linear_extrude(height = 78.0) {
        circle(r = 22.0, $fn = 100);
      }
    }
  }
  translate([0, 0, 82.0]) {
    linear_extrude(height = 18.0) {
      circle(r = 6.0, $fn = 100);
    }
  }
}
$fn = 50;