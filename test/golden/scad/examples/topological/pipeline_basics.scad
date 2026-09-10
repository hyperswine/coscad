linear_extrude(height = 10) {
  translate([0, 100, 0]) {
    translate([100, 0, 0]) {
      circle(r = 8, $fn = 3);
    }
  }
}
$fn = 50;