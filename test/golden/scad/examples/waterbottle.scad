difference() {
  difference() {
    linear_extrude(height = 80) {
      circle(r = 25, $fn = 100);
    }
    translate([0, 0, 2]) {
      linear_extrude(height = 78) {
        circle(r = 22, $fn = 100);
      }
    }
  }
  translate([0, 0, 82]) {
    linear_extrude(height = 18) {
      circle(r = 6, $fn = 100);
    }
  }
}
$fn = 50;