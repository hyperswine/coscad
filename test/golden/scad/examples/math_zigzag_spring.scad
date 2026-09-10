difference() {
  difference() {
    cube([20.0, 5.0, 2.0]);
    translate([2.0, 0, 0]) {
      translate([0, 2.5, 0]) {
        linear_extrude(height = 2.0) {
          circle(r = 1.0, $fn = 100);
        }
      }
    }
  }
  translate([18.0, 0, 0]) {
    translate([0, 2.5, 0]) {
      linear_extrude(height = 2.0) {
        circle(r = 1.0, $fn = 100);
      }
    }
  }
}
$fn = 50;