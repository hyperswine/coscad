difference() {
  difference() {
    cube([20, 5, 2]);
    translate([2, 0, 0]) {
      translate([0, 2.5, 0]) {
        linear_extrude(height = 2) {
          circle(r = 1, $fn = 100);
        }
      }
    }
  }
  translate([18, 0, 0]) {
    translate([0, 2.5, 0]) {
      linear_extrude(height = 2) {
        circle(r = 1, $fn = 100);
      }
    }
  }
}
$fn = 50;