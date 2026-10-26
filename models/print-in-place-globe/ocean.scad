translate([0, 0, 54]) {
  difference() {
    sphere(32);
    translate([0, 0, -40]) {
      cylinder(h = 80, r = 3.1);
    }
  }
}
$fn = 96;