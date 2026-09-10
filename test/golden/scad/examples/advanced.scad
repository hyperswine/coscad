difference() {
  union() {
    cube([20.0, 20.0, 20.0]);
    translate([0, 15.0, 0]) {
      sphere(8.0);
    }
  }
  rotate([45.0, 0, 0]) {
    cylinder(h = 25.0, r = 3.0);
  }
}
$fn = 50;