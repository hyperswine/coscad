difference() {
  union() {
    cube([20, 20, 20]);
    translate([0, 15, 0]) {
      sphere(8);
    }
  }
  rotate([45, 0, 0]) {
    cylinder(h = 25, r = 3);
  }
}
$fn = 50;