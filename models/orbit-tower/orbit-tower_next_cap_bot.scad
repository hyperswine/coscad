difference() {
  union() {
    cylinder(h = 7, r = 9);
    translate([0, 0, 7]) {
      intersection() {
        cylinder(h = 6, r1 = 9, r2 = 0);
        cylinder(h = 4, r = 9);
      }
    }
  }
  translate([-3.3, -3.3, -0.2]) {
    cube([6.6, 6.6, 5.8]);
  }
}
$fn = 50;