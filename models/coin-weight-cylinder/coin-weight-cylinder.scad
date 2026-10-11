difference() {
  cylinder(h = 50, r = 15);
  translate([-12.8, 0, 0]) {
    translate([0, -1.8, 0]) {
      translate([0, 0, 24]) {
        cube([25.6, 3.6, 27]);
      }
    }
  }
}
$fn = 50;