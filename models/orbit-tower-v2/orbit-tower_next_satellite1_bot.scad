union() {
  union() {
    union() {
      cylinder(h = 4, r = 6.5);
      translate([0, 0, 4]) {
        intersection() {
          cylinder(h = 7.8, r1 = 6.5, r2 = 0);
          cylinder(h = 3, r = 6.5);
        }
      }
    }
    translate([0, 0, 6.8]) {
      cylinder(h = 3.6, r = 2.4);
    }
  }
  translate([0, 0, 10.3]) {
    intersection() {
      cylinder(h = 2.4, r1 = 2.4, r2 = 0);
      cylinder(h = 0.5, r = 2.4);
    }
  }
}
$fn = 50;