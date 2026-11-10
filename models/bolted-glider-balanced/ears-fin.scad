union() {
  union() {
    union() {
      translate([6, 5, 0]) {
        cylinder(h = 0.2, r = 6);
      }
      translate([6, 35, 0]) {
        cylinder(h = 0.2, r = 6);
      }
    }
    translate([-6, 5, 0]) {
      cylinder(h = 0.2, r = 6);
    }
  }
  translate([-6, 35, 0]) {
    cylinder(h = 0.2, r = 6);
  }
}
$fn = 50;