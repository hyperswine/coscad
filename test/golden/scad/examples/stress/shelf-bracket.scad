union() {
  union() {
    cube([60, 40, 6]);
    translate([0, 34, 0]) {
      cube([60, 6, 44]);
    }
  }
  translate([0, 0, 44]) {
    translate([0, 4, 0]) {
      cube([60, 36, 6]);
    }
  }
}
$fn = 50;