include <BOSL2/std.scad>

translate([32, 0, 0]) {
  difference() {
    union() {
      translate([0, 0, 33.25]) {
        zcyl(r = 4, l = 16.5);
      }
      translate([0, 0, 42.25]) {
        zcyl(r = 6.5, l = 1.5);
      }
    }
    translate([0, 0, 25]) {
      zcyl(r = 2.75, l = 40);
    }
  }
}
$fn = 50;