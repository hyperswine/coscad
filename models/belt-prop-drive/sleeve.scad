include <BOSL2/std.scad>

translate([-53, -78, 0]) {
  difference() {
    union() {
      translate([0, 0, 38.75]) {
        zcyl(r = 4, l = 16.5);
      }
      translate([0, 0, 47.75]) {
        zcyl(r = 6.5, l = 1.5);
      }
    }
    translate([0, 0, 30.5]) {
      zcyl(r = 2.75, l = 40);
    }
  }
}
$fn = 50;