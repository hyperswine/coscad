include <BOSL2/std.scad>

translate([60, -85, 0]) {
  difference() {
    union() {
      union() {
        translate([0, 0, 8.5]) {
          zcyl(r = 9, l = 7);
        }
        translate([0, 0, 4.5]) {
          zcyl(r = 11, l = 1);
        }
      }
      translate([0, 0, 12.5]) {
        zcyl(r = 11, l = 1);
      }
    }
    zcyl(r = 4.25, l = 40);
  }
}
$fn = 50;