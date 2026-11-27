include <BOSL2/std.scad>

translate([-15, 85, 0]) {
  difference() {
    union() {
      translate([0, 0, 3.5]) {
        zcyl(r = 6, l = 1);
      }
      translate([0, 0, 8.6]) {
        zcyl(r = 4, l = 9.2);
      }
    }
    zcyl(r = 2.8, l = 40);
  }
}
$fn = 50;