include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      hull() {
        translate([100, 30, 16]) {
          zcyl(r = 6, l = 4);
        }
        translate([100, -30, 16]) {
          zcyl(r = 6, l = 4);
        }
      }
      translate([100, 0, 0]) {
        zcyl(r = 4.3, l = 40);
      }
    }
    translate([100, 30, 0]) {
      zcyl(r = 2.8, l = 40);
    }
  }
  translate([100, -30, 0]) {
    zcyl(r = 2.8, l = 40);
  }
}
$fn = 50;