include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        cuboid([73.5, 6, 6]);
        translate([-33.85, 0, 0]) {
          zcyl(r = 1.7, l = 20);
        }
      }
      translate([-33.85, 0, 0]) {
        ycyl(r = 1.7, l = 20);
      }
    }
    translate([33.85, 0, 0]) {
      zcyl(r = 1.7, l = 20);
    }
  }
  translate([33.85, 0, 0]) {
    ycyl(r = 1.7, l = 20);
  }
}
$fn = 50;