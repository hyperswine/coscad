include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        cuboid([66, 6, 6]);
        translate([-30.5, 0, 0]) {
          zcyl(r = 1.3, l = 20);
        }
      }
      translate([-30.5, 0, 0]) {
        ycyl(r = 1.3, l = 20);
      }
    }
    translate([30.5, 0, 0]) {
      zcyl(r = 1.3, l = 20);
    }
  }
  translate([30.5, 0, 0]) {
    ycyl(r = 1.3, l = 20);
  }
}
$fn = 50;