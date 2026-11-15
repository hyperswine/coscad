include <BOSL2/std.scad>

difference() {
  difference() {
    translate([0, 13, 0.7]) {
      cuboid([21, 4.5, 1.4]);
    }
    translate([-8, 13, 0.5]) {
      zcyl(r = 1.23, l = 3);
    }
  }
  translate([8, 13, 0.5]) {
    zcyl(r = 1.23, l = 3);
  }
}
$fn = 50;