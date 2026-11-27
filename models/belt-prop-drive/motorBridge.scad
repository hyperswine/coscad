include <BOSL2/std.scad>

difference() {
  difference() {
    translate([0, 0, 2.5]) {
      cuboid([15, 48, 5]);
    }
    translate([0, 18, 3]) {
      zcyl(r = 2.8, l = 12);
    }
  }
  translate([0, -18, 3]) {
    zcyl(r = 2.8, l = 12);
  }
}
$fn = 50;