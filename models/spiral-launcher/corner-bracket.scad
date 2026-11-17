include <BOSL2/std.scad>

difference() {
  difference() {
    union() {
      translate([0, 2.5, 8.1]) {
        cuboid([18, 5, 16.2]);
      }
      translate([0, 10, 2.5]) {
        cuboid([18, 20, 5]);
      }
    }
    translate([0, 2.5, 10]) {
      ycyl(r = 2.75, l = 7);
    }
  }
  translate([0, 13, 2.5]) {
    zcyl(r = 2.75, l = 7);
  }
}
$fn = 50;