include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      translate([0, 0, 0.6]) {
        cuboid([25, 25, 1.2]);
      }
      translate([-8, 12.5, 0.6]) {
        cuboid([1.6, 3, 3]);
      }
    }
    translate([-4, 12.5, 0.6]) {
      cuboid([1.6, 3, 3]);
    }
  }
  translate([0, 12.5, 0.6]) {
    cuboid([1.6, 3, 3]);
  }
}
$fn = 50;