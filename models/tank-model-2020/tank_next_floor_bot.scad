include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        cuboid([70, 70, 5]);
        translate([35, 35, 0]) {
          cuboid([10.6, 10.6, 7]);
        }
      }
      translate([-35, 35, 0]) {
        cuboid([10.6, 10.6, 7]);
      }
    }
    translate([35, -35, 0]) {
      cuboid([10.6, 10.6, 7]);
    }
  }
  translate([-35, -35, 0]) {
    cuboid([10.6, 10.6, 7]);
  }
}
$fn = 50;