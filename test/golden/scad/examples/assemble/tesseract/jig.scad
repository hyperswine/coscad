include <BOSL2/std.scad>

union() {
  translate([0, 0, 1]) {
    cuboid([26, 26, 2]);
  }
  union() {
    translate([-9, -9, 18.5]) {
      cuboid([3, 3, 33]);
    }
    translate([18, 0, 0]) {
      translate([-9, -9, 18.5]) {
        cuboid([3, 3, 33]);
      }
    }
    translate([0, 18, 0]) {
      translate([-9, -9, 18.5]) {
        cuboid([3, 3, 33]);
      }
    }
    translate([18, 18, 0]) {
      translate([-9, -9, 18.5]) {
        cuboid([3, 3, 33]);
      }
    }
  }
}
$fn = 50;