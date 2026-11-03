include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      cuboid([40, 40, 4]);
      translate([10, 10, 0]) {
        cuboid([20, 20, 6]);
      }
    }
    translate([10, -10, 0]) {
      zcyl(r = 2.75, l = 6);
    }
  }
  translate([-10, 10, 0]) {
    zcyl(r = 2.75, l = 6);
  }
}
$fn = 50;