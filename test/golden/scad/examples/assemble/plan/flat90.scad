include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      cuboid([40, 40, 6]);
      translate([6, 6, 0]) {
        cuboid([28, 28, 8]);
      }
    }
    translate([10, -12, 3]) {
      zcyl(r = 2.75, l = 10);
    }
  }
  translate([-12, 10, 3]) {
    zcyl(r = 2.75, l = 10);
  }
}
$fn = 50;