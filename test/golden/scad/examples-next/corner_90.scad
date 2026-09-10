include <BOSL2/std.scad>

difference() {
  difference() {
    union() {
      cuboid([17, 4, 18]);
      translate([6.5, -4.5, 0]) {
        cuboid([4, 13, 18]);
      }
    }
    translate([-1.5, 0, 0]) {
      ycyl(r = 2.75, l = 8);
    }
  }
  translate([6.5, -8, 0]) {
    xcyl(r = 2.75, l = 8);
  }
}
$fn = 50;