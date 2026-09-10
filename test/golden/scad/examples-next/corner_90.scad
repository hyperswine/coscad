include <BOSL2/std.scad>

difference() {
  difference() {
    union() {
      cuboid([17.0, 4.0, 18.0]);
      translate([6.5, -4.5, 0.0]) {
        cuboid([4.0, 13.0, 18.0]);
      }
    }
    translate([-1.5, 0.0, 0.0]) {
      ycyl(r = 2.75, l = 8.0);
    }
  }
  translate([6.5, -8.0, 0.0]) {
    xcyl(r = 2.75, l = 8.0);
  }
}
$fn = 50;