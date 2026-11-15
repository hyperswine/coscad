include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      union() {
        translate([-96.85, 0, -11]) {
          cuboid([232.7, 12, 10]);
        }
        translate([-209.2, 0, -1]) {
          cuboid([8, 18, 10]);
        }
      }
      translate([6, 0, -11]) {
        zcyl(r = 2.2, l = 12);
      }
    }
    translate([14, 0, -11]) {
      zcyl(r = 2.2, l = 12);
    }
  }
  translate([-193.2, 0, -11]) {
    zcyl(r = 2.2, l = 12);
  }
}
$fn = 50;