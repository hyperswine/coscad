include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        translate([-6, 63.5, 26.2]) {
          cuboid([62, 5, 18]);
        }
        translate([-31, 63.5, 26.2]) {
          ycyl(r = 2.75, l = 7);
        }
      }
      translate([-16, 63.5, 26.2]) {
        ycyl(r = 2.75, l = 7);
      }
    }
    translate([4, 63.5, 26.2]) {
      ycyl(r = 2.75, l = 7);
    }
  }
  translate([19, 63.5, 26.2]) {
    ycyl(r = 2.75, l = 7);
  }
}
$fn = 50;