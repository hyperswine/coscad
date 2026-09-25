include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        cuboid([190, 190, 3]);
        translate([80, 80, 1.5]) {
          zcyl(r = 2.75, l = 10);
        }
      }
      translate([-80, 80, 1.5]) {
        zcyl(r = 2.75, l = 10);
      }
    }
    translate([80, -80, 1.5]) {
      zcyl(r = 2.75, l = 10);
    }
  }
  translate([-80, -80, 1.5]) {
    zcyl(r = 2.75, l = 10);
  }
}
$fn = 50;