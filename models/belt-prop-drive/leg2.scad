include <BOSL2/std.scad>

translate([106, 92, 0]) {
  difference() {
    difference() {
      translate([0, 0, -6]) {
        zcyl(r = 8, l = 12);
      }
      translate([0, 0, -4.19]) {
        cylinder(h = 4.2, r = 4.792, $fn = 6);
      }
    }
    translate([0, 0, -10]) {
      zcyl(r = 2.8, l = 30);
    }
  }
}
$fn = 50;