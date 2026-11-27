include <BOSL2/std.scad>

translate([-15, 85, 0]) {
  difference() {
    translate([0, 0, 14.2]) {
      zcyl(r = 6, l = 2);
    }
    zcyl(r = 2.8, l = 40);
  }
}
$fn = 50;