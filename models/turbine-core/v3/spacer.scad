include <BOSL2/std.scad>

difference() {
  translate([0, 0, 4.5]) {
    zcyl(r = 6, l = 9);
  }
  translate([0, 0, 4.5]) {
    zcyl(r = 4.15, l = 11);
  }
}
$fn = 50;