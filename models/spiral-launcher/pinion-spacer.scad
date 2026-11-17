include <BOSL2/std.scad>

difference() {
  translate([0, 0, 31]) {
    zcyl(r = 8.5, l = 4);
  }
  translate([0, 0, 31]) {
    zcyl(r = 6.3, l = 6);
  }
}
$fn = 50;