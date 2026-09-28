include <BOSL2/std.scad>

difference() {
  xcyl(r = 2.5, l = 46);
  translate([0, 0, -5.5]) {
    cuboid([50, 8, 8]);
  }
}
$fn = 50;