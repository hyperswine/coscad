include <BOSL2/std.scad>

difference() {
  xcyl(r = 2.5, l = 54.337168574);
  translate([0, 0, -5.5]) {
    cuboid([56.337168574, 8, 8]);
  }
}
$fn = 50;