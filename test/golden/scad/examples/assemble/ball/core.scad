include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        cyl(r = 14.8, h = 16);
        translate([0, 0, 4]) {
          xcyl(r = 2.75, l = 40);
        }
      }
      translate([6, 0, 3.825]) {
        cuboid([4.4, 8.4, 9.35]);
      }
    }
    translate([0, 0, -4]) {
      ycyl(r = 2.75, l = 40);
    }
  }
  translate([0, 6, -3.825]) {
    cuboid([8.4, 4.4, 9.35]);
  }
}
$fn = 50;