include <BOSL2/std.scad>

translate([-1.5, 0, 0]) {
  rotate([0, 0, 90]) {
    rotate([90, 0, 0]) {
      difference() {
        difference() {
          difference() {
            linear_extrude(height = 3) {
              polygon(points = [[7, -22], [20, -22], [230, -4], [236, -4], [236, 4], [230, 4], [20, 22], [7, 22]]);
            }
            translate([13.5, 12, 1.5]) {
              zcyl(r = 2.2, l = 5);
            }
          }
          translate([13.5, -12, 1.5]) {
            zcyl(r = 2.2, l = 5);
          }
        }
        translate([232.5, 0, 1.5]) {
          zcyl(r = 0.9, l = 5);
        }
      }
    }
  }
}
$fn = 50;