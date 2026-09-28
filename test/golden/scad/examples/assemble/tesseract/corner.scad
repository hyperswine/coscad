include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          difference() {
            difference() {
              translate([11, 11, 11]) {
                cuboid([22, 22, 22]);
              }
              translate([19.6, 11, 11]) {
                cuboid([5.6, 6.4, 6.4]);
              }
            }
            translate([11, 19.6, 11]) {
              cuboid([6.4, 5.6, 6.4]);
            }
          }
          translate([11, 11, 19.6]) {
            cuboid([6.4, 6.4, 5.6]);
          }
        }
        translate([19.5, 11, 11]) {
          zcyl(r = 1.7, l = 30);
        }
      }
      translate([11, 19.5, 11]) {
        zcyl(r = 1.7, l = 30);
      }
    }
    translate([11, 11, 19.5]) {
      xcyl(r = 1.7, l = 30);
    }
  }
  translate([19, 19, 19]) {
    rotate([0, 0, 45]) {
      rotate([0, 54.7356, 0]) {
        zcyl(r = 2.65, l = 12);
      }
    }
  }
}
$fn = 50;