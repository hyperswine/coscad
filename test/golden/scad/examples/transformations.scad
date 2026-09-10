union() {
  union() {
    union() {
      union() {
        union() {
          union() {
            translate([15, 0, 0]) {
              cube([10, 10, 10]);
            }
            translate([0, 10, 0]) {
              sphere(5);
            }
          }
          translate([0, 0, 5]) {
            cylinder(h = 8, r = 3);
          }
        }
        rotate([45, 0, 0]) {
          cube([10, 10, 10]);
        }
      }
      rotate([0, 30, 0]) {
        sphere(5);
      }
    }
    rotate([0, 0, 60]) {
      cylinder(h = 8, r = 3);
    }
  }
  scale([2, 1.5, 0.5]) {
    cube([10, 10, 10]);
  }
}
$fn = 50;