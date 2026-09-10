union() {
  union() {
    union() {
      union() {
        union() {
          union() {
            translate([15.0, 0, 0]) {
              cube([10.0, 10.0, 10.0]);
            }
            translate([0, 10.0, 0]) {
              sphere(5.0);
            }
          }
          translate([0, 0, 5.0]) {
            cylinder(h = 8.0, r = 3.0);
          }
        }
        rotate([45.0, 0, 0]) {
          cube([10.0, 10.0, 10.0]);
        }
      }
      rotate([0, 30.0, 0]) {
        sphere(5.0);
      }
    }
    rotate([0, 0, 60.0]) {
      cylinder(h = 8.0, r = 3.0);
    }
  }
  scale([2.0, 1.5, 0.5]) {
    cube([10.0, 10.0, 10.0]);
  }
}
$fn = 50;