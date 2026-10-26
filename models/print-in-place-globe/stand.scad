union() {
  union() {
    union() {
      union() {
        union() {
          hull() {
            cylinder(h = 5, r = 43);
            translate([0, 0, 6.8]) {
              cylinder(h = 0.2, r = 41);
            }
          }
          translate([0, 0, 6]) {
            cylinder(h = 12, r = 9);
          }
        }
        translate([0, 0, 54]) {
          rotate([90, 0, 0]) {
            translate([0, 0, -3]) {
              difference() {
                cylinder(h = 6, r = 39.5);
                translate([0, 0, -0.1]) {
                  cylinder(h = 6.2, r = 36);
                }
              }
            }
          }
        }
      }
      translate([0, 0, 16]) {
        cylinder(h = 78, r = 2.5);
      }
    }
    translate([0, 0, 18]) {
      cylinder(h = 6.4, r = 4.5);
    }
  }
  hull() {
    translate([0, 0, 86.6]) {
      cylinder(h = 0.1, r = 2.5);
    }
    translate([0, 0, 88.6]) {
      cylinder(h = 3.5, r = 4.5);
    }
  }
}
$fn = 96;