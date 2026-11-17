include <BOSL2/std.scad>

difference() {
  difference() {
    difference() {
      difference() {
        difference() {
          difference() {
            union() {
              union() {
                union() {
                  union() {
                    union() {
                      union() {
                        union() {
                          union() {
                            union() {
                              union() {
                                union() {
                                  translate([10, -2, -1.875]) {
                                    cuboid([132, 86, 3.85]);
                                  }
                                  translate([10, 39, 14.3]) {
                                    cuboid([132, 4, 36.2]);
                                  }
                                }
                                translate([0, 0, 18.65]) {
                                  zcyl(r = 6, l = 37.3);
                                }
                              }
                              translate([0, 0, 0.5]) {
                                zcyl(r = 10, l = 1);
                              }
                            }
                            translate([40, -3.2, 0.5]) {
                              cuboid([14, 7.6, 1]);
                            }
                          }
                          translate([42.75, -8.6, 4]) {
                            cuboid([14.5, 2.8, 8]);
                          }
                        }
                        translate([43, 2.3, 4]) {
                          cuboid([14, 3, 8]);
                        }
                      }
                      translate([48.6, -3.1, 4]) {
                        cuboid([2.8, 13.8, 8]);
                      }
                    }
                    translate([-50, 36.5, 26.2]) {
                      ycyl(r = 6, l = 1.05);
                    }
                  }
                  translate([-36, 36.5, 26.2]) {
                    ycyl(r = 6, l = 1.05);
                  }
                }
                translate([56, 36.5, 26.2]) {
                  ycyl(r = 6, l = 1.05);
                }
              }
              translate([70, 36.5, 26.2]) {
                ycyl(r = 6, l = 1.05);
              }
            }
            translate([0, 0, 33.65]) {
              zcyl(r = 2.75, l = 9.3);
            }
          }
          translate([-1.1, 0, 33.7]) {
            cuboid([11.8, 8.2, 4.2]);
          }
        }
        translate([-50, 38.5, 26.2]) {
          ycyl(r = 2.75, l = 7);
        }
      }
      translate([-36, 38.5, 26.2]) {
        ycyl(r = 2.75, l = 7);
      }
    }
    translate([56, 38.5, 26.2]) {
      ycyl(r = 2.75, l = 7);
    }
  }
  translate([70, 38.5, 26.2]) {
    ycyl(r = 2.75, l = 7);
  }
}
$fn = 50;