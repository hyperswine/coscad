include <BOSL2/std.scad>

rotate([0, 0, 45]) {
  scale([1.035, 1.035, 1.035]) {
    union() {
      union() {
        union() {
          union() {
            difference() {
              union() {
                union() {
                  union() {
                    hull() {
                      translate([0, -128, 0]) {
                        sphere(2.5);
                      }
                      translate([0, -95, 0]) {
                        sphere(9);
                      }
                    }
                    hull() {
                      translate([0, -95, 0]) {
                        sphere(9);
                      }
                      translate([0, -15, 0]) {
                        sphere(7);
                      }
                    }
                  }
                  hull() {
                    translate([0, -15, 0]) {
                      sphere(7);
                    }
                    translate([0, 70, 0]) {
                      sphere(3.5);
                    }
                  }
                }
                hull() {
                  translate([0, 70, 0]) {
                    sphere(3.5);
                  }
                  translate([0, 126, 0]) {
                    sphere(2.5);
                  }
                }
              }
              union() {
                union() {
                  hull() {
                    translate([0, -76, 0]) {
                      sphere(5.6);
                    }
                    translate([0, -39, 0]) {
                      sphere(4.9);
                    }
                  }
                  hull() {
                    translate([0, -23, 0]) {
                      sphere(4.6);
                    }
                    translate([0, 36, 0]) {
                      sphere(2.6);
                    }
                  }
                }
                hull() {
                  translate([0, 46, 0]) {
                    sphere(2.2);
                  }
                  translate([0, 94, 0]) {
                    sphere(1.2);
                  }
                }
              }
            }
            translate([0, 37, 0]) {
              union() {
                difference() {
                  union() {
                    hull() {
                      hull() {
                        translate([3, 0, 0]) {
                          translate([0, -40, 0]) {
                            translate([0, 0, -4]) {
                              zcyl(r = 1.5, l = 3);
                            }
                          }
                        }
                        translate([3, 0, 0]) {
                          translate([0, 45, 0]) {
                            translate([0, 0, -4]) {
                              zcyl(r = 1.5, l = 3);
                            }
                          }
                        }
                      }
                      translate([113, 0, 0]) {
                        translate([0, 45, 0]) {
                          translate([0, 0, 7]) {
                            zcyl(r = 1.5, l = 3);
                          }
                        }
                      }
                    }
                    hull() {
                      hull() {
                        hull() {
                          hull() {
                            hull() {
                              translate([0, -40, -4]) {
                                zcyl(r = 1.5, l = 6);
                              }
                              translate([0, 45, -4]) {
                                zcyl(r = 1.5, l = 6);
                              }
                            }
                            translate([3, -40, -4]) {
                              zcyl(r = 1.5, l = 6);
                            }
                          }
                          translate([3, 45, -4]) {
                            zcyl(r = 1.5, l = 6);
                          }
                        }
                        translate([20, -26.8636, -2.3]) {
                          zcyl(r = 1.5, l = 3);
                        }
                      }
                      translate([20, 45, -2.3]) {
                        zcyl(r = 1.5, l = 3);
                      }
                    }
                  }
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
                                                                                                                                union() {
                                                                                                                                  union() {
                                                                                                                                    union() {
                                                                                                                                      union() {
                                                                                                                                        hull() {
                                                                                                                                          hull() {
                                                                                                                                            hull() {
                                                                                                                                              translate([7, -36.2591, -3.6]) {
                                                                                                                                                zcyl(r = 0.12, l = 4.0941);
                                                                                                                                              }
                                                                                                                                              translate([7, -29.3682, -3.6]) {
                                                                                                                                                zcyl(r = 0.12, l = 4.0941);
                                                                                                                                              }
                                                                                                                                            }
                                                                                                                                            translate([19.4, -26.6773, -2.36]) {
                                                                                                                                              zcyl(r = 0.12, l = 1.9059);
                                                                                                                                            }
                                                                                                                                          }
                                                                                                                                          translate([19.4, -20.7445, -2.36]) {
                                                                                                                                            zcyl(r = 0.12, l = 1.9059);
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                        hull() {
                                                                                                                                          hull() {
                                                                                                                                            hull() {
                                                                                                                                              translate([7, -28.0682, -3.6]) {
                                                                                                                                                zcyl(r = 0.12, l = 4.0941);
                                                                                                                                              }
                                                                                                                                              translate([7, -21.1773, -3.6]) {
                                                                                                                                                zcyl(r = 0.12, l = 4.0941);
                                                                                                                                              }
                                                                                                                                            }
                                                                                                                                            translate([19.4, -19.4445, -2.36]) {
                                                                                                                                              zcyl(r = 0.12, l = 1.9059);
                                                                                                                                            }
                                                                                                                                          }
                                                                                                                                          translate([19.4, -13.5118, -2.36]) {
                                                                                                                                            zcyl(r = 0.12, l = 1.9059);
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                      hull() {
                                                                                                                                        hull() {
                                                                                                                                          hull() {
                                                                                                                                            translate([7, -19.8773, -3.6]) {
                                                                                                                                              zcyl(r = 0.12, l = 4.0941);
                                                                                                                                            }
                                                                                                                                            translate([7, -12.9864, -3.6]) {
                                                                                                                                              zcyl(r = 0.12, l = 4.0941);
                                                                                                                                            }
                                                                                                                                          }
                                                                                                                                          translate([19.4, -12.2118, -2.36]) {
                                                                                                                                            zcyl(r = 0.12, l = 1.9059);
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                        translate([19.4, -6.2791, -2.36]) {
                                                                                                                                          zcyl(r = 0.12, l = 1.9059);
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                    hull() {
                                                                                                                                      hull() {
                                                                                                                                        hull() {
                                                                                                                                          translate([7, -11.6864, -3.6]) {
                                                                                                                                            zcyl(r = 0.12, l = 4.0941);
                                                                                                                                          }
                                                                                                                                          translate([7, -4.7955, -3.6]) {
                                                                                                                                            zcyl(r = 0.12, l = 4.0941);
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                        translate([19.4, -4.9791, -2.36]) {
                                                                                                                                          zcyl(r = 0.12, l = 1.9059);
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                      translate([19.4, 0.9536, -2.36]) {
                                                                                                                                        zcyl(r = 0.12, l = 1.9059);
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                  hull() {
                                                                                                                                    hull() {
                                                                                                                                      hull() {
                                                                                                                                        translate([7, -3.4955, -3.6]) {
                                                                                                                                          zcyl(r = 0.12, l = 4.0941);
                                                                                                                                        }
                                                                                                                                        translate([7, 3.3955, -3.6]) {
                                                                                                                                          zcyl(r = 0.12, l = 4.0941);
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                      translate([19.4, 2.2536, -2.36]) {
                                                                                                                                        zcyl(r = 0.12, l = 1.9059);
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                    translate([19.4, 8.1864, -2.36]) {
                                                                                                                                      zcyl(r = 0.12, l = 1.9059);
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                }
                                                                                                                                hull() {
                                                                                                                                  hull() {
                                                                                                                                    hull() {
                                                                                                                                      translate([7, 4.6955, -3.6]) {
                                                                                                                                        zcyl(r = 0.12, l = 4.0941);
                                                                                                                                      }
                                                                                                                                      translate([7, 11.5864, -3.6]) {
                                                                                                                                        zcyl(r = 0.12, l = 4.0941);
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                    translate([19.4, 9.4864, -2.36]) {
                                                                                                                                      zcyl(r = 0.12, l = 1.9059);
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                  translate([19.4, 15.4191, -2.36]) {
                                                                                                                                    zcyl(r = 0.12, l = 1.9059);
                                                                                                                                  }
                                                                                                                                }
                                                                                                                              }
                                                                                                                              hull() {
                                                                                                                                hull() {
                                                                                                                                  hull() {
                                                                                                                                    translate([7, 12.8864, -3.6]) {
                                                                                                                                      zcyl(r = 0.12, l = 4.0941);
                                                                                                                                    }
                                                                                                                                    translate([7, 19.7773, -3.6]) {
                                                                                                                                      zcyl(r = 0.12, l = 4.0941);
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                  translate([19.4, 16.7191, -2.36]) {
                                                                                                                                    zcyl(r = 0.12, l = 1.9059);
                                                                                                                                  }
                                                                                                                                }
                                                                                                                                translate([19.4, 22.6518, -2.36]) {
                                                                                                                                  zcyl(r = 0.12, l = 1.9059);
                                                                                                                                }
                                                                                                                              }
                                                                                                                            }
                                                                                                                            hull() {
                                                                                                                              hull() {
                                                                                                                                hull() {
                                                                                                                                  translate([7, 21.0773, -3.6]) {
                                                                                                                                    zcyl(r = 0.12, l = 4.0941);
                                                                                                                                  }
                                                                                                                                  translate([7, 27.9682, -3.6]) {
                                                                                                                                    zcyl(r = 0.12, l = 4.0941);
                                                                                                                                  }
                                                                                                                                }
                                                                                                                                translate([19.4, 23.9518, -2.36]) {
                                                                                                                                  zcyl(r = 0.12, l = 1.9059);
                                                                                                                                }
                                                                                                                              }
                                                                                                                              translate([19.4, 29.8845, -2.36]) {
                                                                                                                                zcyl(r = 0.12, l = 1.9059);
                                                                                                                              }
                                                                                                                            }
                                                                                                                          }
                                                                                                                          hull() {
                                                                                                                            hull() {
                                                                                                                              hull() {
                                                                                                                                translate([7, 29.2682, -3.6]) {
                                                                                                                                  zcyl(r = 0.12, l = 4.0941);
                                                                                                                                }
                                                                                                                                translate([7, 36.1591, -3.6]) {
                                                                                                                                  zcyl(r = 0.12, l = 4.0941);
                                                                                                                                }
                                                                                                                              }
                                                                                                                              translate([19.4, 31.1845, -2.36]) {
                                                                                                                                zcyl(r = 0.12, l = 1.9059);
                                                                                                                              }
                                                                                                                            }
                                                                                                                            translate([19.4, 37.1173, -2.36]) {
                                                                                                                              zcyl(r = 0.12, l = 1.9059);
                                                                                                                            }
                                                                                                                          }
                                                                                                                        }
                                                                                                                        hull() {
                                                                                                                          hull() {
                                                                                                                            hull() {
                                                                                                                              translate([7, 37.4591, -3.6]) {
                                                                                                                                zcyl(r = 0.12, l = 4.0941);
                                                                                                                              }
                                                                                                                              translate([7, 44.35, -3.6]) {
                                                                                                                                zcyl(r = 0.12, l = 4.0941);
                                                                                                                              }
                                                                                                                            }
                                                                                                                            translate([19.4, 38.4173, -2.36]) {
                                                                                                                              zcyl(r = 0.12, l = 1.9059);
                                                                                                                            }
                                                                                                                          }
                                                                                                                          translate([19.4, 44.35, -2.36]) {
                                                                                                                            zcyl(r = 0.12, l = 1.9059);
                                                                                                                          }
                                                                                                                        }
                                                                                                                      }
                                                                                                                      hull() {
                                                                                                                        hull() {
                                                                                                                          hull() {
                                                                                                                            translate([20.6, -25.75, -2.24]) {
                                                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                                                            }
                                                                                                                            translate([20.6, -19.91, -2.24]) {
                                                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                                                            }
                                                                                                                          }
                                                                                                                          translate([34, -15.3955, -0.9]) {
                                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                                          }
                                                                                                                        }
                                                                                                                        translate([34, -10.5909, -0.9]) {
                                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                                        }
                                                                                                                      }
                                                                                                                    }
                                                                                                                    hull() {
                                                                                                                      hull() {
                                                                                                                        hull() {
                                                                                                                          translate([20.6, -18.61, -2.24]) {
                                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                                          }
                                                                                                                          translate([20.6, -12.77, -2.24]) {
                                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                                          }
                                                                                                                        }
                                                                                                                        translate([34, -9.2909, -0.9]) {
                                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                                        }
                                                                                                                      }
                                                                                                                      translate([34, -4.4864, -0.9]) {
                                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                                      }
                                                                                                                    }
                                                                                                                  }
                                                                                                                  hull() {
                                                                                                                    hull() {
                                                                                                                      hull() {
                                                                                                                        translate([20.6, -11.47, -2.24]) {
                                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                                        }
                                                                                                                        translate([20.6, -5.63, -2.24]) {
                                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                                        }
                                                                                                                      }
                                                                                                                      translate([34, -3.1864, -0.9]) {
                                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                                      }
                                                                                                                    }
                                                                                                                    translate([34, 1.6182, -0.9]) {
                                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                                    }
                                                                                                                  }
                                                                                                                }
                                                                                                                hull() {
                                                                                                                  hull() {
                                                                                                                    hull() {
                                                                                                                      translate([20.6, -4.33, -2.24]) {
                                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                                      }
                                                                                                                      translate([20.6, 1.51, -2.24]) {
                                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                                      }
                                                                                                                    }
                                                                                                                    translate([34, 2.9182, -0.9]) {
                                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                                    }
                                                                                                                  }
                                                                                                                  translate([34, 7.7227, -0.9]) {
                                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                                  }
                                                                                                                }
                                                                                                              }
                                                                                                              hull() {
                                                                                                                hull() {
                                                                                                                  hull() {
                                                                                                                    translate([20.6, 2.81, -2.24]) {
                                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                                    }
                                                                                                                    translate([20.6, 8.65, -2.24]) {
                                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                                    }
                                                                                                                  }
                                                                                                                  translate([34, 9.0227, -0.9]) {
                                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                                  }
                                                                                                                }
                                                                                                                translate([34, 13.8273, -0.9]) {
                                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                                }
                                                                                                              }
                                                                                                            }
                                                                                                            hull() {
                                                                                                              hull() {
                                                                                                                hull() {
                                                                                                                  translate([20.6, 9.95, -2.24]) {
                                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                                  }
                                                                                                                  translate([20.6, 15.79, -2.24]) {
                                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                                  }
                                                                                                                }
                                                                                                                translate([34, 15.1273, -0.9]) {
                                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                                }
                                                                                                              }
                                                                                                              translate([34, 19.9318, -0.9]) {
                                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                                              }
                                                                                                            }
                                                                                                          }
                                                                                                          hull() {
                                                                                                            hull() {
                                                                                                              hull() {
                                                                                                                translate([20.6, 17.09, -2.24]) {
                                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                                }
                                                                                                                translate([20.6, 22.93, -2.24]) {
                                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                                }
                                                                                                              }
                                                                                                              translate([34, 21.2318, -0.9]) {
                                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                                              }
                                                                                                            }
                                                                                                            translate([34, 26.0364, -0.9]) {
                                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                                            }
                                                                                                          }
                                                                                                        }
                                                                                                        hull() {
                                                                                                          hull() {
                                                                                                            hull() {
                                                                                                              translate([20.6, 24.23, -2.24]) {
                                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                                              }
                                                                                                              translate([20.6, 30.07, -2.24]) {
                                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                                              }
                                                                                                            }
                                                                                                            translate([34, 27.3364, -0.9]) {
                                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                                            }
                                                                                                          }
                                                                                                          translate([34, 32.1409, -0.9]) {
                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                          }
                                                                                                        }
                                                                                                      }
                                                                                                      hull() {
                                                                                                        hull() {
                                                                                                          hull() {
                                                                                                            translate([20.6, 31.37, -2.24]) {
                                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                                            }
                                                                                                            translate([20.6, 37.21, -2.24]) {
                                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                                            }
                                                                                                          }
                                                                                                          translate([34, 33.4409, -0.9]) {
                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                          }
                                                                                                        }
                                                                                                        translate([34, 38.2455, -0.9]) {
                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                        }
                                                                                                      }
                                                                                                    }
                                                                                                    hull() {
                                                                                                      hull() {
                                                                                                        hull() {
                                                                                                          translate([20.6, 38.51, -2.24]) {
                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                          }
                                                                                                          translate([20.6, 44.35, -2.24]) {
                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                          }
                                                                                                        }
                                                                                                        translate([34, 39.5455, -0.9]) {
                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                        }
                                                                                                      }
                                                                                                      translate([34, 44.35, -0.9]) {
                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                      }
                                                                                                    }
                                                                                                  }
                                                                                                  hull() {
                                                                                                    hull() {
                                                                                                      hull() {
                                                                                                        translate([35.2, -14.4682, -0.78]) {
                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                        }
                                                                                                        translate([35.2, -9.7564, -0.78]) {
                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                        }
                                                                                                      }
                                                                                                      translate([49, -3.8045, 0.6]) {
                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                      }
                                                                                                    }
                                                                                                    translate([49, -0.1591, 0.6]) {
                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                    }
                                                                                                  }
                                                                                                }
                                                                                                hull() {
                                                                                                  hull() {
                                                                                                    hull() {
                                                                                                      translate([35.2, -8.4564, -0.78]) {
                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                      }
                                                                                                      translate([35.2, -3.7445, -0.78]) {
                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                      }
                                                                                                    }
                                                                                                    translate([49, 1.1409, 0.6]) {
                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                    }
                                                                                                  }
                                                                                                  translate([49, 4.7864, 0.6]) {
                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                  }
                                                                                                }
                                                                                              }
                                                                                              hull() {
                                                                                                hull() {
                                                                                                  hull() {
                                                                                                    translate([35.2, -2.4445, -0.78]) {
                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                    }
                                                                                                    translate([35.2, 2.2673, -0.78]) {
                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                    }
                                                                                                  }
                                                                                                  translate([49, 6.0864, 0.6]) {
                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                  }
                                                                                                }
                                                                                                translate([49, 9.7318, 0.6]) {
                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                            hull() {
                                                                                              hull() {
                                                                                                hull() {
                                                                                                  translate([35.2, 3.5673, -0.78]) {
                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                  }
                                                                                                  translate([35.2, 8.2791, -0.78]) {
                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                  }
                                                                                                }
                                                                                                translate([49, 11.0318, 0.6]) {
                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                }
                                                                                              }
                                                                                              translate([49, 14.6773, 0.6]) {
                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                              }
                                                                                            }
                                                                                          }
                                                                                          hull() {
                                                                                            hull() {
                                                                                              hull() {
                                                                                                translate([35.2, 9.5791, -0.78]) {
                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                }
                                                                                                translate([35.2, 14.2909, -0.78]) {
                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                }
                                                                                              }
                                                                                              translate([49, 15.9773, 0.6]) {
                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                              }
                                                                                            }
                                                                                            translate([49, 19.6227, 0.6]) {
                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                            }
                                                                                          }
                                                                                        }
                                                                                        hull() {
                                                                                          hull() {
                                                                                            hull() {
                                                                                              translate([35.2, 15.5909, -0.78]) {
                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                              }
                                                                                              translate([35.2, 20.3027, -0.78]) {
                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                              }
                                                                                            }
                                                                                            translate([49, 20.9227, 0.6]) {
                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                            }
                                                                                          }
                                                                                          translate([49, 24.5682, 0.6]) {
                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                      hull() {
                                                                                        hull() {
                                                                                          hull() {
                                                                                            translate([35.2, 21.6027, -0.78]) {
                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                            }
                                                                                            translate([35.2, 26.3145, -0.78]) {
                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                            }
                                                                                          }
                                                                                          translate([49, 25.8682, 0.6]) {
                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                          }
                                                                                        }
                                                                                        translate([49, 29.5136, 0.6]) {
                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                    hull() {
                                                                                      hull() {
                                                                                        hull() {
                                                                                          translate([35.2, 27.6145, -0.78]) {
                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                          }
                                                                                          translate([35.2, 32.3264, -0.78]) {
                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                          }
                                                                                        }
                                                                                        translate([49, 30.8136, 0.6]) {
                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                        }
                                                                                      }
                                                                                      translate([49, 34.4591, 0.6]) {
                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                  hull() {
                                                                                    hull() {
                                                                                      hull() {
                                                                                        translate([35.2, 33.6264, -0.78]) {
                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                        }
                                                                                        translate([35.2, 38.3382, -0.78]) {
                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                        }
                                                                                      }
                                                                                      translate([49, 35.7591, 0.6]) {
                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                      }
                                                                                    }
                                                                                    translate([49, 39.4045, 0.6]) {
                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                    }
                                                                                  }
                                                                                }
                                                                                hull() {
                                                                                  hull() {
                                                                                    hull() {
                                                                                      translate([35.2, 39.6382, -0.78]) {
                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                      }
                                                                                      translate([35.2, 44.35, -0.78]) {
                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                      }
                                                                                    }
                                                                                    translate([49, 40.7045, 0.6]) {
                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                    }
                                                                                  }
                                                                                  translate([49, 44.35, 0.6]) {
                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                  }
                                                                                }
                                                                              }
                                                                              hull() {
                                                                                hull() {
                                                                                  hull() {
                                                                                    translate([50.2, -2.8773, 0.72]) {
                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                    }
                                                                                    translate([50.2, 0.6755, 0.72]) {
                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                    }
                                                                                  }
                                                                                  translate([64, 7.7864, 2.1]) {
                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                  }
                                                                                }
                                                                                translate([64, 10.2727, 2.1]) {
                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                }
                                                                              }
                                                                            }
                                                                            hull() {
                                                                              hull() {
                                                                                hull() {
                                                                                  translate([50.2, 1.9755, 0.72]) {
                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                  }
                                                                                  translate([50.2, 5.5282, 0.72]) {
                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                  }
                                                                                }
                                                                                translate([64, 11.5727, 2.1]) {
                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                }
                                                                              }
                                                                              translate([64, 14.0591, 2.1]) {
                                                                                zcyl(r = 0.12, l = 1.8);
                                                                              }
                                                                            }
                                                                          }
                                                                          hull() {
                                                                            hull() {
                                                                              hull() {
                                                                                translate([50.2, 6.8282, 0.72]) {
                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                }
                                                                                translate([50.2, 10.3809, 0.72]) {
                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                }
                                                                              }
                                                                              translate([64, 15.3591, 2.1]) {
                                                                                zcyl(r = 0.12, l = 1.8);
                                                                              }
                                                                            }
                                                                            translate([64, 17.8455, 2.1]) {
                                                                              zcyl(r = 0.12, l = 1.8);
                                                                            }
                                                                          }
                                                                        }
                                                                        hull() {
                                                                          hull() {
                                                                            hull() {
                                                                              translate([50.2, 11.6809, 0.72]) {
                                                                                zcyl(r = 0.12, l = 1.8);
                                                                              }
                                                                              translate([50.2, 15.2336, 0.72]) {
                                                                                zcyl(r = 0.12, l = 1.8);
                                                                              }
                                                                            }
                                                                            translate([64, 19.1455, 2.1]) {
                                                                              zcyl(r = 0.12, l = 1.8);
                                                                            }
                                                                          }
                                                                          translate([64, 21.6318, 2.1]) {
                                                                            zcyl(r = 0.12, l = 1.8);
                                                                          }
                                                                        }
                                                                      }
                                                                      hull() {
                                                                        hull() {
                                                                          hull() {
                                                                            translate([50.2, 16.5336, 0.72]) {
                                                                              zcyl(r = 0.12, l = 1.8);
                                                                            }
                                                                            translate([50.2, 20.0864, 0.72]) {
                                                                              zcyl(r = 0.12, l = 1.8);
                                                                            }
                                                                          }
                                                                          translate([64, 22.9318, 2.1]) {
                                                                            zcyl(r = 0.12, l = 1.8);
                                                                          }
                                                                        }
                                                                        translate([64, 25.4182, 2.1]) {
                                                                          zcyl(r = 0.12, l = 1.8);
                                                                        }
                                                                      }
                                                                    }
                                                                    hull() {
                                                                      hull() {
                                                                        hull() {
                                                                          translate([50.2, 21.3864, 0.72]) {
                                                                            zcyl(r = 0.12, l = 1.8);
                                                                          }
                                                                          translate([50.2, 24.9391, 0.72]) {
                                                                            zcyl(r = 0.12, l = 1.8);
                                                                          }
                                                                        }
                                                                        translate([64, 26.7182, 2.1]) {
                                                                          zcyl(r = 0.12, l = 1.8);
                                                                        }
                                                                      }
                                                                      translate([64, 29.2045, 2.1]) {
                                                                        zcyl(r = 0.12, l = 1.8);
                                                                      }
                                                                    }
                                                                  }
                                                                  hull() {
                                                                    hull() {
                                                                      hull() {
                                                                        translate([50.2, 26.2391, 0.72]) {
                                                                          zcyl(r = 0.12, l = 1.8);
                                                                        }
                                                                        translate([50.2, 29.7918, 0.72]) {
                                                                          zcyl(r = 0.12, l = 1.8);
                                                                        }
                                                                      }
                                                                      translate([64, 30.5045, 2.1]) {
                                                                        zcyl(r = 0.12, l = 1.8);
                                                                      }
                                                                    }
                                                                    translate([64, 32.9909, 2.1]) {
                                                                      zcyl(r = 0.12, l = 1.8);
                                                                    }
                                                                  }
                                                                }
                                                                hull() {
                                                                  hull() {
                                                                    hull() {
                                                                      translate([50.2, 31.0918, 0.72]) {
                                                                        zcyl(r = 0.12, l = 1.8);
                                                                      }
                                                                      translate([50.2, 34.6445, 0.72]) {
                                                                        zcyl(r = 0.12, l = 1.8);
                                                                      }
                                                                    }
                                                                    translate([64, 34.2909, 2.1]) {
                                                                      zcyl(r = 0.12, l = 1.8);
                                                                    }
                                                                  }
                                                                  translate([64, 36.7773, 2.1]) {
                                                                    zcyl(r = 0.12, l = 1.8);
                                                                  }
                                                                }
                                                              }
                                                              hull() {
                                                                hull() {
                                                                  hull() {
                                                                    translate([50.2, 35.9445, 0.72]) {
                                                                      zcyl(r = 0.12, l = 1.8);
                                                                    }
                                                                    translate([50.2, 39.4973, 0.72]) {
                                                                      zcyl(r = 0.12, l = 1.8);
                                                                    }
                                                                  }
                                                                  translate([64, 38.0773, 2.1]) {
                                                                    zcyl(r = 0.12, l = 1.8);
                                                                  }
                                                                }
                                                                translate([64, 40.5636, 2.1]) {
                                                                  zcyl(r = 0.12, l = 1.8);
                                                                }
                                                              }
                                                            }
                                                            hull() {
                                                              hull() {
                                                                hull() {
                                                                  translate([50.2, 40.7973, 0.72]) {
                                                                    zcyl(r = 0.12, l = 1.8);
                                                                  }
                                                                  translate([50.2, 44.35, 0.72]) {
                                                                    zcyl(r = 0.12, l = 1.8);
                                                                  }
                                                                }
                                                                translate([64, 41.8636, 2.1]) {
                                                                  zcyl(r = 0.12, l = 1.8);
                                                                }
                                                              }
                                                              translate([64, 44.35, 2.1]) {
                                                                zcyl(r = 0.12, l = 1.8);
                                                              }
                                                            }
                                                          }
                                                          hull() {
                                                            hull() {
                                                              hull() {
                                                                translate([65.2, 8.7136, 2.22]) {
                                                                  zcyl(r = 0.12, l = 1.8);
                                                                }
                                                                translate([65.2, 11.1073, 2.22]) {
                                                                  zcyl(r = 0.12, l = 1.8);
                                                                }
                                                              }
                                                              translate([79, 19.3773, 3.6]) {
                                                                zcyl(r = 0.12, l = 1.8);
                                                              }
                                                            }
                                                            translate([79, 20.7045, 3.6]) {
                                                              zcyl(r = 0.12, l = 1.8);
                                                            }
                                                          }
                                                        }
                                                        hull() {
                                                          hull() {
                                                            hull() {
                                                              translate([65.2, 12.4073, 2.22]) {
                                                                zcyl(r = 0.12, l = 1.8);
                                                              }
                                                              translate([65.2, 14.8009, 2.22]) {
                                                                zcyl(r = 0.12, l = 1.8);
                                                              }
                                                            }
                                                            translate([79, 22.0045, 3.6]) {
                                                              zcyl(r = 0.12, l = 1.8);
                                                            }
                                                          }
                                                          translate([79, 23.3318, 3.6]) {
                                                            zcyl(r = 0.12, l = 1.8);
                                                          }
                                                        }
                                                      }
                                                      hull() {
                                                        hull() {
                                                          hull() {
                                                            translate([65.2, 16.1009, 2.22]) {
                                                              zcyl(r = 0.12, l = 1.8);
                                                            }
                                                            translate([65.2, 18.4945, 2.22]) {
                                                              zcyl(r = 0.12, l = 1.8);
                                                            }
                                                          }
                                                          translate([79, 24.6318, 3.6]) {
                                                            zcyl(r = 0.12, l = 1.8);
                                                          }
                                                        }
                                                        translate([79, 25.9591, 3.6]) {
                                                          zcyl(r = 0.12, l = 1.8);
                                                        }
                                                      }
                                                    }
                                                    hull() {
                                                      hull() {
                                                        hull() {
                                                          translate([65.2, 19.7945, 2.22]) {
                                                            zcyl(r = 0.12, l = 1.8);
                                                          }
                                                          translate([65.2, 22.1882, 2.22]) {
                                                            zcyl(r = 0.12, l = 1.8);
                                                          }
                                                        }
                                                        translate([79, 27.2591, 3.6]) {
                                                          zcyl(r = 0.12, l = 1.8);
                                                        }
                                                      }
                                                      translate([79, 28.5864, 3.6]) {
                                                        zcyl(r = 0.12, l = 1.8);
                                                      }
                                                    }
                                                  }
                                                  hull() {
                                                    hull() {
                                                      hull() {
                                                        translate([65.2, 23.4882, 2.22]) {
                                                          zcyl(r = 0.12, l = 1.8);
                                                        }
                                                        translate([65.2, 25.8818, 2.22]) {
                                                          zcyl(r = 0.12, l = 1.8);
                                                        }
                                                      }
                                                      translate([79, 29.8864, 3.6]) {
                                                        zcyl(r = 0.12, l = 1.8);
                                                      }
                                                    }
                                                    translate([79, 31.2136, 3.6]) {
                                                      zcyl(r = 0.12, l = 1.8);
                                                    }
                                                  }
                                                }
                                                hull() {
                                                  hull() {
                                                    hull() {
                                                      translate([65.2, 27.1818, 2.22]) {
                                                        zcyl(r = 0.12, l = 1.8);
                                                      }
                                                      translate([65.2, 29.5755, 2.22]) {
                                                        zcyl(r = 0.12, l = 1.8);
                                                      }
                                                    }
                                                    translate([79, 32.5136, 3.6]) {
                                                      zcyl(r = 0.12, l = 1.8);
                                                    }
                                                  }
                                                  translate([79, 33.8409, 3.6]) {
                                                    zcyl(r = 0.12, l = 1.8);
                                                  }
                                                }
                                              }
                                              hull() {
                                                hull() {
                                                  hull() {
                                                    translate([65.2, 30.8755, 2.22]) {
                                                      zcyl(r = 0.12, l = 1.8);
                                                    }
                                                    translate([65.2, 33.2691, 2.22]) {
                                                      zcyl(r = 0.12, l = 1.8);
                                                    }
                                                  }
                                                  translate([79, 35.1409, 3.6]) {
                                                    zcyl(r = 0.12, l = 1.8);
                                                  }
                                                }
                                                translate([79, 36.4682, 3.6]) {
                                                  zcyl(r = 0.12, l = 1.8);
                                                }
                                              }
                                            }
                                            hull() {
                                              hull() {
                                                hull() {
                                                  translate([65.2, 34.5691, 2.22]) {
                                                    zcyl(r = 0.12, l = 1.8);
                                                  }
                                                  translate([65.2, 36.9627, 2.22]) {
                                                    zcyl(r = 0.12, l = 1.8);
                                                  }
                                                }
                                                translate([79, 37.7682, 3.6]) {
                                                  zcyl(r = 0.12, l = 1.8);
                                                }
                                              }
                                              translate([79, 39.0955, 3.6]) {
                                                zcyl(r = 0.12, l = 1.8);
                                              }
                                            }
                                          }
                                          hull() {
                                            hull() {
                                              hull() {
                                                translate([65.2, 38.2627, 2.22]) {
                                                  zcyl(r = 0.12, l = 1.8);
                                                }
                                                translate([65.2, 40.6564, 2.22]) {
                                                  zcyl(r = 0.12, l = 1.8);
                                                }
                                              }
                                              translate([79, 40.3955, 3.6]) {
                                                zcyl(r = 0.12, l = 1.8);
                                              }
                                            }
                                            translate([79, 41.7227, 3.6]) {
                                              zcyl(r = 0.12, l = 1.8);
                                            }
                                          }
                                        }
                                        hull() {
                                          hull() {
                                            hull() {
                                              translate([65.2, 41.9564, 2.22]) {
                                                zcyl(r = 0.12, l = 1.8);
                                              }
                                              translate([65.2, 44.35, 2.22]) {
                                                zcyl(r = 0.12, l = 1.8);
                                              }
                                            }
                                            translate([79, 43.0227, 3.6]) {
                                              zcyl(r = 0.12, l = 1.8);
                                            }
                                          }
                                          translate([79, 44.35, 3.6]) {
                                            zcyl(r = 0.12, l = 1.8);
                                          }
                                        }
                                      }
                                      hull() {
                                        hull() {
                                          hull() {
                                            translate([80.2, 20.3045, 3.72]) {
                                              zcyl(r = 0.12, l = 1.8);
                                            }
                                            translate([80.2, 21.5391, 3.72]) {
                                              zcyl(r = 0.12, l = 1.8);
                                            }
                                          }
                                          translate([92, 29.4227, 4.9]) {
                                            zcyl(r = 0.12, l = 1.8);
                                          }
                                        }
                                        translate([92, 29.7455, 4.9]) {
                                          zcyl(r = 0.12, l = 1.8);
                                        }
                                      }
                                    }
                                    hull() {
                                      hull() {
                                        hull() {
                                          translate([80.2, 22.8391, 3.72]) {
                                            zcyl(r = 0.12, l = 1.8);
                                          }
                                          translate([80.2, 24.0736, 3.72]) {
                                            zcyl(r = 0.12, l = 1.8);
                                          }
                                        }
                                        translate([92, 31.0455, 4.9]) {
                                          zcyl(r = 0.12, l = 1.8);
                                        }
                                      }
                                      translate([92, 31.3682, 4.9]) {
                                        zcyl(r = 0.12, l = 1.8);
                                      }
                                    }
                                  }
                                  hull() {
                                    hull() {
                                      hull() {
                                        translate([80.2, 25.3736, 3.72]) {
                                          zcyl(r = 0.12, l = 1.8);
                                        }
                                        translate([80.2, 26.6082, 3.72]) {
                                          zcyl(r = 0.12, l = 1.8);
                                        }
                                      }
                                      translate([92, 32.6682, 4.9]) {
                                        zcyl(r = 0.12, l = 1.8);
                                      }
                                    }
                                    translate([92, 32.9909, 4.9]) {
                                      zcyl(r = 0.12, l = 1.8);
                                    }
                                  }
                                }
                                hull() {
                                  hull() {
                                    hull() {
                                      translate([80.2, 27.9082, 3.72]) {
                                        zcyl(r = 0.12, l = 1.8);
                                      }
                                      translate([80.2, 29.1427, 3.72]) {
                                        zcyl(r = 0.12, l = 1.8);
                                      }
                                    }
                                    translate([92, 34.2909, 4.9]) {
                                      zcyl(r = 0.12, l = 1.8);
                                    }
                                  }
                                  translate([92, 34.6136, 4.9]) {
                                    zcyl(r = 0.12, l = 1.8);
                                  }
                                }
                              }
                              hull() {
                                hull() {
                                  hull() {
                                    translate([80.2, 30.4427, 3.72]) {
                                      zcyl(r = 0.12, l = 1.8);
                                    }
                                    translate([80.2, 31.6773, 3.72]) {
                                      zcyl(r = 0.12, l = 1.8);
                                    }
                                  }
                                  translate([92, 35.9136, 4.9]) {
                                    zcyl(r = 0.12, l = 1.8);
                                  }
                                }
                                translate([92, 36.2364, 4.9]) {
                                  zcyl(r = 0.12, l = 1.8);
                                }
                              }
                            }
                            hull() {
                              hull() {
                                hull() {
                                  translate([80.2, 32.9773, 3.72]) {
                                    zcyl(r = 0.12, l = 1.8);
                                  }
                                  translate([80.2, 34.2118, 3.72]) {
                                    zcyl(r = 0.12, l = 1.8);
                                  }
                                }
                                translate([92, 37.5364, 4.9]) {
                                  zcyl(r = 0.12, l = 1.8);
                                }
                              }
                              translate([92, 37.8591, 4.9]) {
                                zcyl(r = 0.12, l = 1.8);
                              }
                            }
                          }
                          hull() {
                            hull() {
                              hull() {
                                translate([80.2, 35.5118, 3.72]) {
                                  zcyl(r = 0.12, l = 1.8);
                                }
                                translate([80.2, 36.7464, 3.72]) {
                                  zcyl(r = 0.12, l = 1.8);
                                }
                              }
                              translate([92, 39.1591, 4.9]) {
                                zcyl(r = 0.12, l = 1.8);
                              }
                            }
                            translate([92, 39.4818, 4.9]) {
                              zcyl(r = 0.12, l = 1.8);
                            }
                          }
                        }
                        hull() {
                          hull() {
                            hull() {
                              translate([80.2, 38.0464, 3.72]) {
                                zcyl(r = 0.12, l = 1.8);
                              }
                              translate([80.2, 39.2809, 3.72]) {
                                zcyl(r = 0.12, l = 1.8);
                              }
                            }
                            translate([92, 40.7818, 4.9]) {
                              zcyl(r = 0.12, l = 1.8);
                            }
                          }
                          translate([92, 41.1045, 4.9]) {
                            zcyl(r = 0.12, l = 1.8);
                          }
                        }
                      }
                      hull() {
                        hull() {
                          hull() {
                            translate([80.2, 40.5809, 3.72]) {
                              zcyl(r = 0.12, l = 1.8);
                            }
                            translate([80.2, 41.8155, 3.72]) {
                              zcyl(r = 0.12, l = 1.8);
                            }
                          }
                          translate([92, 42.4045, 4.9]) {
                            zcyl(r = 0.12, l = 1.8);
                          }
                        }
                        translate([92, 42.7273, 4.9]) {
                          zcyl(r = 0.12, l = 1.8);
                        }
                      }
                    }
                    hull() {
                      hull() {
                        hull() {
                          translate([80.2, 43.1155, 3.72]) {
                            zcyl(r = 0.12, l = 1.8);
                          }
                          translate([80.2, 44.35, 3.72]) {
                            zcyl(r = 0.12, l = 1.8);
                          }
                        }
                        translate([92, 44.0273, 4.9]) {
                          zcyl(r = 0.12, l = 1.8);
                        }
                      }
                      translate([92, 44.35, 4.9]) {
                        zcyl(r = 0.12, l = 1.8);
                      }
                    }
                  }
                }
                mirror([1, 0, 0]) {
                  difference() {
                    union() {
                      hull() {
                        hull() {
                          translate([3, 0, 0]) {
                            translate([0, -40, 0]) {
                              translate([0, 0, -4]) {
                                zcyl(r = 1.5, l = 3);
                              }
                            }
                          }
                          translate([3, 0, 0]) {
                            translate([0, 45, 0]) {
                              translate([0, 0, -4]) {
                                zcyl(r = 1.5, l = 3);
                              }
                            }
                          }
                        }
                        translate([113, 0, 0]) {
                          translate([0, 45, 0]) {
                            translate([0, 0, 7]) {
                              zcyl(r = 1.5, l = 3);
                            }
                          }
                        }
                      }
                      hull() {
                        hull() {
                          hull() {
                            hull() {
                              hull() {
                                translate([0, -40, -4]) {
                                  zcyl(r = 1.5, l = 6);
                                }
                                translate([0, 45, -4]) {
                                  zcyl(r = 1.5, l = 6);
                                }
                              }
                              translate([3, -40, -4]) {
                                zcyl(r = 1.5, l = 6);
                              }
                            }
                            translate([3, 45, -4]) {
                              zcyl(r = 1.5, l = 6);
                            }
                          }
                          translate([20, -26.8636, -2.3]) {
                            zcyl(r = 1.5, l = 3);
                          }
                        }
                        translate([20, 45, -2.3]) {
                          zcyl(r = 1.5, l = 3);
                        }
                      }
                    }
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
                                                                                                                                  union() {
                                                                                                                                    union() {
                                                                                                                                      union() {
                                                                                                                                        union() {
                                                                                                                                          hull() {
                                                                                                                                            hull() {
                                                                                                                                              hull() {
                                                                                                                                                translate([7, -36.2591, -3.6]) {
                                                                                                                                                  zcyl(r = 0.12, l = 4.0941);
                                                                                                                                                }
                                                                                                                                                translate([7, -29.3682, -3.6]) {
                                                                                                                                                  zcyl(r = 0.12, l = 4.0941);
                                                                                                                                                }
                                                                                                                                              }
                                                                                                                                              translate([19.4, -26.6773, -2.36]) {
                                                                                                                                                zcyl(r = 0.12, l = 1.9059);
                                                                                                                                              }
                                                                                                                                            }
                                                                                                                                            translate([19.4, -20.7445, -2.36]) {
                                                                                                                                              zcyl(r = 0.12, l = 1.9059);
                                                                                                                                            }
                                                                                                                                          }
                                                                                                                                          hull() {
                                                                                                                                            hull() {
                                                                                                                                              hull() {
                                                                                                                                                translate([7, -28.0682, -3.6]) {
                                                                                                                                                  zcyl(r = 0.12, l = 4.0941);
                                                                                                                                                }
                                                                                                                                                translate([7, -21.1773, -3.6]) {
                                                                                                                                                  zcyl(r = 0.12, l = 4.0941);
                                                                                                                                                }
                                                                                                                                              }
                                                                                                                                              translate([19.4, -19.4445, -2.36]) {
                                                                                                                                                zcyl(r = 0.12, l = 1.9059);
                                                                                                                                              }
                                                                                                                                            }
                                                                                                                                            translate([19.4, -13.5118, -2.36]) {
                                                                                                                                              zcyl(r = 0.12, l = 1.9059);
                                                                                                                                            }
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                        hull() {
                                                                                                                                          hull() {
                                                                                                                                            hull() {
                                                                                                                                              translate([7, -19.8773, -3.6]) {
                                                                                                                                                zcyl(r = 0.12, l = 4.0941);
                                                                                                                                              }
                                                                                                                                              translate([7, -12.9864, -3.6]) {
                                                                                                                                                zcyl(r = 0.12, l = 4.0941);
                                                                                                                                              }
                                                                                                                                            }
                                                                                                                                            translate([19.4, -12.2118, -2.36]) {
                                                                                                                                              zcyl(r = 0.12, l = 1.9059);
                                                                                                                                            }
                                                                                                                                          }
                                                                                                                                          translate([19.4, -6.2791, -2.36]) {
                                                                                                                                            zcyl(r = 0.12, l = 1.9059);
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                      hull() {
                                                                                                                                        hull() {
                                                                                                                                          hull() {
                                                                                                                                            translate([7, -11.6864, -3.6]) {
                                                                                                                                              zcyl(r = 0.12, l = 4.0941);
                                                                                                                                            }
                                                                                                                                            translate([7, -4.7955, -3.6]) {
                                                                                                                                              zcyl(r = 0.12, l = 4.0941);
                                                                                                                                            }
                                                                                                                                          }
                                                                                                                                          translate([19.4, -4.9791, -2.36]) {
                                                                                                                                            zcyl(r = 0.12, l = 1.9059);
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                        translate([19.4, 0.9536, -2.36]) {
                                                                                                                                          zcyl(r = 0.12, l = 1.9059);
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                    hull() {
                                                                                                                                      hull() {
                                                                                                                                        hull() {
                                                                                                                                          translate([7, -3.4955, -3.6]) {
                                                                                                                                            zcyl(r = 0.12, l = 4.0941);
                                                                                                                                          }
                                                                                                                                          translate([7, 3.3955, -3.6]) {
                                                                                                                                            zcyl(r = 0.12, l = 4.0941);
                                                                                                                                          }
                                                                                                                                        }
                                                                                                                                        translate([19.4, 2.2536, -2.36]) {
                                                                                                                                          zcyl(r = 0.12, l = 1.9059);
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                      translate([19.4, 8.1864, -2.36]) {
                                                                                                                                        zcyl(r = 0.12, l = 1.9059);
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                  hull() {
                                                                                                                                    hull() {
                                                                                                                                      hull() {
                                                                                                                                        translate([7, 4.6955, -3.6]) {
                                                                                                                                          zcyl(r = 0.12, l = 4.0941);
                                                                                                                                        }
                                                                                                                                        translate([7, 11.5864, -3.6]) {
                                                                                                                                          zcyl(r = 0.12, l = 4.0941);
                                                                                                                                        }
                                                                                                                                      }
                                                                                                                                      translate([19.4, 9.4864, -2.36]) {
                                                                                                                                        zcyl(r = 0.12, l = 1.9059);
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                    translate([19.4, 15.4191, -2.36]) {
                                                                                                                                      zcyl(r = 0.12, l = 1.9059);
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                }
                                                                                                                                hull() {
                                                                                                                                  hull() {
                                                                                                                                    hull() {
                                                                                                                                      translate([7, 12.8864, -3.6]) {
                                                                                                                                        zcyl(r = 0.12, l = 4.0941);
                                                                                                                                      }
                                                                                                                                      translate([7, 19.7773, -3.6]) {
                                                                                                                                        zcyl(r = 0.12, l = 4.0941);
                                                                                                                                      }
                                                                                                                                    }
                                                                                                                                    translate([19.4, 16.7191, -2.36]) {
                                                                                                                                      zcyl(r = 0.12, l = 1.9059);
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                  translate([19.4, 22.6518, -2.36]) {
                                                                                                                                    zcyl(r = 0.12, l = 1.9059);
                                                                                                                                  }
                                                                                                                                }
                                                                                                                              }
                                                                                                                              hull() {
                                                                                                                                hull() {
                                                                                                                                  hull() {
                                                                                                                                    translate([7, 21.0773, -3.6]) {
                                                                                                                                      zcyl(r = 0.12, l = 4.0941);
                                                                                                                                    }
                                                                                                                                    translate([7, 27.9682, -3.6]) {
                                                                                                                                      zcyl(r = 0.12, l = 4.0941);
                                                                                                                                    }
                                                                                                                                  }
                                                                                                                                  translate([19.4, 23.9518, -2.36]) {
                                                                                                                                    zcyl(r = 0.12, l = 1.9059);
                                                                                                                                  }
                                                                                                                                }
                                                                                                                                translate([19.4, 29.8845, -2.36]) {
                                                                                                                                  zcyl(r = 0.12, l = 1.9059);
                                                                                                                                }
                                                                                                                              }
                                                                                                                            }
                                                                                                                            hull() {
                                                                                                                              hull() {
                                                                                                                                hull() {
                                                                                                                                  translate([7, 29.2682, -3.6]) {
                                                                                                                                    zcyl(r = 0.12, l = 4.0941);
                                                                                                                                  }
                                                                                                                                  translate([7, 36.1591, -3.6]) {
                                                                                                                                    zcyl(r = 0.12, l = 4.0941);
                                                                                                                                  }
                                                                                                                                }
                                                                                                                                translate([19.4, 31.1845, -2.36]) {
                                                                                                                                  zcyl(r = 0.12, l = 1.9059);
                                                                                                                                }
                                                                                                                              }
                                                                                                                              translate([19.4, 37.1173, -2.36]) {
                                                                                                                                zcyl(r = 0.12, l = 1.9059);
                                                                                                                              }
                                                                                                                            }
                                                                                                                          }
                                                                                                                          hull() {
                                                                                                                            hull() {
                                                                                                                              hull() {
                                                                                                                                translate([7, 37.4591, -3.6]) {
                                                                                                                                  zcyl(r = 0.12, l = 4.0941);
                                                                                                                                }
                                                                                                                                translate([7, 44.35, -3.6]) {
                                                                                                                                  zcyl(r = 0.12, l = 4.0941);
                                                                                                                                }
                                                                                                                              }
                                                                                                                              translate([19.4, 38.4173, -2.36]) {
                                                                                                                                zcyl(r = 0.12, l = 1.9059);
                                                                                                                              }
                                                                                                                            }
                                                                                                                            translate([19.4, 44.35, -2.36]) {
                                                                                                                              zcyl(r = 0.12, l = 1.9059);
                                                                                                                            }
                                                                                                                          }
                                                                                                                        }
                                                                                                                        hull() {
                                                                                                                          hull() {
                                                                                                                            hull() {
                                                                                                                              translate([20.6, -25.75, -2.24]) {
                                                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                                                              }
                                                                                                                              translate([20.6, -19.91, -2.24]) {
                                                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                                                              }
                                                                                                                            }
                                                                                                                            translate([34, -15.3955, -0.9]) {
                                                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                                                            }
                                                                                                                          }
                                                                                                                          translate([34, -10.5909, -0.9]) {
                                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                                          }
                                                                                                                        }
                                                                                                                      }
                                                                                                                      hull() {
                                                                                                                        hull() {
                                                                                                                          hull() {
                                                                                                                            translate([20.6, -18.61, -2.24]) {
                                                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                                                            }
                                                                                                                            translate([20.6, -12.77, -2.24]) {
                                                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                                                            }
                                                                                                                          }
                                                                                                                          translate([34, -9.2909, -0.9]) {
                                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                                          }
                                                                                                                        }
                                                                                                                        translate([34, -4.4864, -0.9]) {
                                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                                        }
                                                                                                                      }
                                                                                                                    }
                                                                                                                    hull() {
                                                                                                                      hull() {
                                                                                                                        hull() {
                                                                                                                          translate([20.6, -11.47, -2.24]) {
                                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                                          }
                                                                                                                          translate([20.6, -5.63, -2.24]) {
                                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                                          }
                                                                                                                        }
                                                                                                                        translate([34, -3.1864, -0.9]) {
                                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                                        }
                                                                                                                      }
                                                                                                                      translate([34, 1.6182, -0.9]) {
                                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                                      }
                                                                                                                    }
                                                                                                                  }
                                                                                                                  hull() {
                                                                                                                    hull() {
                                                                                                                      hull() {
                                                                                                                        translate([20.6, -4.33, -2.24]) {
                                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                                        }
                                                                                                                        translate([20.6, 1.51, -2.24]) {
                                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                                        }
                                                                                                                      }
                                                                                                                      translate([34, 2.9182, -0.9]) {
                                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                                      }
                                                                                                                    }
                                                                                                                    translate([34, 7.7227, -0.9]) {
                                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                                    }
                                                                                                                  }
                                                                                                                }
                                                                                                                hull() {
                                                                                                                  hull() {
                                                                                                                    hull() {
                                                                                                                      translate([20.6, 2.81, -2.24]) {
                                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                                      }
                                                                                                                      translate([20.6, 8.65, -2.24]) {
                                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                                      }
                                                                                                                    }
                                                                                                                    translate([34, 9.0227, -0.9]) {
                                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                                    }
                                                                                                                  }
                                                                                                                  translate([34, 13.8273, -0.9]) {
                                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                                  }
                                                                                                                }
                                                                                                              }
                                                                                                              hull() {
                                                                                                                hull() {
                                                                                                                  hull() {
                                                                                                                    translate([20.6, 9.95, -2.24]) {
                                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                                    }
                                                                                                                    translate([20.6, 15.79, -2.24]) {
                                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                                    }
                                                                                                                  }
                                                                                                                  translate([34, 15.1273, -0.9]) {
                                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                                  }
                                                                                                                }
                                                                                                                translate([34, 19.9318, -0.9]) {
                                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                                }
                                                                                                              }
                                                                                                            }
                                                                                                            hull() {
                                                                                                              hull() {
                                                                                                                hull() {
                                                                                                                  translate([20.6, 17.09, -2.24]) {
                                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                                  }
                                                                                                                  translate([20.6, 22.93, -2.24]) {
                                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                                  }
                                                                                                                }
                                                                                                                translate([34, 21.2318, -0.9]) {
                                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                                }
                                                                                                              }
                                                                                                              translate([34, 26.0364, -0.9]) {
                                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                                              }
                                                                                                            }
                                                                                                          }
                                                                                                          hull() {
                                                                                                            hull() {
                                                                                                              hull() {
                                                                                                                translate([20.6, 24.23, -2.24]) {
                                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                                }
                                                                                                                translate([20.6, 30.07, -2.24]) {
                                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                                }
                                                                                                              }
                                                                                                              translate([34, 27.3364, -0.9]) {
                                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                                              }
                                                                                                            }
                                                                                                            translate([34, 32.1409, -0.9]) {
                                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                                            }
                                                                                                          }
                                                                                                        }
                                                                                                        hull() {
                                                                                                          hull() {
                                                                                                            hull() {
                                                                                                              translate([20.6, 31.37, -2.24]) {
                                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                                              }
                                                                                                              translate([20.6, 37.21, -2.24]) {
                                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                                              }
                                                                                                            }
                                                                                                            translate([34, 33.4409, -0.9]) {
                                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                                            }
                                                                                                          }
                                                                                                          translate([34, 38.2455, -0.9]) {
                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                          }
                                                                                                        }
                                                                                                      }
                                                                                                      hull() {
                                                                                                        hull() {
                                                                                                          hull() {
                                                                                                            translate([20.6, 38.51, -2.24]) {
                                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                                            }
                                                                                                            translate([20.6, 44.35, -2.24]) {
                                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                                            }
                                                                                                          }
                                                                                                          translate([34, 39.5455, -0.9]) {
                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                          }
                                                                                                        }
                                                                                                        translate([34, 44.35, -0.9]) {
                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                        }
                                                                                                      }
                                                                                                    }
                                                                                                    hull() {
                                                                                                      hull() {
                                                                                                        hull() {
                                                                                                          translate([35.2, -14.4682, -0.78]) {
                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                          }
                                                                                                          translate([35.2, -9.7564, -0.78]) {
                                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                                          }
                                                                                                        }
                                                                                                        translate([49, -3.8045, 0.6]) {
                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                        }
                                                                                                      }
                                                                                                      translate([49, -0.1591, 0.6]) {
                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                      }
                                                                                                    }
                                                                                                  }
                                                                                                  hull() {
                                                                                                    hull() {
                                                                                                      hull() {
                                                                                                        translate([35.2, -8.4564, -0.78]) {
                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                        }
                                                                                                        translate([35.2, -3.7445, -0.78]) {
                                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                                        }
                                                                                                      }
                                                                                                      translate([49, 1.1409, 0.6]) {
                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                      }
                                                                                                    }
                                                                                                    translate([49, 4.7864, 0.6]) {
                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                    }
                                                                                                  }
                                                                                                }
                                                                                                hull() {
                                                                                                  hull() {
                                                                                                    hull() {
                                                                                                      translate([35.2, -2.4445, -0.78]) {
                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                      }
                                                                                                      translate([35.2, 2.2673, -0.78]) {
                                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                                      }
                                                                                                    }
                                                                                                    translate([49, 6.0864, 0.6]) {
                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                    }
                                                                                                  }
                                                                                                  translate([49, 9.7318, 0.6]) {
                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                  }
                                                                                                }
                                                                                              }
                                                                                              hull() {
                                                                                                hull() {
                                                                                                  hull() {
                                                                                                    translate([35.2, 3.5673, -0.78]) {
                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                    }
                                                                                                    translate([35.2, 8.2791, -0.78]) {
                                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                                    }
                                                                                                  }
                                                                                                  translate([49, 11.0318, 0.6]) {
                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                  }
                                                                                                }
                                                                                                translate([49, 14.6773, 0.6]) {
                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                            hull() {
                                                                                              hull() {
                                                                                                hull() {
                                                                                                  translate([35.2, 9.5791, -0.78]) {
                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                  }
                                                                                                  translate([35.2, 14.2909, -0.78]) {
                                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                                  }
                                                                                                }
                                                                                                translate([49, 15.9773, 0.6]) {
                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                }
                                                                                              }
                                                                                              translate([49, 19.6227, 0.6]) {
                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                              }
                                                                                            }
                                                                                          }
                                                                                          hull() {
                                                                                            hull() {
                                                                                              hull() {
                                                                                                translate([35.2, 15.5909, -0.78]) {
                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                }
                                                                                                translate([35.2, 20.3027, -0.78]) {
                                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                                }
                                                                                              }
                                                                                              translate([49, 20.9227, 0.6]) {
                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                              }
                                                                                            }
                                                                                            translate([49, 24.5682, 0.6]) {
                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                            }
                                                                                          }
                                                                                        }
                                                                                        hull() {
                                                                                          hull() {
                                                                                            hull() {
                                                                                              translate([35.2, 21.6027, -0.78]) {
                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                              }
                                                                                              translate([35.2, 26.3145, -0.78]) {
                                                                                                zcyl(r = 0.12, l = 1.8);
                                                                                              }
                                                                                            }
                                                                                            translate([49, 25.8682, 0.6]) {
                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                            }
                                                                                          }
                                                                                          translate([49, 29.5136, 0.6]) {
                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                      hull() {
                                                                                        hull() {
                                                                                          hull() {
                                                                                            translate([35.2, 27.6145, -0.78]) {
                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                            }
                                                                                            translate([35.2, 32.3264, -0.78]) {
                                                                                              zcyl(r = 0.12, l = 1.8);
                                                                                            }
                                                                                          }
                                                                                          translate([49, 30.8136, 0.6]) {
                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                          }
                                                                                        }
                                                                                        translate([49, 34.4591, 0.6]) {
                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                    hull() {
                                                                                      hull() {
                                                                                        hull() {
                                                                                          translate([35.2, 33.6264, -0.78]) {
                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                          }
                                                                                          translate([35.2, 38.3382, -0.78]) {
                                                                                            zcyl(r = 0.12, l = 1.8);
                                                                                          }
                                                                                        }
                                                                                        translate([49, 35.7591, 0.6]) {
                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                        }
                                                                                      }
                                                                                      translate([49, 39.4045, 0.6]) {
                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                  hull() {
                                                                                    hull() {
                                                                                      hull() {
                                                                                        translate([35.2, 39.6382, -0.78]) {
                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                        }
                                                                                        translate([35.2, 44.35, -0.78]) {
                                                                                          zcyl(r = 0.12, l = 1.8);
                                                                                        }
                                                                                      }
                                                                                      translate([49, 40.7045, 0.6]) {
                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                      }
                                                                                    }
                                                                                    translate([49, 44.35, 0.6]) {
                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                    }
                                                                                  }
                                                                                }
                                                                                hull() {
                                                                                  hull() {
                                                                                    hull() {
                                                                                      translate([50.2, -2.8773, 0.72]) {
                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                      }
                                                                                      translate([50.2, 0.6755, 0.72]) {
                                                                                        zcyl(r = 0.12, l = 1.8);
                                                                                      }
                                                                                    }
                                                                                    translate([64, 7.7864, 2.1]) {
                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                    }
                                                                                  }
                                                                                  translate([64, 10.2727, 2.1]) {
                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                  }
                                                                                }
                                                                              }
                                                                              hull() {
                                                                                hull() {
                                                                                  hull() {
                                                                                    translate([50.2, 1.9755, 0.72]) {
                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                    }
                                                                                    translate([50.2, 5.5282, 0.72]) {
                                                                                      zcyl(r = 0.12, l = 1.8);
                                                                                    }
                                                                                  }
                                                                                  translate([64, 11.5727, 2.1]) {
                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                  }
                                                                                }
                                                                                translate([64, 14.0591, 2.1]) {
                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                }
                                                                              }
                                                                            }
                                                                            hull() {
                                                                              hull() {
                                                                                hull() {
                                                                                  translate([50.2, 6.8282, 0.72]) {
                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                  }
                                                                                  translate([50.2, 10.3809, 0.72]) {
                                                                                    zcyl(r = 0.12, l = 1.8);
                                                                                  }
                                                                                }
                                                                                translate([64, 15.3591, 2.1]) {
                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                }
                                                                              }
                                                                              translate([64, 17.8455, 2.1]) {
                                                                                zcyl(r = 0.12, l = 1.8);
                                                                              }
                                                                            }
                                                                          }
                                                                          hull() {
                                                                            hull() {
                                                                              hull() {
                                                                                translate([50.2, 11.6809, 0.72]) {
                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                }
                                                                                translate([50.2, 15.2336, 0.72]) {
                                                                                  zcyl(r = 0.12, l = 1.8);
                                                                                }
                                                                              }
                                                                              translate([64, 19.1455, 2.1]) {
                                                                                zcyl(r = 0.12, l = 1.8);
                                                                              }
                                                                            }
                                                                            translate([64, 21.6318, 2.1]) {
                                                                              zcyl(r = 0.12, l = 1.8);
                                                                            }
                                                                          }
                                                                        }
                                                                        hull() {
                                                                          hull() {
                                                                            hull() {
                                                                              translate([50.2, 16.5336, 0.72]) {
                                                                                zcyl(r = 0.12, l = 1.8);
                                                                              }
                                                                              translate([50.2, 20.0864, 0.72]) {
                                                                                zcyl(r = 0.12, l = 1.8);
                                                                              }
                                                                            }
                                                                            translate([64, 22.9318, 2.1]) {
                                                                              zcyl(r = 0.12, l = 1.8);
                                                                            }
                                                                          }
                                                                          translate([64, 25.4182, 2.1]) {
                                                                            zcyl(r = 0.12, l = 1.8);
                                                                          }
                                                                        }
                                                                      }
                                                                      hull() {
                                                                        hull() {
                                                                          hull() {
                                                                            translate([50.2, 21.3864, 0.72]) {
                                                                              zcyl(r = 0.12, l = 1.8);
                                                                            }
                                                                            translate([50.2, 24.9391, 0.72]) {
                                                                              zcyl(r = 0.12, l = 1.8);
                                                                            }
                                                                          }
                                                                          translate([64, 26.7182, 2.1]) {
                                                                            zcyl(r = 0.12, l = 1.8);
                                                                          }
                                                                        }
                                                                        translate([64, 29.2045, 2.1]) {
                                                                          zcyl(r = 0.12, l = 1.8);
                                                                        }
                                                                      }
                                                                    }
                                                                    hull() {
                                                                      hull() {
                                                                        hull() {
                                                                          translate([50.2, 26.2391, 0.72]) {
                                                                            zcyl(r = 0.12, l = 1.8);
                                                                          }
                                                                          translate([50.2, 29.7918, 0.72]) {
                                                                            zcyl(r = 0.12, l = 1.8);
                                                                          }
                                                                        }
                                                                        translate([64, 30.5045, 2.1]) {
                                                                          zcyl(r = 0.12, l = 1.8);
                                                                        }
                                                                      }
                                                                      translate([64, 32.9909, 2.1]) {
                                                                        zcyl(r = 0.12, l = 1.8);
                                                                      }
                                                                    }
                                                                  }
                                                                  hull() {
                                                                    hull() {
                                                                      hull() {
                                                                        translate([50.2, 31.0918, 0.72]) {
                                                                          zcyl(r = 0.12, l = 1.8);
                                                                        }
                                                                        translate([50.2, 34.6445, 0.72]) {
                                                                          zcyl(r = 0.12, l = 1.8);
                                                                        }
                                                                      }
                                                                      translate([64, 34.2909, 2.1]) {
                                                                        zcyl(r = 0.12, l = 1.8);
                                                                      }
                                                                    }
                                                                    translate([64, 36.7773, 2.1]) {
                                                                      zcyl(r = 0.12, l = 1.8);
                                                                    }
                                                                  }
                                                                }
                                                                hull() {
                                                                  hull() {
                                                                    hull() {
                                                                      translate([50.2, 35.9445, 0.72]) {
                                                                        zcyl(r = 0.12, l = 1.8);
                                                                      }
                                                                      translate([50.2, 39.4973, 0.72]) {
                                                                        zcyl(r = 0.12, l = 1.8);
                                                                      }
                                                                    }
                                                                    translate([64, 38.0773, 2.1]) {
                                                                      zcyl(r = 0.12, l = 1.8);
                                                                    }
                                                                  }
                                                                  translate([64, 40.5636, 2.1]) {
                                                                    zcyl(r = 0.12, l = 1.8);
                                                                  }
                                                                }
                                                              }
                                                              hull() {
                                                                hull() {
                                                                  hull() {
                                                                    translate([50.2, 40.7973, 0.72]) {
                                                                      zcyl(r = 0.12, l = 1.8);
                                                                    }
                                                                    translate([50.2, 44.35, 0.72]) {
                                                                      zcyl(r = 0.12, l = 1.8);
                                                                    }
                                                                  }
                                                                  translate([64, 41.8636, 2.1]) {
                                                                    zcyl(r = 0.12, l = 1.8);
                                                                  }
                                                                }
                                                                translate([64, 44.35, 2.1]) {
                                                                  zcyl(r = 0.12, l = 1.8);
                                                                }
                                                              }
                                                            }
                                                            hull() {
                                                              hull() {
                                                                hull() {
                                                                  translate([65.2, 8.7136, 2.22]) {
                                                                    zcyl(r = 0.12, l = 1.8);
                                                                  }
                                                                  translate([65.2, 11.1073, 2.22]) {
                                                                    zcyl(r = 0.12, l = 1.8);
                                                                  }
                                                                }
                                                                translate([79, 19.3773, 3.6]) {
                                                                  zcyl(r = 0.12, l = 1.8);
                                                                }
                                                              }
                                                              translate([79, 20.7045, 3.6]) {
                                                                zcyl(r = 0.12, l = 1.8);
                                                              }
                                                            }
                                                          }
                                                          hull() {
                                                            hull() {
                                                              hull() {
                                                                translate([65.2, 12.4073, 2.22]) {
                                                                  zcyl(r = 0.12, l = 1.8);
                                                                }
                                                                translate([65.2, 14.8009, 2.22]) {
                                                                  zcyl(r = 0.12, l = 1.8);
                                                                }
                                                              }
                                                              translate([79, 22.0045, 3.6]) {
                                                                zcyl(r = 0.12, l = 1.8);
                                                              }
                                                            }
                                                            translate([79, 23.3318, 3.6]) {
                                                              zcyl(r = 0.12, l = 1.8);
                                                            }
                                                          }
                                                        }
                                                        hull() {
                                                          hull() {
                                                            hull() {
                                                              translate([65.2, 16.1009, 2.22]) {
                                                                zcyl(r = 0.12, l = 1.8);
                                                              }
                                                              translate([65.2, 18.4945, 2.22]) {
                                                                zcyl(r = 0.12, l = 1.8);
                                                              }
                                                            }
                                                            translate([79, 24.6318, 3.6]) {
                                                              zcyl(r = 0.12, l = 1.8);
                                                            }
                                                          }
                                                          translate([79, 25.9591, 3.6]) {
                                                            zcyl(r = 0.12, l = 1.8);
                                                          }
                                                        }
                                                      }
                                                      hull() {
                                                        hull() {
                                                          hull() {
                                                            translate([65.2, 19.7945, 2.22]) {
                                                              zcyl(r = 0.12, l = 1.8);
                                                            }
                                                            translate([65.2, 22.1882, 2.22]) {
                                                              zcyl(r = 0.12, l = 1.8);
                                                            }
                                                          }
                                                          translate([79, 27.2591, 3.6]) {
                                                            zcyl(r = 0.12, l = 1.8);
                                                          }
                                                        }
                                                        translate([79, 28.5864, 3.6]) {
                                                          zcyl(r = 0.12, l = 1.8);
                                                        }
                                                      }
                                                    }
                                                    hull() {
                                                      hull() {
                                                        hull() {
                                                          translate([65.2, 23.4882, 2.22]) {
                                                            zcyl(r = 0.12, l = 1.8);
                                                          }
                                                          translate([65.2, 25.8818, 2.22]) {
                                                            zcyl(r = 0.12, l = 1.8);
                                                          }
                                                        }
                                                        translate([79, 29.8864, 3.6]) {
                                                          zcyl(r = 0.12, l = 1.8);
                                                        }
                                                      }
                                                      translate([79, 31.2136, 3.6]) {
                                                        zcyl(r = 0.12, l = 1.8);
                                                      }
                                                    }
                                                  }
                                                  hull() {
                                                    hull() {
                                                      hull() {
                                                        translate([65.2, 27.1818, 2.22]) {
                                                          zcyl(r = 0.12, l = 1.8);
                                                        }
                                                        translate([65.2, 29.5755, 2.22]) {
                                                          zcyl(r = 0.12, l = 1.8);
                                                        }
                                                      }
                                                      translate([79, 32.5136, 3.6]) {
                                                        zcyl(r = 0.12, l = 1.8);
                                                      }
                                                    }
                                                    translate([79, 33.8409, 3.6]) {
                                                      zcyl(r = 0.12, l = 1.8);
                                                    }
                                                  }
                                                }
                                                hull() {
                                                  hull() {
                                                    hull() {
                                                      translate([65.2, 30.8755, 2.22]) {
                                                        zcyl(r = 0.12, l = 1.8);
                                                      }
                                                      translate([65.2, 33.2691, 2.22]) {
                                                        zcyl(r = 0.12, l = 1.8);
                                                      }
                                                    }
                                                    translate([79, 35.1409, 3.6]) {
                                                      zcyl(r = 0.12, l = 1.8);
                                                    }
                                                  }
                                                  translate([79, 36.4682, 3.6]) {
                                                    zcyl(r = 0.12, l = 1.8);
                                                  }
                                                }
                                              }
                                              hull() {
                                                hull() {
                                                  hull() {
                                                    translate([65.2, 34.5691, 2.22]) {
                                                      zcyl(r = 0.12, l = 1.8);
                                                    }
                                                    translate([65.2, 36.9627, 2.22]) {
                                                      zcyl(r = 0.12, l = 1.8);
                                                    }
                                                  }
                                                  translate([79, 37.7682, 3.6]) {
                                                    zcyl(r = 0.12, l = 1.8);
                                                  }
                                                }
                                                translate([79, 39.0955, 3.6]) {
                                                  zcyl(r = 0.12, l = 1.8);
                                                }
                                              }
                                            }
                                            hull() {
                                              hull() {
                                                hull() {
                                                  translate([65.2, 38.2627, 2.22]) {
                                                    zcyl(r = 0.12, l = 1.8);
                                                  }
                                                  translate([65.2, 40.6564, 2.22]) {
                                                    zcyl(r = 0.12, l = 1.8);
                                                  }
                                                }
                                                translate([79, 40.3955, 3.6]) {
                                                  zcyl(r = 0.12, l = 1.8);
                                                }
                                              }
                                              translate([79, 41.7227, 3.6]) {
                                                zcyl(r = 0.12, l = 1.8);
                                              }
                                            }
                                          }
                                          hull() {
                                            hull() {
                                              hull() {
                                                translate([65.2, 41.9564, 2.22]) {
                                                  zcyl(r = 0.12, l = 1.8);
                                                }
                                                translate([65.2, 44.35, 2.22]) {
                                                  zcyl(r = 0.12, l = 1.8);
                                                }
                                              }
                                              translate([79, 43.0227, 3.6]) {
                                                zcyl(r = 0.12, l = 1.8);
                                              }
                                            }
                                            translate([79, 44.35, 3.6]) {
                                              zcyl(r = 0.12, l = 1.8);
                                            }
                                          }
                                        }
                                        hull() {
                                          hull() {
                                            hull() {
                                              translate([80.2, 20.3045, 3.72]) {
                                                zcyl(r = 0.12, l = 1.8);
                                              }
                                              translate([80.2, 21.5391, 3.72]) {
                                                zcyl(r = 0.12, l = 1.8);
                                              }
                                            }
                                            translate([92, 29.4227, 4.9]) {
                                              zcyl(r = 0.12, l = 1.8);
                                            }
                                          }
                                          translate([92, 29.7455, 4.9]) {
                                            zcyl(r = 0.12, l = 1.8);
                                          }
                                        }
                                      }
                                      hull() {
                                        hull() {
                                          hull() {
                                            translate([80.2, 22.8391, 3.72]) {
                                              zcyl(r = 0.12, l = 1.8);
                                            }
                                            translate([80.2, 24.0736, 3.72]) {
                                              zcyl(r = 0.12, l = 1.8);
                                            }
                                          }
                                          translate([92, 31.0455, 4.9]) {
                                            zcyl(r = 0.12, l = 1.8);
                                          }
                                        }
                                        translate([92, 31.3682, 4.9]) {
                                          zcyl(r = 0.12, l = 1.8);
                                        }
                                      }
                                    }
                                    hull() {
                                      hull() {
                                        hull() {
                                          translate([80.2, 25.3736, 3.72]) {
                                            zcyl(r = 0.12, l = 1.8);
                                          }
                                          translate([80.2, 26.6082, 3.72]) {
                                            zcyl(r = 0.12, l = 1.8);
                                          }
                                        }
                                        translate([92, 32.6682, 4.9]) {
                                          zcyl(r = 0.12, l = 1.8);
                                        }
                                      }
                                      translate([92, 32.9909, 4.9]) {
                                        zcyl(r = 0.12, l = 1.8);
                                      }
                                    }
                                  }
                                  hull() {
                                    hull() {
                                      hull() {
                                        translate([80.2, 27.9082, 3.72]) {
                                          zcyl(r = 0.12, l = 1.8);
                                        }
                                        translate([80.2, 29.1427, 3.72]) {
                                          zcyl(r = 0.12, l = 1.8);
                                        }
                                      }
                                      translate([92, 34.2909, 4.9]) {
                                        zcyl(r = 0.12, l = 1.8);
                                      }
                                    }
                                    translate([92, 34.6136, 4.9]) {
                                      zcyl(r = 0.12, l = 1.8);
                                    }
                                  }
                                }
                                hull() {
                                  hull() {
                                    hull() {
                                      translate([80.2, 30.4427, 3.72]) {
                                        zcyl(r = 0.12, l = 1.8);
                                      }
                                      translate([80.2, 31.6773, 3.72]) {
                                        zcyl(r = 0.12, l = 1.8);
                                      }
                                    }
                                    translate([92, 35.9136, 4.9]) {
                                      zcyl(r = 0.12, l = 1.8);
                                    }
                                  }
                                  translate([92, 36.2364, 4.9]) {
                                    zcyl(r = 0.12, l = 1.8);
                                  }
                                }
                              }
                              hull() {
                                hull() {
                                  hull() {
                                    translate([80.2, 32.9773, 3.72]) {
                                      zcyl(r = 0.12, l = 1.8);
                                    }
                                    translate([80.2, 34.2118, 3.72]) {
                                      zcyl(r = 0.12, l = 1.8);
                                    }
                                  }
                                  translate([92, 37.5364, 4.9]) {
                                    zcyl(r = 0.12, l = 1.8);
                                  }
                                }
                                translate([92, 37.8591, 4.9]) {
                                  zcyl(r = 0.12, l = 1.8);
                                }
                              }
                            }
                            hull() {
                              hull() {
                                hull() {
                                  translate([80.2, 35.5118, 3.72]) {
                                    zcyl(r = 0.12, l = 1.8);
                                  }
                                  translate([80.2, 36.7464, 3.72]) {
                                    zcyl(r = 0.12, l = 1.8);
                                  }
                                }
                                translate([92, 39.1591, 4.9]) {
                                  zcyl(r = 0.12, l = 1.8);
                                }
                              }
                              translate([92, 39.4818, 4.9]) {
                                zcyl(r = 0.12, l = 1.8);
                              }
                            }
                          }
                          hull() {
                            hull() {
                              hull() {
                                translate([80.2, 38.0464, 3.72]) {
                                  zcyl(r = 0.12, l = 1.8);
                                }
                                translate([80.2, 39.2809, 3.72]) {
                                  zcyl(r = 0.12, l = 1.8);
                                }
                              }
                              translate([92, 40.7818, 4.9]) {
                                zcyl(r = 0.12, l = 1.8);
                              }
                            }
                            translate([92, 41.1045, 4.9]) {
                              zcyl(r = 0.12, l = 1.8);
                            }
                          }
                        }
                        hull() {
                          hull() {
                            hull() {
                              translate([80.2, 40.5809, 3.72]) {
                                zcyl(r = 0.12, l = 1.8);
                              }
                              translate([80.2, 41.8155, 3.72]) {
                                zcyl(r = 0.12, l = 1.8);
                              }
                            }
                            translate([92, 42.4045, 4.9]) {
                              zcyl(r = 0.12, l = 1.8);
                            }
                          }
                          translate([92, 42.7273, 4.9]) {
                            zcyl(r = 0.12, l = 1.8);
                          }
                        }
                      }
                      hull() {
                        hull() {
                          hull() {
                            translate([80.2, 43.1155, 3.72]) {
                              zcyl(r = 0.12, l = 1.8);
                            }
                            translate([80.2, 44.35, 3.72]) {
                              zcyl(r = 0.12, l = 1.8);
                            }
                          }
                          translate([92, 44.0273, 4.9]) {
                            zcyl(r = 0.12, l = 1.8);
                          }
                        }
                        translate([92, 44.35, 4.9]) {
                          zcyl(r = 0.12, l = 1.8);
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          translate([0, 114, 0]) {
            rotate([6, 0, 0]) {
              translate([0, -114, 0]) {
                translate([0, 0, -0.8]) {
                  linear_extrude(height = 1.6) {
                    polygon(points = [[0, 75], [48, 127], [0, 127], [-48, 127]]);
                  }
                }
              }
            }
          }
        }
        hull() {
          hull() {
            translate([0, 92, 0]) {
              translate([0, 0, 2]) {
                xcyl(r = 1.25, l = 2.5);
              }
            }
            translate([0, 127, 0]) {
              translate([0, 0, 2]) {
                xcyl(r = 1.25, l = 2.5);
              }
            }
          }
          translate([0, 127, 0]) {
            translate([0, 0, 38]) {
              xcyl(r = 1.25, l = 2.5);
            }
          }
        }
      }
      hull() {
        translate([0, 39, -4]) {
          cuboid([8, 21, 2]);
        }
        translate([0, 39, -10]) {
          cuboid([5, 15, 2]);
        }
      }
    }
  }
}
$fn = 50;