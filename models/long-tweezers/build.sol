# Slice the long tweezers for one printer (2026-11-28).
#
# Renders print.coscad (the tweezers turned -45 degrees with mouse-ear pads), centres it on the bed
# and slices it through models/lib/bambu.sol. The process makes the arms solid walls along their
# length, so the printed part matches the FEA's solid-PLA model: 8 walls, 100% infill laid along the
# arms, 4 top and bottom layers.
#
#   sol design.sol && sol build.sol --printer x1c      (or a1)
J = use "sol/lib/json".
Bambu = use "../lib/bambu".

coscadBin root = case unwrap (Env.get "COSCAD") of
    EnvText path -> path
  | EnvMissing -> "{root}/dist-newstyle/build/aarch64-osx/ghc-9.8.2/coscad-1.6.0.0/x/coscad/build/coscad/coscad".
# "... bounds [x0 y0 z0] .. [x1 y1 z1]" from coscad stl.
bounds line = i = Str.indexOf "bounds [" line; t = Str.slice line (i + 8) (Str.len line);
  ws = Str.words (Str.replace "]" " " (Str.replace "[" " " (Str.replace ".." " " t)));
  List.map (fn w -> unwrap (Try.parseNum w)) (List.take 6 ws).
tagsFor dir printer = raw = Str.replace "PRINTER" printer (Bambu.readNow "{dir}/parts.json");
  J.set "dir" (J.JStr dir) (unwrap (J.parse raw)).

> argv = args Unit;
  printer = Bambu.argAfter "--printer" argv;
  u0 = if List.has printer ["x1c", "a1", "p1s"] then Unit else error "usage: sol build.sol --printer x1c|a1|p1s";
  root = Bambu.repoRoot Unit;
  dir = "{root}/models/long-tweezers";
  presets = "{dir}/presets/{printer}";
  ProcessResult code stdout err = Bambu.run [coscadBin root, "stl", "{dir}/print.coscad", "-o", "{dir}/print.stl"];
  u1 = if code == 0 then Bambu.run ["rm", "-f", "{dir}/print.scad"] else error "coscad: {err}{stdout}";
  b = bounds stdout;
  cx = 128.0 - ((b ! 0) + (b ! 3)) / 2.0;
  cy = 128.0 - ((b ! 1) + (b ! 4)) / 2.0;
  u2 = print "footprint {(b ! 3) - (b ! 0)} x {(b ! 4) - (b ! 1)} mm, placed at {cx}, {cy}";
  job = "{dir}/{printer}";
  u3 = Bambu.run ["mkdir", "-p", job];
  base = unwrap (J.parse (Bambu.readNow "{presets}/process.json"));
  overrides = [("wall_loops", "8"), ("sparse_infill_density", "100%"), ("sparse_infill_pattern", "zig-zag"),
    ("infill_direction", "135"), ("solid_infill_direction", "135"), ("top_shell_layers", "4"), ("bottom_shell_layers", "4"),
    ("enable_support", "0"), ("brim_type", "no_brim"), ("layer_height", "0.2")];
  process = List.fold (fn acc kv -> (k, v) = kv; J.set k (J.JStr v) acc) base overrides;
  u4 = Bambu.putNow "{job}/process.json" (J.pretty process + Bambu.nl);
  result = Bambu.slice {dir = job, tags = tagsFor dir printer, stls = [("main", "{dir}/print.stl")], machine = "{presets}/machine.json",
    process = "{job}/process.json", center = (cx, cy), filamentMap = "", name = "long-tweezers-{printer}"};
  print (J.pretty result).
