# Two-colour X2D package for the AGVN sailboat, sliced from the colour tags in ../parts.json
# (2026-11-28; Sol port of build.py).
#
# Every part named in parts.json is compiled from ../boat.coscad (main = part) to an STL, and the
# Bambu Studio assemble list gives each part its tagged filament at the same plate position, so the
# letters stay registered in the hull. The filament-to-nozzle map comes from the X2D's live report
# (which nozzle holds each tagged spool, read-only), or from --filament-map. The process is the
# earlier one made as light as it will print: lightning infill, 2 walls, supports only when asked
# (tree, from the bed).
#
#   sol build.sol [--tree] [--no-tower] [--cooldown[=MIN]] [--infill=5%] [--filament-map "1 2"] [--name=DIR]
#
#   --tree           bed-only tree supports
#   --no-tower       no prime tower (each nozzle switch still re-primes at the purge chute)
#   --cooldown[=10]  after the print, both auxiliary fans and the exhaust fan at full speed for MIN minutes
# The output folder is named from the options, e.g. tree-two-nozzles-notower-cool10/.
J = use "sol/lib/json".
Bambu = use "../../../../lib/bambu".

option prefix argv = List.fold (fn acc a -> if Str.startsWith prefix a then Str.slice a (Str.len prefix) (Str.len a) else acc) "" argv.
coscadBin root = case unwrap (Env.get "COSCAD") of
    EnvText path -> path
  | EnvMissing -> "{root}/dist-newstyle/build/aarch64-osx/ghc-9.8.2/coscad-1.6.0.0/x/coscad/build/coscad/coscad".
withMain source part = Str.join Bambu.nl (List.map (fn l -> if Str.startsWith "main = " l then "main = {part}" else l) (Str.lines source)).

compile root here sail tags part =
  src = "{here}/{part}.coscad";
  stl = "{here}/{part}.stl";
  u = Bambu.putNow src (withMain (Bambu.readNow "{sail}/{Bambu.text "model" tags}") part + Bambu.nl);
  ProcessResult code stdout err = Bambu.run [coscadBin root, "stl", src, "-o", stl];
  if code != 0 then error "coscad failed on {part}: {err}{stdout}" else
  v = Bambu.run ["rm", "-f", "{here}/{part}.scad"];
  u2 = print (Str.trim stdout);
  (part, stl).

# Which nozzle holds each tagged spool right now (read-only status).
liveMap p tags =
  code = Bambu.accessCode p;
  u = Bambu.identity p code;
  pl = Bambu.plan p (Bambu.status p code) (Bambu.filamentsOf tags);
  u2 = print "spools now: {J.render (Bambu.planJson p pl)}";
  pl.filamentMap.

> argv = args Unit;
  root = Bambu.repoRoot Unit;
  sail = "{root}/models/twin-boat-trials/agvn/sail";
  here = "{sail}/x2d-light";
  agvn = "{root}/models/twin-boat-trials/agvn";
  tree = List.has "--tree" argv;
  notower = List.has "--no-tower" argv;
  coolArg = List.fold (fn acc a -> if a == "--cooldown" then "10" else if Str.startsWith "--cooldown=" a then Str.slice a 11 (Str.len a) else acc) "" argv;
  cool = if coolArg == "" then 0 else unwrap (Try.parseInt coolArg);
  infillArg = option "--infill=" argv;
  infill = if infillArg == "" then "5%" else infillArg;
  tags = Bambu.readTags sail;
  p = Bambu.printer "x2d";
  given = Bambu.argAfter "--filament-map" argv;
  fmap = if given != "" then given else liveMap p tags;
  two = fmap != "1 1";
  named = option "--name=" argv;
  name = if named != "" then named else (if tree then "tree" else "nosupport") + (if two then "-two-nozzles" else "") + (if notower then "-notower" else "") + (if cool > 0 then "-cool{cool}" else "");
  dir = "{here}/{name}";
  u3 = Bambu.run ["mkdir", "-p", dir];
  stls = List.map (fn q -> compile root here sail tags q.name) (Bambu.partsOf tags);
  base = unwrap (J.parse (Bambu.readNow "{sail}/tree/process.json"));
  overrides = [("sparse_infill_pattern", "lightning"), ("sparse_infill_density", infill), ("wall_loops", "2"),
    ("top_shell_layers", "4"), ("bottom_shell_layers", "3"), ("enable_support", if tree then "1" else "0"),
    ("support_type", "tree(auto)"), ("support_on_build_plate_only", "1"), ("enable_prime_tower", if notower then "0" else "1"),
    ("support_filament", str (Bambu.filamentIndex tags (Bambu.text "supports" tags))),
    ("support_interface_filament", str (Bambu.filamentIndex tags (Bambu.text "supports" tags)))];
  process = List.fold (fn acc kv -> (k, v) = kv; J.set k (J.JStr v) acc) base overrides;
  u4 = Bambu.putNow "{dir}/process.json" (J.pretty process + Bambu.nl);
  machine = Bambu.withCooldown p (unwrap (J.parse (Bambu.readNow "{agvn}/machine.json"))) cool;
  u5 = Bambu.putNow "{dir}/machine.json" (J.pretty machine + Bambu.nl);
  centre = Bambu.items "center" tags;
  job = {dir = dir, tags = tags, stls = stls, machine = "{dir}/machine.json", process = "{dir}/process.json",
         center = (unwrap (J.num (centre ! 0)), unwrap (J.num (centre ! 1))), filamentMap = fmap, name = "AGVN-sail-light"};
  result = Bambu.slice job;
  print (J.pretty (J.set "job" (J.JStr name) (J.set "infill" (J.JStr "lightning {infill}") result))).
