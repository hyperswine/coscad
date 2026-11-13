# Slice the actual Orbit Tower plate with its historical model-relative receipts.
G = use "../bolted-glider-balanced/provenance".
C = use "sol/lib/cli".
tool = "/Applications/BambuStudio.app/Contents/MacOS/BambuStudio".
shared = "../bolted-glider/prepare_slice.py".
driver = "../bolted-glider-balanced/provenance.sol".
parts = ["axle","base","cap","rotor","satellite"].
geometryKeys = ["orbit-tower.assemble","prepare_print.py",shared] + List.map (fn p -> "{p}.coscad") parts +
  List.map (fn p -> suffix = if List.has p ["axle","rotor","satellite"] then "1" else ""; "orbit-tower_next_{p}{suffix}_bot.stl") parts.
checkInputs root rows = G.requireWith [shared,driver] root rows.
sliceTower root slicer job =
  _ = if job == "x1c-tower" then Unit else error "unknown Orbit Tower bed: {job}";
  geometry = G.entries "{root}/meshes.json";
  prepared = G.entries "{root}/{job}/prepared-inputs.json";
  _ = G.requireKeys geometryKeys geometry;
  _ = G.requireKeys (geometryKeys + ["{job}/glider-project.3mf"] +
    List.map (fn preset -> "{job}/presets/{preset}.json") ["machine","process","filament","project"]) prepared;
  controls = List.map (G.row root) ["meshes.json","{job}/prepared-inputs.json","{job}/layout.json"];
  scripts = List.map (G.row root) ["slice.sol",driver];
  inputs = geometry + prepared + controls + scripts;
  _ = checkInputs root inputs;
  recheck = fn u -> checkInputs root inputs;
  G.sliceProject root job slicer (prepared + controls) scripts recheck.
> argv = args Unit;
  usage = "usage: slice.sol [--slicer PATH] [x1c-tower]";
  if List.has "--help" argv or List.has "-h" argv then print usage else
    opts = unwrap (C.parse [C.Option "slicer" "" C.One] argv);
    dir = if exists "models/orbit-tower-v2/slice.sol" then "models/orbit-tower-v2" else ".";
    root = G.line (G.output ["realpath",dir]);
    jobs = case C.operands opts of [] -> ["x1c-tower"] | xs -> xs;
    _ = List.map (sliceTower root (C.valueOr tool "slicer" opts)) jobs;
    Unit.
