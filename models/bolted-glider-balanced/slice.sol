# Slice previously prepared beds and publish a hash receipt for each completed job.
G = use "provenance".
C = use "sol/lib/cli".
tool = "/Applications/BambuStudio.app/Contents/MacOS/BambuStudio".
defaults = ["a1-wing-right","p1s-wing-left","x1c-fuselage","a1-tail"].
sliceOne root slicer job =
  _ = if or (not (G.safeKey job)) (Str.contains "/" job) then error "invalid job directory: {job}" else Unit;
  folder = "{root}/{job}";
  receipts = List.map (G.row root) ["meshes.json","{job}/prepared-inputs.json"];
  geometry = G.entries "{root}/meshes.json";
  sources = G.sourceFiles root;
  _ = G.requireKeys (["design.py","design.json"] + G.geometryFiles sources) geometry;
  _ = G.require root geometry;
  prepared = G.entries "{folder}/prepared-inputs.json";
  extra = case job of "p1s-tail" -> ["prepare_tail_p1s.py"] | "p1s-wing-right" -> ["prepare_right_p1s.py"] | _ -> [];
  _ = G.requireKeys (extra + ["meshes.json","prepare_slice.py","{job}/glider-project.3mf","{job}/layout.json"] +
      List.map (fn name -> "{job}/presets/{name}.json") ["machine","process","filament","project"]) prepared;
  _ = G.require root prepared;
  drivers = List.map (G.row root) ["slice.sol","provenance.sol"];
  recheck = fn u ->
    _ = G.require root (receipts + geometry + prepared + drivers);
    if sources == G.sourceFiles root then Unit else error "source list changed during slice";
  G.sliceProject root job slicer prepared drivers recheck.
> argv = args Unit;
  if List.has "--help" argv or List.has "-h" argv then print "usage: slice.sol [--slicer PATH] [JOB ...]" else
    options = unwrap (C.parse [C.Option "slicer" "" C.One] argv);
    jobs = case C.operands options of [] -> defaults | selected -> selected;
    _ = List.map (sliceOne (G.modelRoot Unit) (C.valueOr tool "slicer" options)) jobs;
    Unit.
