# Run the design, meshing, provenance and verification stages on a scratch build.
G = use "provenance".
C = use "sol/lib/cli".
buildGlider root python compiler incidence =
  # verify.py reads meshes.json, so it must be visible before verification.
  stage = tempDir "coscad-glider-build";
  originalSources = G.sourceFiles root;
  files = ["design.py","assembly.py","provenance.py","verify.py"] + originalSources;
  _ = List.map (fn name -> G.copyFile "{root}/{name}" "{stage}/{name}") files;
  initial = List.map (G.row stage) files;
  _ = G.checked [python,"{stage}/design.py","--tail-incidence",incidence];
  sources = G.sourceFiles stage;
  _ = List.map (fn source -> G.checked [compiler,"stl","{stage}/{source}","-o","{stage}/{G.meshName source}"]) sources;
  geometry = G.geometryFiles sources;
  rows = List.map (G.row stage) (["design.py","design.json"] + geometry);
  _ = G.publish "{stage}/meshes.json" rows;
  _ = G.checked [python,"{stage}/verify.py"];
  _ = G.require stage rows;
  _ = G.require root initial;
  _ = if originalSources == G.sourceFiles root then Unit else error "source list changed during build";
  exports = ls stage |> List.filter (Str.endsWith ".scad");
  _ = List.map (fn name -> G.copyFile "{stage}/{name}" "{root}/{name}")
      (geometry + exports + ["design.json","preview.png","validation.json","meshes.json"]);
  print "Verified {List.len sources} meshes".
> argv = args Unit;
  usage = "usage: build.sol [--coscad PATH] [--python PATH] [--tail-incidence DEGREES]";
  if List.has "--help" argv or List.has "-h" argv then print usage else
    options = unwrap (C.parse [C.Option "coscad" "" C.One,C.Option "python" "" C.One,C.Option "tail-incidence" "" C.One] argv);
    root = case unwrap (Env.get "COSCAD_GLIDER_DIR") of EnvText path -> path | EnvMissing -> G.modelRoot Unit;
    if C.operands options != [] then error usage else
      buildGlider root (C.valueOr "python3" "python" options) (C.valueOr "coscad" "coscad" options) (C.valueOr "-5.5" "tail-incidence" options).
