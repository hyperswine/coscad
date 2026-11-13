# Rebuild the A1 globe using the same renderer and slicer as the threaded revision.
C = use "sol/lib/cli".
P = use "sol/lib/proc".
W = use "workflow".
parts = [[("stand","stand")],[("rotor","rotor")],[("bearing-blocker","bearing_blocker")]].
views = [("globe","stand ⊕ rotor"),("stand","stand"),("rotor","rotor"),("bearing-blocker","bearing_blocker"),
         ("land","ζ center_z land"),("ocean","ζ center_z (ocean ⊖ bore)")].
build : unsafe String -> C.Parsed -> Unit.
build dir opts =
  python = C.valueOr (W.env "COSCAD_GLOBE_PYTHON" "python3") "python" opts;
  mpl = W.env "MPLCONFIGDIR" "/private/tmp/globe-matplotlib";
  script name argv = W.command (P.withEnv "MPLCONFIGDIR" mpl (P.spec ([python,"{dir}/{name}.py"] + argv)));
  selected = C.valueOr (W.env "COSCAD" "") "coscad" opts;
  compiler = if selected != "" then selected else W.query (P.inDir "{dir}/../.." (P.spec ["cabal","list-bin","exe:coscad"]));
  _ = W.command (P.spec [python,"{dir}/generate.py","--source-only"]);
  source = W.prefixOf "{dir}/globe.coscad";
  tmp = tempDir "coscad-globe-parts";
  _ = List.map (W.exportView dir tmp compiler source "") views;
  meshes = tempDir "coscad-globe-meshes";
  _ = W.renderParts dir meshes compiler source parts;
  _ = script "verify" ["--assemble"];
  if C.flag "render-only" opts then Unit else
    _ = script "prepare_slice" [];
    slicer = C.valueOr "/Applications/BambuStudio.app/Contents/MacOS/BambuStudio" "slicer" opts;
    _ = W.sliceProject dir slicer ("slice","process.json","globe-project.3mf","globe-a1.gcode.3mf");
    _ = script "verify_toolpaths" [];
    script "finalize_package" [].
> usage = "usage: build.sol [--coscad PATH] [--render-only] [--python PATH] [--slicer PATH]";
  argv = args Unit;
  if List.has "--help" argv or List.has "-h" argv then print usage else
    opts = unwrap (C.parse [C.Option "coscad" "" C.One,C.Option "render-only" "" C.Switch,
                            C.Option "python" "" C.One,C.Option "slicer" "" C.One] argv);
    rel = "models/print-in-place-globe";
    dir = W.env "COSCAD_GLOBE_DIR" (if exists "{rel}/workflow.sol" then rel else ".");
    if C.operands opts != [] or not (exists "{dir}/workflow.sol") then error usage
    else build (W.query (P.spec ["realpath",dir])) opts.
